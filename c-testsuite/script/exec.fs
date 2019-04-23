
module build
    open System
    open System.Diagnostics

    let to_utf8 (s : string) =
        if s = null then
            null
        else
            let nlen = System.Text.Encoding.UTF8.GetByteCount(s)

            let ba = Array.zeroCreate nlen
            nlen = System.Text.Encoding.UTF8.GetBytes(s, 0, s.Length, ba, 0)

            ba

    let exec filename args startDir = 
        let wd = System.IO.Path.GetFullPath(startDir)
        let timer = Stopwatch.StartNew()
        let procStartInfo = 
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = filename,
                Arguments = args,
                WorkingDirectory = wd
            )

        let ProcessOutput (e : DataReceivedEventArgs) = System.Console.WriteLine(e.Data)
        let ProcessError (e : DataReceivedEventArgs) = System.Console.Error.WriteLine(e.Data)

        let p = new Process(StartInfo = procStartInfo)
        p.OutputDataReceived.Add(ProcessOutput)
        p.ErrorDataReceived.Add(ProcessError)

        let desc = sprintf "%s %s in %s" filename args wd
        printfn "-------- %s" desc
        p.Start()
        //printfn "Started %s with pid %i" p.ProcessName p.Id
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()
        timer.Stop()
        printfn "Finished %s after %A milliseconds" desc timer.ElapsedMilliseconds
        let rc = p.ExitCode
        if rc <> 0 then raise(Exception())
        ()

    // TODO this needs a timeout
    let exec_capture filename args startDir = 
        use ms = new System.IO.MemoryStream()
        let wd = System.IO.Path.GetFullPath(startDir)
        let timer = Stopwatch.StartNew()
        let procStartInfo = 
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = filename,
                Arguments = args,
                WorkingDirectory = wd
            )

        let ProcessOutput (e : DataReceivedEventArgs) = 
            if e.Data <> null then
                let ba = to_utf8 (e.Data + "\n")
                ms.Write(ba, 0, ba.Length)
        let ProcessError (e : DataReceivedEventArgs) = 
            if e.Data <> null then
                let ba = to_utf8 (e.Data + "\n")
                ms.Write(ba, 0, ba.Length)

        let p = new Process(StartInfo = procStartInfo)
        p.OutputDataReceived.Add(ProcessOutput)
        p.ErrorDataReceived.Add(ProcessError)

        let desc = sprintf "%s %s in %s" filename args wd
        //printfn "-------- %s" desc
        p.Start()
        //printfn "Started %s with pid %i" p.ProcessName p.Id
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()
        timer.Stop()
        //printfn "Finished %s after %A milliseconds" desc timer.ElapsedMilliseconds
        let rc = p.ExitCode
        (rc, ms.ToArray())


