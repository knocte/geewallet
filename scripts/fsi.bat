@ECHO OFF

SET ENTERPRISE="%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Enterprise\Common7\IDE\CommonExtensions\Microsoft\FSharp\Toolz\fsi.exe"
SET ENTERPRISE_OLD="%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Enterprise\Common7\IDE\CommonExtensions\Microsoft\FSharp\fsi.exe"
SET COMMUNITY="%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Community\Common7\IDE\CommonExtensions\Microsoft\FSharp\Toolz\fsi.exe"
SET COMMUNITY_OLD="%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Community\Common7\IDE\CommonExtensions\Microsoft\FSharp\fsi.exe"
SET BUILDTOOLS="%ProgramFiles(x86)%\Microsoft Visual Studio\2019\BuildTools\Common7\IDE\CommonExtensions\Microsoft\FSharp\Toolz\fsi.exe"
SET BUILDTOOLS_OLD="%ProgramFiles(x86)%\Microsoft Visual Studio\2019\BuildTools\Common7\IDE\CommonExtensions\Microsoft\FSharp\fsi.exe"

IF EXIST %ENTERPRISE% (
    SET RUNNER=%ENTERPRISE%
) ELSE (
    IF EXIST %COMMUNITY% (
        SET RUNNER=%COMMUNITY%
    ) ELSE (
        IF EXIST %BUILDTOOLS% (
            SET RUNNER=%BUILDTOOLS%
        ) ELSE (
            IF EXIST %ENTERPRISE_OLD% (
                SET RUNNER=%ENTERPRISE_OLD%
            ) ELSE (
                IF EXIST %COMMUNITY_OLD% (
                    SET RUNNER=%COMMUNITY%
                ) ELSE (
                    IF EXIST %BUILDTOOLS_OLD% (
                        SET RUNNER=%BUILDTOOLS_OLD%
                    ) ELSE (
                        ECHO fsi.exe not found, is F# installed?
                        CD %ProgramFiles(x86)%
                        DIR /S fsi.exe
                        ECHO Or maybe it changed location, see above if you find it.
                        EXIT /b 1
                    )
                )
            )
        )
    )
)

%RUNNER% %*
