*          DATA SET SPREPAS02  AT LEVEL 017 AS OF 02/01/21                      
*PHASE SPAS02A                                                                  
***********************************************************************         
* QOPT1 - C'Y' Print OUTBLK record                                    *         
*       - C'P' Print work records - do not create file                *         
*       - C'S' Create =NWK file with DSN only using SHIP= in JCL      *         
* QOPT2 - C'G'=Gross,C'N'=Net                                         *         
* QOPT3 - C'A' Report ordered assigned for Network                    *         
* QOPT4 - System filter (S, N or P)                                   *         
* QOPT5 - C'B' Report ordered $ by insertion month and billable month *         
*              (Print)                                                *         
* QOPT5+1 C'C' INCLUDE CLIENT TOTALS IN REPORT                                  
***********************************************************************         
                                                                                
SPAS02   TITLE '- Accent File Extract'                                          
SPAS02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPAS02                                                       
         L     RA,0(,R1)                                                        
         LR    R9,RA                                                            
         AHI   R9,4*K                                                           
         LARL  RC,GLOBALS                                                       
                                                                                
         USING SPWORKD,RA,R9                                                    
         USING GLOBALS,RC                                                       
         USING FILEHDRD,FILEHDR                                                 
         USING DETLRECD,FILEREC                                                 
         USING WLHDRD,WRKRECL                                                   
                                                                                
         CLI   MODE,REQFRST                                                     
         JNE   EXIT                                                             
*                                                                               
         SR    RF,RF                                                            
         LA    R1,REPTAB                                                        
         CLI   QOPT5,C'B'                                                       
         BNE   REQF5                                                            
         LA    R1,REPTABI                                                       
*                                                                               
REQF5    CLI   0(R1),X'FF'      END OF TABLE                                    
         BE    REQF10                                                           
         ICM   RF,7,0(R1)                                                       
         MVC   37(1,RF),3(R1)   RESET TO COLUMN IN REP20-REP26 ENTRIES          
*                                                                               
*        NOTE: DISPLACEMENT MAY CHANGE IF MACRO VALUES ARE ALTERED              
*              FOR NOW COLUMN VALUE DISPLACEMENT IS 37                          
*                                                                               
         LA    R1,4(R1)                                                         
         B     REQF5                                                            
*                                                                               
REPTAB   DC    AL3(REP20),C'1'    COLUMN VALUES WHEN INS. MTH HIDDEN            
         DC    AL3(REP21),C'2'                                                  
         DC    AL3(REP22),C'3'                                                  
         DC    AL3(REP23),C'4'                                                  
         DC    AL3(REP24),C'5'                                                  
         DC    AL3(REP25),C'5'                                                  
         DC    AL3(REP26),C'6'                                                  
         DC    X'FF'               END OF TABLE                                 
*                                                                               
REPTABI  DC    AL3(REP20),C'2'    COLUMN VALUES WHEN INS. MTH PRESENT           
         DC    AL3(REP21),C'3'                                                  
         DC    AL3(REP22),C'4'                                                  
         DC    AL3(REP23),C'5'                                                  
         DC    AL3(REP24),C'6'                                                  
         DC    AL3(REP25),C'6'                                                  
         DC    AL3(REP26),C'7'                                                  
         DC    X'FF'               END OF TABLE                                 
*                                                                               
REQF10   DS    0H                                                               
         USING MASTD,RF                                                         
         L     RF,VMASTC                                                        
         L     RE,MCAEXTRA                                                      
         MVC   OUTAGYC,MCAGYCOD-MCEXTRA(RE)                                     
         MVC   SVAGY,MCUSER                                                     
         MVC   WRITESW,MCWRITE                                                  
         MVC   VUTL,MCUTL                                                       
         MVC   VSSB,MCSSB                                                       
         CLC   MCFFPARM(4),SPACES                                               
         BNH   REQF12                                                           
         MVC   PRIMARY,MCFFPARM                                                 
         PACK  DUB,PRIMARY                                                      
         CVB   R1,DUB                                                           
         STCM  R1,7,OUTPRI                                                      
*                                                                               
REQF12   CLC   MCFFPARM+4(4),SPACES                                             
         BNH   REQF14                                                           
         MVC   SECONDRY,MCFFPARM+4                                              
         PACK  DUB,SECONDRY                                                     
         CVB   R1,DUB                                                           
         STCM  R1,7,OUTSEC                                                      
*                                                                               
         USING SSBOFFD,R2                                                       
REQF14   CLI   MCRECOVR,YES                                                     
         BNE   SPAS00                                                           
         ICM   R2,15,VSSB                                                       
         JZ    *+2                                                              
                                                                                
         CLI   SSOXTND,X'FF'                                                    
         JNE   *+2                                                              
                                                                                
*                                                                               
         MVC   DSPACE,SSODSPAC                                                  
         CLI   DSPACE,C'T'                                                      
         BNE   *+10                                                             
         MVC   OUTDSN(7),=C'TSTTAPE'                                            
         CLI   DSPACE,C'Q'                                                      
         BNE   *+10                                                             
         MVC   OUTDSN(7),=C'FQATAPE'                                            
         CLI   DSPACE,C'C'                                                      
         BNE   *+10                                                             
         MVC   OUTDSN(7),=C'CSCTAPE'                                            
*                                                                               
         NI    SSOSTAT2,X'FF'-SSOSNRCV                                          
         OI    SSOSTAT2,SSOSROLC                                                
*                                                                               
SPAS00   CLI   QOPT1,C'S'          Test if creating worker file only            
         JNE   SPAS04                                                           
         L     RE,MCVLOGOC                                                      
         MVC   OUTDSN,LOGOINFO-LOGOD(RE)                                        
         J     SPAS06                                                           
         DROP  RF                                                               
                                                                                
SPAS04   GOTOR INIRUN              Initialize run values                        
                                                                                
         GOTOR PRCSYS,SYSSPTQ      Process Spot system data                     
         GOTOR PRCSYS,SYSNETQ      Process Net system data                      
         GOTOR PRCSYS,SYSPRTQ      Process Print system data                    
                                                                                
         GOTOR PUTFIL,1            Write last output block & close file         
                                                                                
SPAS06   DS    0H                                                               
         GOTOR ADDWRK              Create worker file                           
         CLI   ARCHIVE,YES                                                      
         BNE   SPAS10                                                           
*        GOTOR ADDPTR                                                           
                                                                                
SPAS10   MVC   P(11),=C'Output DSN='                                            
         MVC   P+11(L'OUTDSN),OUTDSN                                            
         GOTOR REPORT                                                           
                                                                                
         GOTOR AENDREQ             Force end of run                             
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Initialize areas and tables                                         *         
***********************************************************************         
                                                                                
INIRUN   NTR1                                                                   
         L     RE,ADCONLST                                                      
         MVC   OFFICER,VOFFICER-SPADCONS(RE)                                    
                                                                                
         LARL  R1,OUTBLK           Initialize output block                      
         LHI   R0,4                                                             
         STH   R0,0(R1)                                                         
                                                                                
         TIME  DEC                 R0 = HHMMSSTH                                
         SRL   R0,4                R0 = 0HHMMSST                                
         ST    R0,FULL                                                          
         SRL   R0,4                R0 = 00HHMMSS                                
         STCM  R0,7,REALTIME                                                    
         SRL   R0,16               R0 = 000000HH                                
         OI    FULL+L'FULL-1,X'0F'                                              
         UNPK  OUTTIME,FULL                                                     
                                                                                
         GOTOR DATCON,DMCB,(5,0),(0,DUB)                                        
         CLC   TODAY,DUB                                                        
         BNE   INIRUN01            JCL has must have DATE=                      
         CHI   R0,6                12pm, Over-run?                              
         BH    INIRUN01            Maybe not                                    
         GOTOR ADDAY,DMCB,(C'D',TODAY),WORK,-1                                  
         MVC   TODAY,WORK          Adjust by a day                              
                                                                                
INIRUN01 GOTOR DATCON,DMCB,(0,TODAY),(X'20',OUTDATE)                            
                                                                                
         LA    RE,FIXAMTS                                                       
         SR    RF,RF                                                            
INIRUN02 CLI   0(RE),EOT           End of table                                 
         JE    INIRUN03                                                         
         ICM   RF,7,1(RE)                                                       
         USING AMTTABD,RF                                                       
         OI    AMTIND1,AMTIHIDE    Set to hide column                           
         CLC   QOPT3,0(RE)         Match on option (assigned)                   
         JNE   *+8                                                              
         NI    AMTIND1,TURNOFF-AMTIHIDE                                         
         AHI   RE,L'FIXAMTS                                                     
         J     INIRUN02                                                         
         DROP  RF                                                               
*                                                                               
INIRUN03 LA    RE,FIXAMTS5                                                      
         SR    RF,RF                                                            
INIRUN04 CLI   0(RE),EOT           End of table                                 
         JE    INIRUN05                                                         
         ICM   RF,7,1(RE)                                                       
         USING AMTTABD,RF                                                       
         OI    AMTIND1,AMTIHIDE    Set to hide column                           
         CLC   QOPT5,0(RE)         Match on option (ins. mth)                   
         JNE   *+8                                                              
         NI    AMTIND1,TURNOFF-AMTIHIDE                                         
         AHI   RE,L'FIXAMTS5                                                    
         J     INIRUN04                                                         
         DROP  RF                                                               
                                                                                
***********************************************************************         
* Initialize schema based on options, Net vs. Gross and Assigned      *         
***********************************************************************         
                                                                                
INIRUN05 LA    RE,FIXSCHMA         Change schema based on option 3              
INIRUN06 CLI   0(RE),EOT           End of table                                 
         JE    INIRUN6P                                                         
         ICM   RF,7,1(RE)          Get table entry                              
         MVI   2(RF),TURNOFF       Mark as inactive                             
         CLC   QOPT3,0(RE)         Support Assigned amounts?                    
         JNE   *+8                 No, so schema okay                           
         MVI   2(RF),0             Restore this entry                           
         AHI   RE,L'FIXSCHMA                                                    
         J     INIRUN06                                                         
                                                                                
INIRUN6P LA    RE,FIXSCHMB         Change schema based on qopt5                 
INIRUN6R CLI   0(RE),EOT           End of table                                 
         JE    INIRUN08                                                         
         ICM   RF,7,1(RE)          Get table entry                              
         MVI   2(RF),TURNOFF       Mark as inactive                             
         CLC   QOPT5,0(RE)         Support ins. mth ordered?                    
         JNE   *+8                 No, so schema okay                           
         MVI   2(RF),0             Restore this entry                           
         AHI   RE,L'FIXSCHMB                                                    
         J     INIRUN6R                                                         
*                                                                               
INIRUN08 LARL  R7,SCHTABLE                                                      
         USING SCHTABLE,R7                                                      
*                                                                               
         LA    R1,CFMTTAB          SCHEMA FORMAT TABLE                          
         MVC   WORK(1),QOPT3                                                    
         MVC   WORK+1(1),QOPT5                                                  
INIRUN8B CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                INVALID OPTIONS ENCOUNTERED                  
         CLC   WORK(2),0(R1)                                                    
         BE    INIRUN8X                                                         
         LA    R1,3(R1)                                                         
         B     INIRUN8B                                                         
INIRUN8X MVC   FE12CFMT,2(R1)                                                   
         MVC   FE00CFMT,2(R1)                                                   
**NO-OP  MVC   FE12CFMT,QOPT3                                                   
**NO-OP  MVC   FE00CFMT,QOPT3                                                   
         MVC   FE00DESC(5),=C'Net $'                                            
         LA    R2,FE00DESC+6                                                    
         MVI   OUT$TYP,C'N'                                                     
         CLI   QOPT2,FILE$NET      Test net amounts                             
         JE    INIRUN10                                                         
         MVC   FE00DESC(7),=C'Gross $'                                          
         LA    R2,FE00DESC+8                                                    
         MVI   OUT$TYP,C'G'                                                     
                                                                                
INIRUN10 CLI   QOPT3,C'A'          Test including assigned                      
         JNE   INIRUN11                                                         
         MVC   0(10,R2),=C'w/Assigned'                                          
         AHI   R2,11                                                            
INIRUN11 DS    0H                                                               
         CLI   QOPT5,C'B'          TEST INCLUDING INS MTH DATA                  
         JNE   INIRUN1B                                                         
         MVC   0(09,R2),=C'w/Ins Mon'                                           
         AHI   R2,10                                                            
INIRUN1B DS    0H                                                               
         MVC   0(6,R2),=C'Run On'                                               
         GOTOR DATCON,DMCB,(X'40',TODAY),(8,7(R2))                              
         DROP  R7                                                               
                                                                                
         LARL  R4,HEADTAB          Adjust column headings                       
INIRUN12 CLI   0(R4),EOT           End of table                                 
         JE    INIRUN14                                                         
         L     RE,0(R4)                                                         
         MVC   0(5,RE),=C'Net  '                                                
         CLI   QOPT2,FILE$NET      Net amounts                                  
         JE    *+10                Yes                                          
         MVC   0(5,RE),=C'Gross'   No, gross amounts                            
         AHI   R4,L'HEADTAB                                                     
         J     INIRUN12                                                         
                                                                                
INIRUN14 CLI   QOPT1,C'P'          Test printing only                           
         JE    INIRUN16                                                         
         GOTOR DYNALLOC,DMCB,(X'80',OUTDDNAM),(X'83',OUTTRK),OUTDSN             
         LA    R0,FILEOUT                                                       
         OPEN  ((R0),OUTPUT)                                                    
         LTR   RF,RF                                                            
         JZ    INIRUN16                                                         
         DC    H'0'                                                             
                                                                                
INIRUN16 LARL  R1,SCHTABLE                                                      
         GOTOR SCHOUT              Put out schema details                       
         LARL  R1,REPTABLE                                                      
         GOTOR SCHOUT              Put out report details                       
                                                                                
***********************************************************************         
* Build month table (MONTAB)                                          *         
***********************************************************************         
                                                                                
         GOTOR ADDAY,DMCB,(C'M',TODAY),QSTART,-18                               
         GOTOR (RF),(R1),,QEND,18                                               
                                                                                
         GOTOR ADDAY,DMCB,(C'M',QEND),WORK,1                                    
         GOTOR DATCON,DMCB,WORK,(3,FULL)                                        
                                                                                
         LA    R4,MONTAB                                                        
         USING MONTABD,R4          R4=A(Month table)                            
         SR    R0,R0               Initialize sequence number                   
         MVC   WORK(4),QSTART      Move EBCDIC start date                       
         MVC   WORK+4(2),=C'15'                                                 
                                                                                
INIRUN18 GOTOR DATCON,DMCB,WORK,(3,WORK+6)                                      
         CLC   WORK+6(3),FULL      Reached end of period                        
         JH    INIRUN20                                                         
                                                                                
         GOTOR (RF),(R1),(3,WORK+6),WORK+12                                     
         MVC   WORK(6),WORK+12                                                  
                                                                                
         MVC   MONTYYMM,WORK+12    Set YYMM in table (Y2K format)               
         AHI   R0,1                                                             
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MONTMONS,DUB                                                     
         GOTOR (RF),(R1),(X'43',WORK+6),(9,WORK+12)                             
         MVC   MONTMNTH,WORK+12    Set month (mmm/yy)                           
                                                                                
         GOTOR ADDAY,DMCB,(C'M',WORK),WORK+6,1                                  
         MVC   WORK(6),WORK+6                                                   
                                                                                
         AHI   R4,MONTABL                                                       
         J     INIRUN18                                                         
                                                                                
INIRUN20 XC    MONTABD(MONTABL),MONTABD                                         
         J     EXIT                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* Process input file for one system                                   *         
***********************************************************************         
PRCSYS   NTR1  LABEL=NO                                                         
                                                                                
         STC   R1,QSYS             Set current system letter                    
         CLI   QOPT4,C' '          Test system filter present                   
         JE    *+14                                                             
         CLC   QSYS,QOPT4          Apply system filter                          
         JNE   EXIT                                                             
                                                                                
         MVI   ASSIGNED,NO                                                      
         MVC   YYMMLEN,=AL2(DETLYYM1)                                           
         CLI   QSYS,SYSNETQ        Test Net system                              
         JE    PRCSYS02                                                         
         CLI   QSYS,SYSSPTQ        Test Spot system                             
         JE    PRCSYS04                                                         
         CLI   QSYS,SYSPRTQ        Test Print system                            
         JE    PRCSYS06                                                         
         DC    H'0'                                                             
                                                                                
PRCSYS02 LA    R0,NETIN            Net system                                   
         MVI   LMTMTHD,LQ_AMNET                                                 
         MVC   NROWS,=AL2(DETLNETR)                                             
         MVC   YYMMLEN,=AL2(DETLYYM2)                                           
         MVI   ASSIGNED,YES                                                     
         MVC   SYSNAME,=C'NET  '                                                
         J     PRCSYS08                                                         
                                                                                
PRCSYS04 LA    R0,SPOTIN           Spot system                                  
         MVI   LMTMTHD,LQ_AMSPT                                                 
         MVC   NROWS,=AL2(DETLSPTR)                                             
         MVC   SYSNAME,=C'SPOT '                                                
         J     PRCSYS08                                                         
                                                                                
PRCSYS06 LA    R0,PRNTIN           Print system                                 
         MVI   LMTMTHD,LQ_AMPRT                                                 
         MVC   NROWS,=AL2(DETLPRTR)                                             
         MVC   YYMMLEN,=AL2(DETLYYM2)   New print format                        
         MVC   SYSNAME,=C'PRINT'                                                
                                                                                
PRCSYS08 ST    R0,AFILEIN          Open the input file                          
         OPEN  ((R0),(INPUT))                                                   
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GETINP              Get first input record                       
         JNE   PRCSYS70                                                         
                                                                                
         MVC   FILEHDR(FILEHDRL),FILEREC                                        
                                                                                
         MVI   INSMTH,C'N'         default to no ins. mth $ present             
         CLC   QSYS,FILESYSC       Test correct system                          
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'A9HDR',FILERCDE  Test file type                               
         JE    *+6                                                              
         DC    H'0'                Not a valid file header                      
*                                                                               
         CLI   QSYS,SYSPRTQ        Print                                        
         BNE   PRCSYS09                                                         
         CLI   FILE$TYP,C' '       Is $ type present?                           
         BNH   PRCSYS10                                                         
         CLC   FILE$TYP,QOPT2      Test net/gross option matches                
         JE    PRCSYS10                                                         
         DC    H'0'                                                             
*                                                                               
PRCSYS09 CLI   QSYS,SYSNETQ                                                     
         JE    *+12                                                             
         CLI   QSYS,SYSSPTQ                                                     
         JNE   PRCSYS10                                                         
         CLC   FILE$TYP,QOPT2      Test net/gross option matches                
         JE    PRCSYS10                                                         
         DC    H'0'                                                             
*                                                                               
PRCSYS10 CLI   FILEMTYP,C'B'       Does file contain ins. mth $                 
         BNE   *+8                                                              
         MVI   INSMTH,C'Y'                                                      
*                                                                               
PRCSYS12 MVC   WRKRECL(LMTRECL),LMTREC                                          
         GOTOR PUTFIL,0            Output limit access values                   
                                                                                
         LA    R0,SAVEREC                                                       
         LHI   R1,SAVERECL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               Clear so no random matches                   
                                                                                
PRCSYS20 GOTOR GETINP              Get next input record                        
         JNE   PRCSYS70                                                         
                                                                                
         MVI   OUTPREC,NO          Reset to no                                  
         LA    R1,DETLYYMM         Test any dollars on this record              
         USING DETLYYMM,R1                                                      
         LH    R0,NROWS                                                         
PRCSYS22 CLC   DETLYYMM,SPACES                                                  
         JNH   PRCSYS24                                                         
         CP    DETLORDR,PACKZERO                                                
         JNE   PRCSYS30                                                         
         CP    DETLPAID,PACKZERO                                                
         JNE   PRCSYS30                                                         
         CP    DETLBILL,PACKZERO                                                
         JNE   PRCSYS30                                                         
         CLI   ASSIGNED,YES                                                     
         JNE   PRCSYS23                                                         
         CP    DETLASSN,PACKZERO                                                
         JNE   PRCSYS30                                                         
*                                                                               
PRCSYS23 CLI   INSMTH,YES          Does file contain ins mth ordered $?         
         JNE   PRCSYS24                                                         
         CP    DETLORDI,PACKZERO                                                
         JNE   PRCSYS30                                                         
*                                                                               
PRCSYS24 AH    R1,YYMMLEN          Bump to next month row                       
         JCT   R0,PRCSYS22                                                      
         J     PRCSYS20            No dollars so skip this record               
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Compare each field to previous and send all changed fields          *         
***********************************************************************         
                                                                                
PRCSYS30 GOTOR GETOFF              Get one character office code                
         GOTOR FLTOFF              Apply office filter (if any)                 
         JNE   PRCSYS20                                                         
                                                                                
         GOTOR CHKLMT              Check limit access values changed            
                                                                                
         LA    R0,WRKDATA          Clear the output area                        
         LHI   R1,WRKDATAL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR INIREC              Initialize output record                     
         USING LQ_D,R5             R5=A(Next output element)                    
         LA    R4,DATATAB          R4=A(Data table)                             
                                                                                
         CLI   SENTSYS,YES         Test sent system                             
         JE    PRCSYS34            Yes                                          
                                                                                
         MVI   LQ_EL,LQ_RAWDQ      Set unedited text element code               
         LHI   RF,MAPSYSCD         Get data type code                           
         STCM  RF,3,LQ_DCODE       Set in record                                
         MVI   LQ_DTYPE,LD_CHARQ   Set unedited char data follows               
         LHI   RF,LQ_LNDQ+L'QSYS                                                
         STCM  RF,3,LQ_LN          Set length of string                         
         MVC   LQ_DDATA(L'QSYS),QSYS                                            
         AR    R5,RF                                                            
                                                                                
         MVC   LQ_D(SYSMAPL),SYSMAP                                             
         AHI   R5,SYSMAPL          Bump to next output area                     
                                                                                
PRCSYS34 SR    R1,R1                                                            
         ICM   R1,3,0(R4)          Get data displacement                        
         LA    RE,FILEREC(R1)                                                   
         LA    RF,SAVEREC(R1)                                                   
         LLC   R1,2(R4)            Get compare length                           
                                                                                
         CLI   SENTSYS,YES         If this is new system, send all              
         JNE   PRCSYS36                                                         
         CLI   SENTLIM,YES         Test just sent limit access data             
         JE    PRCSYS36            Yes - send everything                        
         CLC   0(0,RE),0(RF)                                                    
         EX    R1,*-6              Test data changed                            
         JE    PRCSYS42            No                                           
                                                                                
PRCSYS36 MVI   LQ_EL,LQ_RAWDQ      Set unedited text element code               
         MVC   LQ_DCODE,3(R4)      Set map code                                 
         MVI   LQ_DTYPE,LD_CHARQ   Set unedited character data follows          
                                                                                
         SR    RF,RF                                                            
         ICM   RF,1,2(R4)          Get data length                              
         JZ    PRCSYS40                                                         
         LA    R1,0(RF,RE)         Point to last character in data              
PRCSYS38 CLI   0(R1),C' '                                                       
         JH    PRCSYS40                                                         
         BCTR  R1,0                                                             
         JCT   RF,PRCSYS38                                                      
                                                                                
PRCSYS40 MVC   LQ_DDATA(0),0(RE)   Move data                                    
         EX    RF,*-6                                                           
         AHI   RF,LQ_LNDQ+1        Add overhead                                 
         STCM  RF,3,LQ_LN          Set length of string                         
         AR    R5,RF                                                            
                                                                                
PRCSYS42 AHI   R4,L'DATATAB        Bump to next table entry                     
         CLI   0(R4),EOT                                                        
         JNE   PRCSYS34                                                         
                                                                                
***********************************************************************         
* Process prior dollars                                               *         
***********************************************************************         
                                                                                
         MVC   DOLLARS(DOLLARL),PACKZERO                                        
                                                                                
         LA    R6,DETLYYMM                                                      
         USING DETLYYMM,R6                                                      
         LH    R7,NROWS                                                         
                                                                                
PRCSYS44 CLC   DETLYYMM,SPACES     Test data present                            
         JNH   PRCSYS46                                                         
         CLC   DETLYYMM,QSTART     Test prior to start                          
         JNL   *+8                                                              
         GOTOR ADDDOL                                                           
         AH    R6,YYMMLEN                                                       
         JCT   R7,PRCSYS44                                                      
                                                                                
PRCSYS46 MVC   DOLMONS,=C'00'                                                   
         MVC   DOLMNTH,=C'Prior '                                               
         GOTOR PUTDOL              Put record to output file                    
         DROP  R6                                                               
                                                                                
***********************************************************************         
* Process current dollars                                             *         
***********************************************************************         
                                                                                
PRCSYS48 LA    R6,DETLYYMM         Process months in period                     
         USING DETLYYMM,R6                                                      
         LH    R7,NROWS                                                         
                                                                                
PRCSYS50 CLC   DETLYYMM,SPACES     Test data present                            
         JNH   PRCSYS56                                                         
         CLC   DETLYYMM,QSTART     Test within start/end dates                  
         JL    PRCSYS54                                                         
         CLC   DETLYYMM,QEND                                                    
         JH    PRCSYS54                                                         
                                                                                
         LA    R1,MONTAB           Locate month table entry                     
         USING MONTABD,R1                                                       
         LHI   R0,MONTMAXN                                                      
PRCSYS52 CLC   DETLYYMM,MONTYYMM   Match YYMM                                   
         JE    *+14                                                             
         AHI   R1,MONTABL                                                       
         JCT   R0,PRCSYS52                                                      
         DC    H'0'                                                             
         MVC   DOLMONS,MONTMONS    Set month sequence number                    
         MVC   DOLMNTH,MONTMNTH    Set month printable value                    
         DROP  R1                                                               
                                                                                
         MVC   DOLLARS(DOLLARL),PACKZERO                                        
         GOTOR ADDDOL                                                           
         GOTOR PUTDOL              Put record to output file                    
                                                                                
PRCSYS54 AH    R6,YYMMLEN                                                       
         JCT   R7,PRCSYS50                                                      
         DROP  R6                                                               
                                                                                
***********************************************************************         
* Process subseq dollars                                              *         
***********************************************************************         
                                                                                
PRCSYS56 MVC   DOLLARS(DOLLARL),PACKZERO                                        
                                                                                
         LA    R6,DETLYYMM         Sum subseq dollars                           
         USING DETLYYMM,R6                                                      
         LH    R7,NROWS                                                         
                                                                                
PRCSYS58 CLC   DETLYYMM,SPACES                                                  
         JNH   PRCSYS60                                                         
         CLC   DETLYYMM,QEND       Test higher than end date                    
         JNH   *+8                                                              
         GOTOR ADDDOL                                                           
         AH    R6,YYMMLEN                                                       
         JCT   R7,PRCSYS58                                                      
                                                                                
PRCSYS60 MVC   DOLMONS,=C'99'                                                   
         MVC   DOLMNTH,=C'Subseq'                                               
         GOTOR PUTDOL              Put record to output file                    
                                                                                
         CLI   OUTPREC,DETAIL      Did we actually put a record                 
         BNE   PRCSYS62            No, so don't copy to SAVREC                  
*                                                                               
         LA    R0,SAVEREC          Save off record just sent.                   
         LHI   R1,SAVERECL                                                      
         LA    RE,FILEREC                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
PRCSYS62 MVI   SENTSYS,YES                                                      
         MVI   SENTLIM,NO          Reset 'just sent limit access data'          
         J     PRCSYS20                                                         
                                                                                
***********************************************************************         
* Close input file and print totals                                   *         
***********************************************************************         
                                                                                
PRCSYS70 L     R0,AFILEIN                                                       
         CLOSE ((R0))                                                           
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR PRTTOT,CLTTOTS      Print last client totals                     
                                                                                
         MVC   TOTCLT,=C'***'                                                   
         GOTOR PRTTOT,MEDTOTS      Print last media totals                      
                                                                                
         MVI   TOTMED,C'*'                                                      
         GOTOR PRTTOT,AGYTOTS      Print agency totals                          
         J     EXIT                                                             
                                                                                
GETINP   NTR1  LABEL=NO            Get next record from input file              
         L     R1,AFILEIN                                                       
         LA    R0,FILEREC                                                       
         GET   (1),(0)                                                          
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Add dollar amounts from DETLDOLS into DOLLARS                       *         
***********************************************************************         
                                                                                
ADDDOL   AP    DOLORDR,DETLORDR                                                 
         CLI   QSYS,SYSPRTQ        Test Print                                   
         JNE   *+10                                                             
         AP    DOLORDI,DETLORDI    Ordered $ by insertion mth                   
         AP    DOLPAID,DETLPAID                                                 
         AP    DOLBILL,DETLBILL                                                 
         ZAP   DUB,DETLORDR                                                     
         CLI   QSYS,SYSNETQ        Test Net                                     
         JNE   ADDDOL02                                                         
         AP    DOLASSN,DETLASSN    Set assigned amount                          
         CLI   DETLACAS,DETLAASS   Test use ordered assigned for                
         JNE   ADDDOL02            billable                                     
         ZAP   DUB,DETLASSN                                                     
                                                                                
ADDDOL02 SP    DUB,DETLBILL        Calculate unbilled (billable)                
         AP    DOLUNBD,DUB                                                      
         BR    RE                                                               
         DROP  R6                                                               
                                                                                
***********************************************************************         
* DOLMONS has month sequence number (00 for prior, 99 for subseq)     *         
* DOLMNTH has mmm/yy or prior or subseq                               *         
* DOLLARS has dollar fields                                           *         
*                                                                               
* Note - Altough you think these would not be zero sometime they are            
*        because they back out the amount that went in, so they end             
*        up being zero. Only save the FILEREC if we actually put a              
*        record out.                                                            
***********************************************************************         
PUTDOL   NTR1  LABEL=NO                                                         
                                                                                
         CP    DOLORDR,PACKZERO    Check any values to output                   
         JNE   PUTDOL02                                                         
         CP    DOLPAID,PACKZERO                                                 
         JNE   PUTDOL02                                                         
         CP    DOLBILL,PACKZERO                                                 
         JNE   PUTDOL02                                                         
         CP    DOLASSN,PACKZERO                                                 
         JNE   PUTDOL02                                                         
         CP    DOLORDI,PACKZERO                                                 
         JNE   PUTDOL02                                                         
         CP    DOLUNBD,PACKZERO                                                 
         JE    EXITR5                                                           
                                                                                
PUTDOL02 CLC   TOTSYS,QSYS         Test same system                             
         JNE   *+14                                                             
         CLC   TOTMED,DETLMED      Test same media/client                       
         JE    PUTDOL04                                                         
                                                                                
         GOTOR PRTTOT,CLTTOTS      Change of media - client totals              
                                                                                
         MVC   TOTCLT,=C'***'                                                   
         GOTOR PRTTOT,MEDTOTS      Then media totals                            
         J     PUTDOL06                                                         
                                                                                
PUTDOL04 CLC   TOTCLT,DETLCLT                                                   
         JE    PUTDOL06                                                         
                                                                                
         GOTOR PRTTOT,CLTTOTS      Change of client - client totals             
                                                                                
PUTDOL06 MVC   TOTSYS,QSYS         Save current values                          
         MVC   TOTMED,DETLMED                                                   
         MVC   TOTCLT,DETLCLT                                                   
                                                                                
         GOTOR ADDTOT,CLTTOTS      Add current into client totals               
         CLI   QSYS,SYSNETQ        No media totals for Net                      
         JE    PUTDOL08                                                         
         GOTOR ADDTOT,MEDTOTS      Add current into Media totals                
                                                                                
PUTDOL08 GOTOR ADDTOT,AGYTOTS      Add current into agency totals               
                                                                                
         MVC   LQ_D(SEQMAPL),SEQMAP                                             
         MVC   LQ_D+SEQMAPL(L'DOLMONS),DOLMONS                                  
         AHI   R5,SEQMAPL+L'DOLMONS                                             
                                                                                
         MVC   LQ_D(MOSMAPL),MOSMAP                                             
         MVC   LQ_D+MOSMAPL(L'DOLMNTH),DOLMNTH                                  
         AHI   R5,MOSMAPL+L'DOLMNTH                                             
                                                                                
         LA    R2,AMTTAB                                                        
         USING AMTTABD,R2                                                       
PUTDOL10 OC    AMTMAP#,AMTMAP#     Test end of table                            
         JZ    PUTDOL16                                                         
         TM    AMTIND1,AMTIHIDE    Test hidden column                           
         JNZ   PUTDOL14                                                         
         ICM   RF,7,AMTCURR$       Point to current value                       
                                                                                
         CLI   SENTSYS,NO          Sent system code/name?                       
         JE    PUTDOL12            No, force resend of data                     
         CLI   SENTLIM,YES         Limited access changed                       
         JE    PUTDOL12            Yes, force resend of data                    
         CP    AMTPREV$,0(PACKLENQ,RF)                                          
         JE    PUTDOL14            Skip if same as previous                     
                                                                                
PUTDOL12 ZAP   AMTPREV$,0(PACKLENQ,RF)                                          
                                                                                
         MVI   LQ_EL,LQ_RAWDQ                                                   
         MVC   LQ_DCODE,AMTMAP#                                                 
         MVI   LQ_DTYPE,LD_SPAKQ                                                
         ZAP   LQ_DDATA(PACKLENQ),0(PACKLENQ,RF)                                
         LHI   R0,LQ_DDATA-LQ_D+PACKLENQ                                        
         STCM  R0,3,LQ_LN                                                       
         AR    R5,R0                                                            
                                                                                
PUTDOL14 AHI   R2,AMTTABL          Bump to next table entry                     
         J     PUTDOL10                                                         
                                                                                
PUTDOL16 MVI   LQ_EL,0             Set record terminator                        
         AHI   R5,1                                                             
         LA    R0,WRKRECL                                                       
         SR    R5,R0               Get record length                            
         SLL   R5,16                                                            
         STCM  R5,15,WRKRECL                                                    
         MVI   OUTPREC,DETAIL      We have put a detail record                  
         GOTOR PUTFIL,0                                                         
         GOTOR INIREC                                                           
         J     EXITR5                                                           
                                                                                
INIREC   LA    R5,WRKDATA          Set output pointer                           
         XC    LQ_D(256),LQ_D      Clear output area                            
         MVC   LQ_D(MAPELEML),MAPELEM                                           
         AHI   R5,MAPELEML                                                      
         BR    RE                                                               
                                                                                
ADDTOT   LA    RF,DOLLARS          Add current dollars into total line          
         LHI   R0,DOLLAR#                                                       
ADDTOT02 AP    0(PACKLENQ,R1),0(PACKLENQ,RF)                                    
         AHI   R1,PACKLENQ                                                      
         AHI   RF,PACKLENQ                                                      
         JCT   R0,ADDTOT02                                                      
         BR    RE                                                               
                                                                                
***********************************************************************         
* Output schema/report description to worker file                     *         
* On entry R1 points to appropriate table                             *         
***********************************************************************         
SCHOUT   NTR1  LABEL=NO                                                         
                                                                                
         LR    R2,R1                                                            
         SR    R3,R3                                                            
SCHOUT02 ICM   R3,3,0(R2)          R3=L'Current entry                           
         BCTR  R3,0                                                             
         CLI   2(R2),TURNOFF       If set then skip                             
         JE    SCHOUT04            Not zero so skip it                          
         MVC   WRKRECL(0),0(R2)                                                 
         EX    R3,*-6                                                           
         GOTOR PUTFIL,0                                                         
SCHOUT04 LA    R2,1(R3,R2)         Point to next entry                          
         CLI   0(R2),EOT           Test end of table                            
         JNE   SCHOUT02                                                         
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Put data to BSAM output file                                        *         
* On entry R1=0 to put record, 1 to put last record and close file    *         
***********************************************************************         
PUTFIL   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   QOPT1,C'P'          Test printing only                           
         JE    EXIT                                                             
                                                                                
         LARL  R2,OUTBLK           Point to start of block                      
         LTR   R3,R1               Test put & close                             
         JNZ   PUTFIL02                                                         
         LH    R0,0(R2)            Get current size                             
         AH    R0,WRKRECL          Add length of current record                 
         CHI   R0,OUTBLKL-4        Test record will fit                         
         JL    PUTFIL04            Yes                                          
                                                                                
PUTFIL02 WRITE OUTWRITE,SF,FILEOUT,(R2),MF=E                                    
         WAIT  ECB=OUTECB                                                       
         CHECK OUTECB                                                           
                                                                                
         L     R0,OUTBYTES                                                      
         AH    R0,0(R2)                                                         
         ST    R0,OUTBYTES                                                      
                                                                                
         LHI   R0,4                                                             
         STH   R0,0(R2)            Initialize new block                         
                                                                                
PUTFIL04 LTR   R3,R3               Test close after put                         
         JNZ   PUTFIL06                                                         
                                                                                
         LARL  RE,OUTBLK                                                        
         AH    RE,0(RE)            Point to next record position                
         LH    RF,WRKRECL          Get length of move                           
         LA    R0,WRKRECL          From address                                 
         LR    R1,RF               From length = to length                      
         MVCL  RE,R0                                                            
                                                                                
         L     R0,OUTCOUNT         Bump number of output records                
         AHI   R0,1                                                             
         ST    R0,OUTCOUNT                                                      
                                                                                
         LARL  R2,OUTBLK           Point to output block                        
         LH    R0,0(R2)            Get length so far                            
         AH    R0,WRKRECL          Add length of new record                     
         STH   R0,0(R2)            and store in block                           
                                                                                
         CLI   QOPT1,YES           Test to print as well                        
         JNE   EXIT                                                             
         LA    RF,WRKRECL                                                       
         LA    R0,P                                                             
         LH    R1,0(RF)                                                         
         LR    RE,R1                                                            
         MVCL  R0,RE                                                            
         GOTOR REPORT                                                           
         J     EXIT                                                             
                                                                                
PUTFIL06 LA    R0,FILEOUT          Close output file                            
         CLOSE ((R0))                                                           
         J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Print totals - R1 points to ordered/paid/billed/assigned/unbilled   *         
***********************************************************************         
                                                                                
PRTTOT   NTR1  LABEL=NO                                                         
         LR    RF,R1               R1=A(Accumulators)                           
         LHI   R0,DOLLAR#          Number of accumulators                       
                                                                                
PRTTOT02 CP    0(L'DOLORDR,R1),PACKZERO                                         
         JNE   PRTTOT04                                                         
         AHI   RF,L'DOLORDR                                                     
         JCT   R0,PRTTOT02                                                      
         J     PRTTOTX             No amounts to print                          
                                                                                
PRTTOT04 MVC   P+0(2),QAGY         Print agency                                 
         MVC   P+3(1),TOTMED       Print media                                  
         MVC   P+5(3),TOTCLT       Print client                                 
*                                                                               
         CLI   QOPT5+1,C'C'        DISPLAY CLIENT TOTALS?                       
         JE    PRTTOT05                                                         
         CLI   TOTMED,C'*'         AGY TOTAL                                    
         JE    PRTTOT05                                                         
         CLC   TOTCLT,=C'***'      MEDIA TOTAL                                  
         JE    PRTTOT05                                                         
         J     PRTTOT10            SKIP THIS ACCUMULATOR                        
                                                                                
PRTTOT05 LR    RF,R1                                                            
         LA    RE,P+10                                                          
         LHI   R0,DOLLAR#                                                       
PRTTOT06 MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
         ZAP   DUB,0(L'DOLORDR,RF)                                              
         ED    WORK(21),DUB                                                     
         MVC   0(18,RE),WORK+3     Print amount                                 
         AHI   RF,L'DOLLARS        Next accumulator                             
         AHI   RE,21               Move up in print line                        
         JCT   R0,PRTTOT06         Next                                         
         GOTOR REPORT                                                           
                                                                                
PRTTOT10 MVC   0(DOLLARL,R1),PACKZERO                                           
                                                                                
PRTTOTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Get one character office code                                       *         
***********************************************************************         
                                                                                
GETOFF   NTR1  LABEL=NO                                                         
                                                                                
         CLC   AGY,LAGY            Test change of agency                        
         JNE   GETOFF02                                                         
         CLC   QSYS,LSYS           Test change of system                        
         JNE   GETOFF02                                                         
         CLC   DETLMDOF,L2OFF      Test change of media office                  
         JE    EXIT                                                             
                                                                                
GETOFF02 MVC   LAGY,AGY                                                         
         MVC   LSYS,QSYS                                                        
         MVC   L2OFF,DETLMDOF                                                   
         MVC   L1OFF,DETLMDOF      Set/save possible 1 char office              
                                                                                
         USING OFFICED,WORK                                                     
         XC    OFFICED(OFCLENQ),OFFICED                                         
         MVC   OFCSYS,QSYS                                                      
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC2,DETLMDOF    Move 2-char office                           
                                                                                
         GOTOR OFFICER,DMCB,(C'2',OFFICED),ACOMFACS                             
         CLI   0(R1),0                                                          
         JNE   GETOFFX                                                          
         MVC   L1OFF,OFCOFC        Set/save possible 1 char office              
                                                                                
GETOFFX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Apply office filter (QCLT)                                          *         
***********************************************************************         
                                                                                
FLTOFF   CLI   QCLT,C'*'                                                        
         JNE   FLTOFFY             Not filtering office                         
         CLI   QCLT+1,C' '         Test filter present                          
         JE    FLTOFFY                                                          
         CLI   QCLT+1,C'-'                                                      
         JNE   FLTOFF02                                                         
         CLC   L1OFF,QCLT+2        Exclude                                      
         JE    FLTOFFN                                                          
         J     FLTOFFY                                                          
                                                                                
FLTOFF02 CLC   L1OFF,QCLT+1        Match office code                            
         JL    FLTOFFN                                                          
         JE    FLTOFFY                                                          
         CLI   QCLT+2,C' '         Test range of offices                        
         JE    FLTOFFN                                                          
         CLC   L1OFF,QCLT+2        Test in range                                
         JH    FLTOFFN                                                          
                                                                                
FLTOFFY  CR    RE,RE               Set CC=equal                                 
         BR    RE                                                               
                                                                                
FLTOFFN  LTR   RE,RE               Set CC=not equal                             
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Test for change in limit access values and send element if needed   *         
* Whenever any limit access value changes, send them all              *         
***********************************************************************         
                                                                                
CHKLMT   NTR1  LABEL=NO                                                         
         MVC   WORK+0(1),DETLAGMD  Agency/Media or Media                        
         MVC   WORK+1(1),L1OFF                                                  
         MVC   WORK+2(3),DETLCLT                                                
         MVC   WORK+5(3),DETLCACC                                               
                                                                                
         CLI   SENTSYS,YES         Test new system                              
         JNE   CHKLMT02            Yes - send everything                        
         CLC   LLMTACC,WORK        Test change of limit access data             
         JE    EXIT                                                             
                                                                                
CHKLMT02 MVI   SENTLIM,YES         Set just sent limit access data              
         MVI   SENTSYS,NO          Set that we need system again                
         MVC   LLMTACC,WORK        Save what caused us to send                  
         XC    BCLT,BCLT                                                        
         CLI   QSYS,SYSPRTQ        Print doesn't use packed clients             
         JE    CHKLMT04                                                         
         GOTOR CLPACK,DMCB,DETLCLT,BCLT                                         
                                                                                
CHKLMT04 LA    R0,WRKDATA          Clear the output area                        
         LHI   R1,WRKDATAL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R5,WRKDATA                                                       
                                                                                
         MVC   LQ_DCODE,LMTMEDC    Agency/media or media                        
         MVC   LQ_VVALU(L'DETLAGMD),DETLAGMD                                    
         GOTOR CHKSET,L'DETLAGMD                                                
                                                                                
*        MVC   LQ_DCODE,LMTOFFC    Media office (replaced w/ internal)          
*        MVC   LQ_VVALU(L'DETLMDOF),DETLMDOF                                    
*        GOTOR CHKSET,L'DETLMDOF                                                
                                                                                
         MVC   LQ_DCODE,LMTIMOC    Internal Media office                        
         MVC   LQ_VVALU(L'L1OFF),L1OFF                                          
         GOTOR CHKSET,L'L1OFF                                                   
                                                                                
         MVC   LQ_DCODE,LMTECLC    EBCDIC client code                           
         MVC   LQ_VVALU(L'DETLCLT),DETLCLT                                      
         GOTOR CHKSET,L'DETLCLT                                                 
                                                                                
         MVC   LQ_DCODE,LMTPCLC    Packed client code                           
         MVC   LQ_VVALU(L'BCLT),BCLT                                            
         GOTOR CHKSET,L'BCLT                                                    
                                                                                
         MVC   LQ_DCODE,LMTCLAC    Client limit access codes                    
         MVC   LQ_VVALU(L'DETLCACC),DETLCACC                                    
         GOTOR CHKSET,L'DETLCACC                                                
                                                                                
         MVC   LQ_D(MAPELEML),MAPELEM                                           
         AHI   R5,MAPELEML                                                      
                                                                                
         MVI   LQ_EL,0             Set record terminator                        
         AHI   R5,1                                                             
                                                                                
         LA    R0,WRKRECL                                                       
         SR    R5,R0                                                            
         SLL   R5,16               Left align in register                       
         STCM  R5,15,WRKRECL                                                    
         GOTOR PUTFIL,0                                                         
         J     EXIT                                                             
                                                                                
CHKSET   MVI   LQ_EL,LQ_VALUQ      Set element code and length                  
         AHI   R1,LQ_LNCQ                                                       
         STCM  R1,3,LQ_LN                                                       
         AR    R5,R1               Point to next output element                 
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Add Control file pointer for BSAM file just created                 *         
***********************************************************************         
ADDPTR   NTR1  BASE=*,LABEL=*                                                   
         CLI   WRITESW,YES                                                      
         JNE   EXIT                                                             
                                                                                
         GOTOR DATCON,DMCB,(0,TODAY),(2,CRTDATEC)                               
         GOTOR DATCON,DMCB,(5,0),(1,REALDATE)                                   
         GOTOR DATCON,DMCB,(X'30',TODAY),(1,LASTDAY),(1,0)                      
         GOTOR DATCON,DMCB,(0,TODAY),(1,DUB)                                    
         GOTOR ADDAY,DMCB,(C'D',TODAY),WORK,8                                   
         CLC   LASTDAY,DUB         Last day of the month ?                      
         BNE   ADDPTR10            No                                           
         GOTOR DATCON,DMCB,(1,LASTDAY),(0,DUB)                                  
         GOTOR ADDAY,DMCB,(C'Y',DUB),WORK,4                                     
                                                                                
ADDPTR10 GOTOR DATCON,DMCB,(0,WORK),(1,EXPDATE)                                 
         ICM   RE,15,VUTL                                                       
         BNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   SVSE,4(RE)                                                       
         MVI   4(RE),10             Control system                              
*                                                                               
         L     R2,AIO                                                           
         GOTOR DATAMGR,DMCB,DMCLOSE,CONTROL,FLIST,(R2)                          
         CLI   8(R1),0             Close so we can open for update              
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING ISDTF,R3                                                         
         GOTOR DATAMGR,DMCB,DMDTF,GENDIR,FULL,FULL                              
         L     R3,12(,R1)                                                       
         NILH  GR3,X'00FF'         R3                                           
         NI    ISFOPEN,X'FF'-ISFORO                                             
*                                                                               
         USING DTFPHD,R3                                                        
         GOTOR DATAMGR,DMCB,DMDTF,GENFIL,FULL,FULL                              
         L     R3,12(,R1)                                                       
         NILH  GR3,X'00FF'         R3                                           
         NI    DTFOPEN,X'FF'-DTF_RO                                             
         DROP  R3                                                               
                                                                                
         GOTOR DATAMGR,DMCB,DMOPEN,CONTROL,FLIST,(R2)                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
***********************************************************************         
* Build key and read high for update, this will enqueue CTRL files              
***********************************************************************         
         USING GARCD,R2                                                         
         GOTOR DATAMGR,PARMS,ENQCTL,(C'E',=C'CTRL')                             
         TM    8(R1),X'04'                                                      
         BO    *+6                                                              
         DC    H'00'                                                            
                                                                                
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   GARKTYP,GARKTYPQ    C'A' Archive                                 
         MVC   GARKAGY,SVAGY                                                    
         MVC   GARKFILE,=C'A9..A'                                               
         MVC   GARKCDAT,CRTDATEC                                                
         L     R2,AIO                                                           
         MVI   ERR,1                                                            
         GOTOR DATAMGR,DMCB,DMRDHI,GENDIR,IOKEY,(R2)                            
         CLI   8(R1),0                                                          
         BNE   ADDPTRNO                                                         
*                                  Did one exist already                        
         CLC   GARKEY(GARKSEQN-GARKEY),IOKEY                                    
         BNE   ADDPTR20            No                                           
         MVI   ERR,2                                                            
         LLC   RF,GARKSEQN         Yes                                          
         SHI   RF,1                Change sequence then add                     
         BM    ADDPTRNO                                                         
                                                                                
         STC   RF,GARKSEQN         Set new sequence number                      
         MVI   GARRSTAT,GARKBIGL   Element have 2 byte length                   
         MVC   GARREXPD,EXPDATE    New expiration date                          
         XC    GARRSYS,GARRSYS                                                  
         B     ADDPTR30                                                         
                                                                                
ADDPTR20 XC    0(255,R2),0(R2)     Clear it out a bit                           
         MVC   GARKEY,IOKEY        Reset key                                    
         MVI   GARKSEQN,X'FF'      complement or count down                     
         OI    GARRSTAT,GARKBIGL   Element have 2 byte length                   
         MVC   GARREXPD,EXPDATE    New expiration date                          
                                                                                
***********************************************************************         
* Build record to put to worker file                                            
* Object 15 and 1A                                                              
***********************************************************************         
ADDPTR30 LARL  R3,FE00A            Point to 1st element                         
         SR    R1,R1               Output file desc. to worker file             
         ICM   R1,3,1(R3)          Get length                                   
         BCTR  R1,0                                                             
         MVC   GARRFST,0(R3)       Move it in                                   
         EX    R1,*-6                                                           
         LA    R3,GARRFST+1(R1)    Point to end of record                       
                                                                                
B        USING LQ_D,R3                                                          
         MVI   B.LQ_EL,LQ_BSAMQ    Build BSAM pointer element                   
         LHI   R0,LQ_LNBQ                                                       
         STCM  R0,3,B.LQ_LN                                                     
         MVC   B.LQ_BRECS,OUTCOUNT                                              
         MVC   B.LQ_BSIZE,OUTBYTES                                              
         MVC   B.LQ_BDSN,OUTDSN                                                 
         LA    R3,LQ_LNBQ(,R3)                                                  
         DROP  B                                                                
                                                                                
         USING BSXELD,R3                                                        
         MVI   BSXEL,BSXELQ        Extra BSAM info (X'F8')                      
         LHI   R0,BSXLNQ                                                        
         STCM  R0,3,BSXLN                                                       
         MVC   BSXDATE,REALDATE                                                 
         MVC   BSXTIME,REALTIME                                                 
         AR    R3,R0                                                            
         XC    0(2,R3),0(R3)       Clear end                                    
         AHI   R3,1                                                             
         SR    R3,R2                                                            
         STH   R3,GARRLEN                                                       
         DROP  R2,R3                                                            
                                                                                
ADDPTR80 DS    0H                                                               
         GOTOR DATAMGR,DMCB,(X'80',ADDREC),GENFIL,DISK,(R2),IOWRK               
         CLI   8(R1),0                                                          
         BE    ADDPTR90                                                         
         MVI   ERR,3                                                            
                                                                                
ADDPTRNO GOTOR DATAMGR,PARMS,ENQCTL,(C'D',=C'CTRL')                             
         GOTOR DATAMGR,PARMS,DMCLOSE,CONTROL,FLIST,(R2)                         
         LA    R1,DMCB                                                          
         DC    H'00'         Failed, dequeue control file                       
                                                                                
ADDPTR90 GOTOR DATAMGR,PARMS,ENQCTL,(C'D',=C'CTRL')                             
         GOTOR DATAMGR,DMCB,DMCLOSE,CONTROL,FLIST,(R2)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
ADDPTRX  L     RE,VUTL                                                          
         MVC   4(1,RE),SVSE        Restore                                      
         J     EXIT                                                             
                                                                                
ENQCTL   DC    CL8'ENQCTL'                                                      
CONTROL  DC    CL8'CONTROL'                                                     
GENDIR   DC    CL8'GENDIR'                                                      
GENFIL   DC    CL8'GENFIL'                                                      
FLIST    DS    0CL8                                                             
         DC    C'UGENDIR '                                                      
         DC    C'UGENFIL '                                                      
         DC    C'UCTRCVR '                                                      
         DC    C'X       '                                                      
ERR      DC    AL1(0)                                                           
DISK     DS    F                                                                
IOKEY    DS    XL32                                                             
IOWRK    DS    CL96                                                             
AIO      DC    A(IO)                                                            
***********************************************************************         
* Add worker file pointing to BSAM file just created                  *         
***********************************************************************         
                                                                                
         USING UKRECD,WRKRINDX                                                  
ADDWRK   NTR1  BASE=*,LABEL=*                                                   
         CLI   QOPT1,C'P'          Test printing only                           
         JNE   ADDWRK10                                                         
         GOTOR WRKIT,REC           Build data and print it                      
         J     EXIT                                                             
                                                                                
ADDWRK10 GOTOR DATCON,DMCB,(0,TODAY),(1,TODAY1)                                 
         GOTOR DATCON,DMCB,(X'30',TODAY),(1,LASTDAY),(1,0)                      
                                                                                
         GOTOR DATCON,DMCB,(0,TODAY),(2,CRTDATEC)   Run date/creation           
         GOTOR DATCON,DMCB,(0,TODAY),(30,CRTDATEN)  Run date/creation           
                                                                                
         GOTOR ADDAY,DMCB,(C'D',TODAY),WORK,RETDAYS                             
         GOTOR DATCON,DMCB,(0,WORK),(1,EXPDATE)                                 
         GOTOR DATCON,DMCB,(0,WORK),(2,EXPDATEC)                                
         GOTOR DATCON,DMCB,(0,WORK),(30,EXPDATEN)                               
                                                                                
         MVI   ARCHIVE,NO                                                       
         XC    UKINDEX,UKINDEX     Build key for new file                       
         MVC   UKUSRID,RCORIGID                                                 
         MVC   UKSYSPRG(4),=C'A9**'                                             
         MVC   UKDAY,TODAY1+2                                                   
         MVI   UKCLASS,C'A'                                                     
         LHI   R1,24*RETDAYS                                                    
         STCM  R1,3,RETAIN                                                      
         OI    UKATTB,WLATOBJ                                                   
                                                                                
         GOTOR WRKIT,GFILE                                                      
                                                                                
         CLC   LASTDAY,TODAY1      Is today the last day of the month?          
         BNE   ADDWRK20            No                                           
                                                                                
         GOTOR ADDAY,DMCB,(C'Y',TODAY),WORK,6                                   
         GOTOR DATCON,DMCB,(0,WORK),(1,EXPDATE)                                 
         GOTOR DATCON,DMCB,(0,WORK),(2,EXPDATEC)                                
         GOTOR DATCON,DMCB,(0,WORK),(30,EXPDATEN)                               
         L     R1,=A(24*365*7)     7 Years                                      
         STCM  R1,3,RETAIN                                                      
         MVI   ARCHIVE,YES                                                      
                                                                                
ADDWRK20 GOTOR WRKIT,SOF                                                        
         GOTOR WRKIT,REC           Add data record to worker file               
         GOTOR WRKIT,EOF                                                        
         GOTOR WRKIT,KEEP                                                       
         J     EXIT                No                                           
                                                                                
***********************************************************************         
* Add record to worker file                                           *         
***********************************************************************         
WRKIT    NTR1                                                                   
         STC   R1,WKACTION                                                      
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     WKFILE#             0 - Get worker file name WRKFn               
         B     WKSOF               1 - Create a file                            
         B     WKREC               2 - Add specific record to file              
         B     WKEOF               3 - Close file                               
         B     WKSRCH              4 - Search for file                          
         B     WKKEEP              5 - Keep file                                
         B     WKSOF               6 - Open file for modifiy                    
         B     WKGET               7 - Get a record from file                   
         B     WKPUT               8 - update record from file                  
                                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
WKFILE#  GOTOR DATAMGR,DMCB,WKGFILE,WRKFILEN,UKINDEX,WRKDATA,AWKBUFF            
         MVC   WRKFILE,UKUSRINF                                                 
         B     WRKIOCC             Check condition code                         
                                                                                
***********************************************************************         
* Create a file                                                                 
***********************************************************************         
WKSOF    XC    WLHDRD(256),WLHDRD  Build header                                 
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLINDEX,UKINDEX                                                  
         MVC   WLRETNL,RETAIN                                                   
         MVC   WLPSWD,SPACES                                                    
         CLI   WKACTION,REOPEN                                                  
         BNE   WRKIO                                                            
         MVC   WLFILENO,UKFILENO                                                
         OC    WLFILENO,WLFILENO                                                
         JZ    *+2                                                              
         OI    WLFLAG,WLFLMOD+WLFLREFN+WLFLRCOP                                 
         B     WRKIO                                                            
                                                                                
***********************************************************************         
* Build record to put to worker file                                            
* Object 15 and 1A                                                              
***********************************************************************         
WKREC    LARL  R2,FE00                                                          
         SR    R1,R1               Output file desc. to worker file             
         ICM   R1,3,0(R2)                                                       
         BCTR  R1,0                                                             
         BASR  RF,0                RF = A(Next instruction)                     
         MVC   WRKRECL(0),0(R2)                                                 
         EX    R1,0(RF)                                                         
         LARL  R2,WRKRECL          Point to end of record                       
         AR    R2,R1                                                            
                                                                                
B        USING LQ_D,R2                                                          
         MVI   B.LQ_EL,LQ_BSAMQ    Build BSAM pointer element                   
*AW      LHI   R0,LQ_LNB2Q                                                      
         LHI   R0,LQ_LNB3Q                                                      
         STCM  R0,3,B.LQ_LN                                                     
         MVC   B.LQ_BRECS,OUTCOUNT                                              
         MVC   B.LQ_BSIZE,OUTBYTES                                              
         MVC   B.LQ_BDSN,OUTDSN                                                 
                                                                                
         OI    B.LQ_BIND,LQ_DTE14  Type 14, support for dates > 2027            
         MVC   B.LQ_BRUND,CRTDATEN Run date of data (creation)                  
         MVC   B.LQ_BEXPD,EXPDATEN Expiration of report                         
         MVI   B.LQ_D+LQ_LNB3Q,0                                                
                                                                                
*AW      MVC   B.LQ_BRUND,CRTDATEC Run date of data (creation)                  
*AW      MVC   B.LQ_BEXPD,EXPDATEC Expiration of report                         
*AW      MVI   B.LQ_D+LQ_LNB2Q,0                                                
                                                                                
         DROP  B                                                                
                                                                                
         AH    R0,WRKRECL                                                       
         STH   R0,WRKRECL          Adjust record length                         
         CLI   QOPT1,C'P'          Print                                        
         BNE   WRKIO                                                            
         LA    RE,WRKRECL                                                       
         LA    R0,P                                                             
         LH    R1,WRKRECL                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTOR REPORT                                                           
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Get record from specific worker file                                          
***********************************************************************         
WKGET    OI    UKFLAG,UKFLDAT                                                   
         GOTOR DATAMGR,DMCB,WKREAD,WRKFILE,UKINDEX,WRKRECL,AWKBUFF              
         B     WRKIOCC                                                          
                                                                                
***********************************************************************         
* Put record from specific worker file                                          
***********************************************************************         
WKPUT    GOTOR DATAMGR,DMCB,WKWRITE,WRKFILE,UKINDEX,WRKDATA,AWKBUFF             
         B     WRKIOCC                                                          
                                                                                
***********************************************************************         
* Close a file                                                                  
***********************************************************************         
WKEOF    XC    WLHDRD(256),WLHDRD  Build header                                 
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         B     WRKIO                                                            
                                                                                
***********************************************************************         
* Search for a file                                                             
***********************************************************************         
WKSRCH   MVC   SVINDEX,UKINDEX                                                  
WKSRCH10 GOTOR DATAMGR,DMCB,WKINDEX,WRKFILE,UKINDEX,WRKDATA,AWKBUFF             
         CLI   8(R1),0                                                          
         JNE   WKSRCH30                                                         
         CLC   SVINDEX,UKINDEX     Match on whole key CL8                       
         BNE   WKSRCH10            Try again                                    
         MVC   WLKEY,UKKEY                                                      
         J     EXIT                                                             
*                                                                               
WKSRCH30 TM    8(R1),X'90'         Not found                                    
         JO    EXIT                                                             
         DC    H'00'                                                            
                                                                                
***********************************************************************         
* Keep a file                                                                   
***********************************************************************         
WKKEEP   GOTOR DATAMGR,DMCB,DMKEEP,WRKFILE,UKINDEX,WRKDATA,AWKBUFF              
         B     WRKIOCC                                                          
                                                                                
WRKIO    GOTOR DATAMGR,DMCB,DMPRINT,WRKFILE,0,WRKRECL,AWKBUFF                   
         CLI   WKACTION,SOF                                                     
         JNE   WRKIOCC                                                          
                                                                                
         MVC   UKFILENO,WLREPRNO   Worker file number                           
         GOTOR REPORT                                                           
         MVC   P(16),=C'Worker file key='                                       
         EDITR (B2,WLUSRID),(5,P+16)                                            
         MVI   P+21,C','                                                        
         MVC   P+22(4),WLFILEID                                                 
         GOTOR HEXOUT,PARMS,WLDAY,P+26,1,0                                      
         MVC   P+28(1),WLCLASS                                                  
         MVI   P+29,C','                                                        
         EDITR WLREPRNO,(5,P+30),0,ALIGN=LEFT                                   
         GOTOR REPORT                                                           
                                                                                
WRKIOCC  CLI   DMCB+8,0                                                         
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Relative exits for sub-routines                                               
***********************************************************************         
EXITY    LHI   RE,1                                                             
         J     EXITCC                                                           
EXITN    LHI   RE,0                                                             
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                                                                
                                                                                
EXITR5   XIT1  REGS=(R5)                                                        
                                                                                
         DS    0D                  ** Global values **                          
         DC    C'*GLOBALS'                                                      
GLOBALS  DS    0D                                                               
         LTORG                                                                  
*                                                                               
*        TABLE OF SCHEMA MODIFIERS                                              
*                                                                               
CFMTTAB  DC    C'  ',C' '        QOPT3,QOPT5  SCHEMA MODIFIER                   
         DC    C'A ',C'A'        ASSIGNED  - NO INSERTION MONTH                 
         DC    C' B',C'I'        INSERTION MONTH - NO ASSIGNED                  
         DC    C'AB',C'B'        BOTH ASSIGNED AND INSERTION MONTH              
         DC    X'FFFFFF'         END OF TABLE                                   
*                                                                               
OUTWRITE WRITE OUTECB,SF,,MF=L                                                  
OUTTRK   DS    0XL6                                                             
OUTPRI   DC    AL3(15)                                                          
OUTSEC   DC    AL3(8*15)                                                        
*                                                                               
PRIMARY  DC    C'0015'             Primary tracks                               
SECONDRY DC    C'0120'             Secondary tracks                             
                                                                                
MAPELEM  DC    AL1(LQ_DLDDQ),AL2(LQ_LNCQ),AL2(DATAMAP#)                         
MAPELEML EQU   *-MAPELEM                                                        
                                                                                
DDSPOTIN DC    CL8'SPOTIN'                                                      
DDNETIN  DC    CL8'NETIN'                                                       
DDPRNTIN DC    CL8'PRNTIN'                                                      
                                                                                
SPOTIN   DCB   DDNAME=SPOTIN,DSORG=PS,RECFM=FB,MACRF=GM,LRECL=768,     +        
               EODAD=EXITN                                                      
                                                                                
PRNTIN   DCB   DDNAME=PRNTIN,DSORG=PS,RECFM=FB,MACRF=GM,LRECL=900,     +        
               EODAD=EXITN                                                      
                                                                                
NETIN    DCB   DDNAME=NETIN,DSORG=PS,RECFM=FB,MACRF=GM,LRECL=565,      +        
               EODAD=EXITN                                                      
                                                                                
FILEOUT  DCB   DDNAME=FILEOUT,BLKSIZE=OUTBLKL,DSORG=PS,MACRF=(W),      +        
               LRECL=08000,RECFM=V                                              
                                                                                
OUTDDNAM DC    CL8'FILEOUT'                                                     
                                                                                
OUTDSN   DC    CL35'SPTDISK.SAGaaaa.Dyymmdd.Thhmmss.t'                          
         ORG   OUTDSN+11                                                        
OUTAGYC  DS    CL4                                                              
         DS    CL2                                                              
OUTDATE  DS    CL6                                                              
         DS    CL2                                                              
OUTTIME  DS    CL6                                                              
         DS    CL1                                                              
OUT$TYP  DS    CL1                 N or G                                       
         ORG                                                                    
                                                                                
OUTCOUNT DC    F'0'                Number of records written                    
OUTBYTES DC    F'0'                Number of bytes written                      
                                                                                
SYSSPTQ  EQU   C'S'                Spot system                                  
SYSPRTQ  EQU   C'P'                Print system                                 
SYSNETQ  EQU   C'N'                Net system                                   
                                                                                
K        EQU   1024                                                             
PACKLENQ EQU   8                                                                
RETDAYS  EQU   5                   Worker file retention days                   
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   X'FF'                                                            
TURNOFF  EQU   X'FF'                                                            
WRITESW  DS    X                   WRITE=NO or YES                              
SVSE     DS    X                   Saved SE#                                    
DSPACE   DS    C                                                                
OUTPREC  DS    C                                                                
DETAIL   EQU   C'D'                                                             
                                                                                
PARMS    DS    6F                                                               
VUTL     DS    A                   V(UTL)                                       
VSSB     DS    A                   V(SSB)                                       
AFILEIN  DC    A(0)                Address of current input file                
AWKBUFF  DC    A(WKBUFF)                                                        
OFFICER  DS    A                                                                
NROWS    DS    H                   Number of accumulator rows                   
YYMMLEN  DS    H                   Width of an accumulator row                  
INSMTH   DS    C                   Ins. mth ordered $ present? (Y/N)            
ASSIGNED DS    C                   Assigned dollars (Yes/No)                    
QSYS     DS    C                   Current system (S, N or P)                   
SENTSYS  DC    AL1(NO)             System sent                                  
SENTLIM  DC    AL1(NO)             Limit access sent                            
                                                                                
LSYS     DS    CL(L'QSYS)          Last system                                  
LAGY     DS    CL(L'AGY)           Last agency                                  
L1OFF    DS    C                   Last 1 character media office                
L2OFF    DS    CL(L'DETLMDOF)      Last 2 character media office                
LLMTACC  DC    XL8'00'             Last limit access values                     
*                                                                               
EXPDATE  DS    XL3                 Expiration date                              
EXPDATEC DS    XL2                 Expiration date (compressed)                 
EXPDATEN DS    XL2                 Expiration date (support for >2027)          
CRTDATEC DS    XL2                 Creation date   (compressed)                 
CRTDATEN DS    XL2                 Creation date   (support for >2027)          
RETAIN   DS    XL2                 Retention hours                              
*                                                                               
DAYARC   EQU   X'80'               Archive day value (Not used)                 
SVINDEX  DS    XL8                                                              
WKACTION DC    X'00'                                                            
GFILE    EQU   0                                                                
SOF      EQU   1                                                                
REC      EQU   2                                                                
EOF      EQU   3                                                                
SRCH     EQU   4                                                                
KEEP     EQU   5                                                                
REOPEN   EQU   6                                                                
GETWK    EQU   7                                                                
PUTWK    EQU   8                                                                
*                                                                               
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLOSE  DC    CL8'DMCLSE'                                                      
DMKEEP   DC    CL8'KEEP'                                                        
DMPRINT  DC    CL8'DMPRINT'                                                     
DMDTF    DC    CL8'DTFAD'                                                       
*                                                                               
WKINDEX  DC    CL8'INDEX'                                                       
WKREAD   DC    CL8'READ'                                                        
WKOPEN   DC    CL8'OPEN'                                                        
WKSEQ    DC    CL8'SEQ'                                                         
WKADD    DC    CL8'ADD'                                                         
WKWRITE  DC    CL8'WRITE'                                                       
WKCLOSE  DC    CL8'CLOSE'                                                       
WKGFILE  DC    CL8'GFILE'                                                       
WRKFILE  DC    CL8'WRKF1  '                                                     
WRKFILEN DC    CL8'WRKFILE'                                                     
                                                                                
DOLMONS  DS    CL(L'MONTMONS)      Month sequence number (00,1-nn,99)           
DOLMNTH  DS    CL(L'MONTMNTH)      MOS name                                     
                                                                                
DOLLARS  DS    0PL(PACKLENQ)                                                    
DOLORDR  DS    PL(PACKLENQ)        Ordered value                                
DOLPAID  DS    PL(PACKLENQ)        Paid value                                   
DOLBILL  DS    PL(PACKLENQ)        Billed value                                 
DOLASSN  DS    PL(PACKLENQ)        Assigned value                               
DOLUNBD  DS    PL(PACKLENQ)        Unbilled value                               
DOLORDI  DS    PL(PACKLENQ)        Ordered by ins. mth - Print only             
DOLLARL  EQU   *-DOLLARS                                                        
DOLLAR#  EQU   (*-DOLLARS)/L'DOLLARS                                            
                                                                                
MEDTOTS  DC    (DOLLAR#)PL(PACKLENQ)'0'                                         
CLTTOTS  DC    (DOLLAR#)PL(PACKLENQ)'0'                                         
AGYTOTS  DC    (DOLLAR#)PL(PACKLENQ)'0'                                         
                                                                                
PACKZERO DC    (DOLLAR#)PL(PACKLENQ)'0'                                         
                                                                                
TOTSYS   DC    CL(L'QSYS)' '                                                    
TOTMED   DC    CL(L'DETLMED)' '                                                 
TOTCLT   DC    CL(L'DETLCLT)' '                                                 
                                                                                
ARCHIVE  DC    AL1(NO)                                                          
LASTDAY  DS    XL3                 Last day of month packed unsigned            
TODAY1   DS    XL3                 Today packed unsigned                        
REALTIME DS    XL3                                                              
REALDATE DS    XL3                                                              
                                                                                
MONTAB   DS    (MONTMAXN)XL(MONTABL)                                            
         EJECT                                                                  
***********************************************************************         
* Tables                                                              *         
***********************************************************************         
                                                                                
FIXAMTS  DS    0XL4                   For qopt3                                 
         DC    C'A',AL3(AMTASSNG)                                               
         DC    C' ',AL3(AMTUNBD1)                                               
         DC    C'A',AL3(AMTUNBD2)                                               
         DC    AL1(EOT)                                                         
*                                                                               
FIXAMTS5 DS    0XL4                   For qopt 5                                
         DC    C'B',AL3(AMTORDI)       Ordered $ by insertion month             
         DC    AL1(EOT)                                                         
                                                                                
FIXSCHMA DS    0XL4                   For qopt3                                 
         DC    C' ',AL3(REP24)                                                  
         DC    C'A',AL3(REP25)                                                  
         DC    C'A',AL3(REP26)                                                  
         DC    C' ',AL3(REP27)        Insertion mth ordered                     
         DC    C'A',AL3(MPID19)                                                 
         DC    C' ',AL3(MPID20)                                                 
         DC    C'A',AL3(MPID21)                                                 
         DC    C' ',AL3(MPID22)       Insertion mth ordered                     
         DC    AL1(EOT)                                                         
                                                                                
FIXSCHMB DS    0XL4                    For qopt5                                
         DC    C'B',AL3(REP27)        Insertion mth ordered                     
         DC    C'B',AL3(MPID22)       Insertion mth ordered                     
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* Limit access record                                                 *         
***********************************************************************         
                                                                                
LMTREC   DC    AL2(LMTRECL)        Record length                                
         DC    AL2(0)                                                           
                                                                                
LMTELEM  DC    AL1(LQ_LIMAQ)                                                    
         DC    AL2(LMTELEML)                                                    
                                                                                
LMTMTHD  DC    AL1(0)              Limit access method                          
         DC    AL2(DATAMAP#)       Record map code                              
                                                                                
         DC    AL1(LQ_ATMED)                                                    
LMTMEDC  DC    AL2(LMTMEDQ)        Media code                                   
                                                                                
*        DC    AL1(LQ_ATOFF)       (replaced with internal office)              
*MTOFFC  DC    AL2(LMTOFFQ)        Office code                                  
                                                                                
         DC    AL1(LQ_ATIMO)                                                    
LMTIMOC  DC    AL2(LMTIMOQ)        Internal Media Office                        
                                                                                
         DC    AL1(LQ_ATECL)                                                    
LMTECLC  DC    AL2(LMTECLQ)        Client code (EBCDIC)                         
                                                                                
         DC    AL1(LQ_ATPCL)                                                    
LMTPCLC  DC    AL2(LMTPCLQ)        Client code (packed)                         
                                                                                
         DC    AL1(LQ_ATCLA)                                                    
LMTCLAC  DC    AL2(LMTCLAQ)        Client limit access value                    
                                                                                
         DC    AL1(0)              List terminator                              
                                                                                
LMTELEML EQU   *-LMTELEM                                                        
                                                                                
         DC    AL1(0)              EOR                                          
LMTRECL  EQU   *-LMTREC                                                         
                                                                                
***********************************************************************         
* Map data for system, MOS code and MOS name                          *         
***********************************************************************         
                                                                                
SYSMAP   DC    AL1(LQ_RAWDQ),AL2(SYSMAPL)                                       
         DC    AL2(MAPSYSNM),AL1(LD_CHARQ)                                      
SYSNAME  DC    CL5' '                                                           
SYSMAPL  EQU   *-SYSMAP                                                         
                                                                                
SEQMAP   DC    AL1(LQ_RAWDQ),AL2(SEQMAPL+L'DOLMONS)                             
         DC    AL2(MAPMOSCD),AL1(LD_CHARQ)                                      
SEQMAPL  EQU   *-SEQMAP                                                         
                                                                                
MOSMAP   DC    AL1(LQ_RAWDQ),AL2(MOSMAPL+L'DOLMNTH)                             
         DC    AL2(MAPMOSNM),AL1(LD_CHARQ)                                      
MOSMAPL  EQU   *-MOSMAP                                                         
                                                                                
***********************************************************************         
* These data fields are in fixed portion of record                    *         
***********************************************************************         
                                                                                
DATATAB  DS    0XL5                                                             
         DC    AL2(DETLMED-DETLRECD),AL1(L'DETLMED-1),AL2(MAPMEDCD)             
         DC    AL2(DETLMDNM-DETLRECD),AL1(L'DETLMDNM-1),AL2(MAPMEDNM)           
         DC    AL2(DETLMDOF-DETLRECD),AL1(L'DETLMDOF-1),AL2(MAPMOFCD)           
         DC    AL2(DETLCLT-DETLRECD),AL1(L'DETLCLT-1),AL2(MAPCLTCD)             
         DC    AL2(DETLCLNM-DETLRECD),AL1(L'DETLCLNM-1),AL2(MAPCLTNM)           
         DC    AL2(DETLACOF-DETLRECD),AL1(L'DETLACOF-1),AL2(MAPAOFCD)           
         DC    AL2(DETLPRD-DETLRECD),AL1(L'DETLPRD-1),AL2(MAPPRDCD)             
         DC    AL2(DETLPRNM-DETLRECD),AL1(L'DETLPRNM-1),AL2(MAPPRDNM)           
         DC    AL2(DETLEST-DETLRECD),AL1(L'DETLEST-1),AL2(MAPESTCD)             
         DC    AL2(DETLESNM-DETLRECD),AL1(L'DETLESNM-1),AL2(MAPESTNM)           
         DC    AL2(DETLTYPE-DETLRECD),AL1(L'DETLTYPE-1),AL2(MAPESTTY)           
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* Amount columns to process                                           *         
***********************************************************************         
                                                                                
AMTTAB   DS    0XL(AMTTABL)                                                     
AMTORDI  DC    AL2(MAPORDI),AL1(0),AL3(DOLORDI),PL(PACKLENQ)'0'                 
         DC    AL2(MAPORDR),AL1(0),AL3(DOLORDR),PL(PACKLENQ)'0'                 
         DC    AL2(MAPPAID),AL1(0),AL3(DOLPAID),PL(PACKLENQ)'0'                 
         DC    AL2(MAPUNPD),AL1(0),AL3(PACKZERO),PL(PACKLENQ)'0'                
         DC    AL2(MAPBLLD),AL1(0),AL3(DOLBILL),PL(PACKLENQ)'0'                 
AMTUNBD1 DC    AL2(MAPUNBD),AL1(0),AL3(PACKZERO),PL(PACKLENQ)'0'                
AMTASSNG DC    AL2(MAPASSN),AL1(0),AL3(DOLASSN),PL(PACKLENQ)'0'                 
AMTUNBD2 DC    AL2(MAPUNB2),AL1(0),AL3(DOLUNBD),PL(PACKLENQ)'0'                 
         DC    AL2(0)                                                           
         EJECT                                                                  
         DS    0D                                                               
WRKRINDX DC    XL48'00'            Worker file index entry                      
                                                                                
         DS    0D                                                               
         DC    CL8'*WRKREC*'                                                    
WRKRECL  DS    H                   Length goes here                             
         DS    H                                                                
WRKDATA  DS    XL512               Data goes here                               
WRKDATAL EQU   *-WRKDATA                                                        
                                                                                
         DS    0D                                                               
         DC    CL8'*FILEHDR'                                                    
FILEHDR  DS    CL(FILEHDRL)                                                     
                                                                                
         DS    0D                                                               
         DC    CL8'*FILEREC'                                                    
FILEREC  DS    XL(K)                                                            
                                                                                
         DS    0D                                                               
         DC    CL8'*SAVEREC'                                                    
SAVEREC  DS    XL(L'FILEREC)                                                    
SAVERECL EQU   *-SAVEREC                                                        
                                                                                
IO       DS    XL(2*K)                                                          
         EJECT                                                                  
***********************************************************************         
* Record/data map numbers                                             *         
***********************************************************************         
                                                                                
DATAMAP# EQU   8                   Map code of output data record               
                                                                                
MAPSYSCD EQU   1                   System code                                  
MAPSYSNM EQU   2                   System name                                  
MAPMEDCD EQU   3                   Media code                                   
MAPMEDNM EQU   4                   Media name                                   
MAPMOFCD EQU   5                   Media office                                 
MAPCLTCD EQU   6                   Client code                                  
MAPCLTNM EQU   7                   Client name                                  
MAPAOFCD EQU   8                   Account office                               
MAPPRDCD EQU   9                   Product code                                 
MAPPRDNM EQU   10                  Product name                                 
MAPESTCD EQU   11                  Estimate code                                
MAPESTNM EQU   12                  Estimate name                                
MAPESTTY EQU   13                  Estimate type                                
MAPMOSCD EQU   14                  MOS code                                     
MAPMOSNM EQU   15                  MOS name                                     
MAPORDR  EQU   16                  Ordered                                      
MAPPAID  EQU   17                  Cleared / Paid                               
MAPUNPD  EQU   18                  Not cleared / Un-paid                        
MAPBLLD  EQU   19                  Billed                                       
MAPUNBD  EQU   20                  Billable (Unbilled)                          
MAPASSN  EQU   20                  Ordered Assigned                             
MAPUNB2  EQU   21                  Billable (Unbilled), w/Assigned              
MAPORDI  EQU   22                  Ordered by insertion month (Print)           
                                                                                
LMTMEDQ  EQU   201                 Media code                                   
LMTOFFQ  EQU   202                 Office code                                  
LMTECLQ  EQU   203                 EBCDIC client code                           
LMTPCLQ  EQU   204                 Packed client code                           
LMTCLAQ  EQU   205                 Client limit access value                    
LMTIMOQ  EQU   207                 Internal Media Office code                   
         EJECT                                                                  
***********************************************************************         
* Macro for Schema                                                    *         
***********************************************************************         
                                                                                
         MACRO                                                                  
&TAG     SDEF  &IAMNODE=,&LABEL=,&MAPID=,&TYPE=C,&PREVNODE=,           +        
               &COLUMNOF=,&CHILDOF=,&GLOBID=,&MAXW=,&MAXTOT=                    
&TAG     DC    AL2(&TAG.Z-&TAG+1),AL2(0)                                        
.*                                                                              
&TAG.0   DC    AL1(LQ_DLDDQ),AL2(&TAG.1-&TAG.0),X'FE02'                         
.*                                                                              
&TAG.1   DC    AL1(LQ_RAWDQ),AL2(&TAG.2-&TAG.1),X'0001',AL1(LD_CHARQ)           
         DC    C'&LABEL'                                                        
&TAG.2   DC    AL1(LQ_RAWDQ),AL2(&TAG.3-&TAG.2),X'0002',AL1(LD_CBINQ)           
         DC    AL2(&MAPID)                                                      
&TAG.3   DC    AL1(LQ_RAWDQ),AL2(&TAG.4-&TAG.3),X'0003',AL1(LD_CHARQ)           
         DC    C'&TYPE'                                                         
&TAG.4   EQU   *                                                                
         AIF   (T'&IAMNODE NE 'O').A                                            
         AIF   (T'&CHILDOF  NE 'O').B                                           
         AIF   (T'&COLUMNOF NE 'O').C                                           
         MNOTE 1,'Missing IAMNODE,CHILDOF,COLUMNOF'                             
.*                                                                              
.A       ANOP                                                                   
         AIF   (T'&CHILDOF NE 'O').AERR                                         
         AIF   (T'&COLUMNOF  NE 'O').AERR                                       
&TAG.5   DC    AL1(LQ_RAWDQ),AL2(&TAG.6-&TAG.5),X'0004',AL1(LD_UBINQ)           
         DC    AL1(&IAMNODE)                                                    
&TAG.6   DC    AL1(LQ_RAWDQ),AL2(&TAG.7-&TAG.6),X'0005',AL1(LD_UBINQ)           
         DC    AL1(&PREVNODE)                                                   
&TAG.7   EQU   *                                                                
         AGO   .X                                                               
.*                                                                              
.AERR    ANOP                                                                   
         MNOTE 1,'IAMNODE so cant use CHILDOF or COLUMNOF'                      
         MEXIT                                                                  
.*                                                                              
.B       ANOP                                                                   
         AIF   (T'&IAMNODE NE 'O').BERR                                         
         AIF   (T'&COLUMNOF NE 'O').BERR                                        
&TAG.8   DC    AL1(LQ_RAWDQ),AL2(&TAG.9-&TAG.8),X'0006',AL1(LD_UBINQ)           
         DC    AL1(&CHILDOF)                                                    
&TAG.9   EQU   *                                                                
         AGO   .X                                                               
.BERR    ANOP                                                                   
         MNOTE 1,'CHILDOF so cant use IAMNODE or COLUMNOF'                      
         MEXIT                                                                  
.*                                                                              
.C       ANOP                                                                   
         AIF   (T'&IAMNODE NE 'O').CERR                                         
         AIF   (T'&CHILDOF NE 'O').CERR                                         
&TAG.10  DC    AL1(LQ_RAWDQ),AL2(&TAG.11-&TAG.10),X'0006',AL1(LD_UBINQ)         
         DC    AL1(&COLUMNOF)                                                   
&TAG.11  DC    AL1(LQ_RAWDQ),AL2(&TAG.12-&TAG.11),X'0008',AL1(LD_UBINQ)         
         DC    AL1(&MAXTOT)                                                     
&TAG.12  DC    AL1(LQ_RAWDQ),AL2(&TAG.13-&TAG.12),X'000B',AL1(LD_CHARQ)         
         DC    C'&GLOBID'                                                       
&TAG.13  EQU   *                                                                
         AGO   .XXX                                                             
.CERR    ANOP                                                                   
         MNOTE 1,'COLUMNOF so cant use IAMNODE or CHILDOF'                      
         MEXIT                                                                  
.*                                                                              
.X       ANOP                                                                   
&TAG.X   DC    AL1(LQ_RAWDQ),AL2(&TAG.Y-&TAG.X),X'000B',AL1(LD_CHARQ)           
         DC    C'&GLOBID'                                                       
&TAG.Y   DC    AL1(LQ_RAWDQ),AL2(&TAG.Z-&TAG.Y),X'000C',AL1(LD_UBINQ)           
         DC    AL1(&MAXW)                                                       
         AGO   .XXX                                                             
.*                                                                              
.XXX     ANOP                                                                   
&TAG.Z   DC    X'00'                                                            
         MEND                                                                   
         EJECT                                                                  
***********************************************************************         
* Actual schema                                                       *         
***********************************************************************         
                                                                                
SCHTABLE DS    0H                                                               
                                                                                
FE00     DC    AL2(FE00X-*),AL2(0)                                              
FE00A    DC    AL1(LQ_DESCQ),AL2(FE00B-FE00A)                                   
         DC    C'XAGY'                                                          
FE00CFMT DC    C' '                                                             
         DC    C'   A49',C'Agency Summary - '                                   
FE00DESC DC    CL47' '                                                          
FE00B    DC    AL1(0)                                                           
FE00X    EQU   *                                                                
                                                                                
FE01     DC    AL2(FE01X-*),AL2(0)                                              
FE01A    DC    AL1(LQ_DLDDQ),AL2(FE01B-FE01A),X'FE01'                           
FE01B    DC    AL1(LQ_RAWDQ),AL2(FE01C-FE01B),X'0001',AL1(LD_CHARQ)             
         DC    C'Doc'                                                           
FE01C    DC    AL1(LQ_RAWDQ),AL2(FE01D-FE01C),X'0002',AL1(LD_CBINQ)             
         DC    AL2(9999)                                                        
FE01D    DC    AL1(0)                                                           
FE01X    EQU   *                                                                
                                                                                
FE012    DC    AL2(FE012X-*),AL2(0)                                             
FE012A   DC    AL1(LQ_DLDDQ),AL2(FE012B-FE012A),X'FE01'                         
FE012B   DC    AL1(LQ_RAWDQ),AL2(FE012C-FE012B),X'0001',AL1(LD_CHARQ)           
         DC    C'S49'                                                           
FE012C   DC    AL1(LQ_RAWDQ),AL2(FE012D-FE012C),X'0002',AL1(LD_CBINQ)           
         DC    AL2(DATAMAP#)                                                    
FE012D   DC    AL1(LQ_RAWDQ),AL2(FE012E-FE012D),X'0004',AL1(LD_HDROQ)           
FE012E   DC    AL1(0)                                                           
FE012X   EQU   *                                                                
*                                                                               
*                                  System code                                  
MPID00   SDEF  IAMNODE=100,LABEL=SYSTEM,MAPID=MAPSYSCD,PREVNODE=8,     +        
               GLOBID=SYSC,MAXW=1,TYPE=C                                        
*                                  System name                                  
MPID01   SDEF  CHILDOF=100,LABEL=SYSTEM_NAME,MAPID=MAPSYSNM,           +        
               GLOBID=SYSN,MAXW=5,TYPE=C                                        
*                                  Media code                                   
MPID02   SDEF  IAMNODE=101,LABEL=MEDIA,MAPID=MAPMEDCD,PREVNODE=100,    +        
               GLOBID=MDCD,MAXW=L'DETLMED,TYPE=C                                
*                                  Media name                                   
MPID03   SDEF  CHILDOF=101,LABEL=MEDIA_NAME,MAPID=MAPMEDNM,            +        
               GLOBID=MDNM,MAXW=L'DETLMDNM,TYPE=C                               
*                                  Client media office                          
MPID04   SDEF  IAMNODE=102,LABEL=MEDIA_OFFC,MAPID=MAPMOFCD,            +        
               PREVNODE=101,GLOBID=MDOF,MAXW=L'DETLMDOF,TYPE=C                  
*                                  Client code                                  
MPID05   SDEF  IAMNODE=103,LABEL=CLIENT,MAPID=MAPCLTCD,PREVNODE=102,   +        
               GLOBID=CLCD,MAXW=L'DETLCLT,TYPE=C                                
*                                  Client name                                  
MPID06   SDEF  CHILDOF=103,LABEL=CLIENT_NAME,MAPID=MAPCLTNM,           +        
               GLOBID=CLNM,MAXW=L'DETLCLNM,TYPE=C                               
*                                  Client accounting office                     
MPID07   SDEF  IAMNODE=105,LABEL=CLIENT_ACOF,MAPID=MAPAOFCD,           +        
               PREVNODE=103,GLOBID=ACOF,MAXW=L'DETLACOF,TYPE=C                  
*                                  Product code                                 
MPID08   SDEF  IAMNODE=106,LABEL=PRODUCT,MAPID=MAPPRDCD,PREVNODE=105,  +        
               GLOBID=PRCD,MAXW=L'DETLPRD,TYPE=C                                
*                                  Product name                                 
MPID09   SDEF  CHILDOF=106,LABEL=PRODUCT_NAME,MAPID=MAPPRDNM,          +        
               GLOBID=PRNM,MAXW=L'DETLPRNM,TYPE=C                               
*                                  Estimate number                              
MPID10   SDEF  IAMNODE=108,LABEL=ESTIMATE,MAPID=MAPESTCD,              +        
               PREVNODE=106,GLOBID=ESCD,MAXW=L'DETLEST,TYPE=C                   
*                                  Estimate name                                
MPID11   SDEF  CHILDOF=108,LABEL=ESTIMATE_NAME,MAPID=MAPESTNM,         +        
               GLOBID=ESNM,MAXW=L'DETLESNM,TYPE=C                               
*                                  Estimate type                                
MPID12   SDEF  CHILDOF=108,LABEL=ESTIMATE_TYPE,MAPID=MAPESTTY,         +        
               GLOBID=ESTY,MAXW=L'DETLTYPE,TYPE=C                               
*                                  MOS sequence                                 
MPID13   SDEF  IAMNODE=109,LABEL=MOSCODE,MAPID=MAPMOSCD,PREVNODE=108,  +        
               GLOBID=MOSC,MAXW=L'DOLMONS,TYPE=C                                
*                                  MOS name                                     
MPID14   SDEF  CHILDOF=109,LABEL=MOS,MAPID=MAPMOSNM,                   +        
               GLOBID=MOS,MAXW=L'DOLMNTH,TYPE=C                                 
                                                                                
MPID15   SDEF  COLUMNOF=109,LABEL=ORDERED,MAPID=MAPORDR,MAXTOT=0,      +        
               GLOBID=ORDG,TYPE=L                                               
                                                                                
MPID16   SDEF  COLUMNOF=109,LABEL=CLEARED,MAPID=MAPPAID,MAXTOT=0,      +        
               GLOBID=CLRG,TYPE=L                                               
                                                                                
MPID17   SDEF  COLUMNOF=109,LABEL=UNCLEARED,MAPID=MAPUNPD,MAXTOT=0,    +        
               GLOBID=UNCG,TYPE=L                                               
                                                                                
MPID18   SDEF  COLUMNOF=109,LABEL=BILLED,MAPID=MAPBLLD,MAXTOT=0,       +        
               GLOBID=BLLG,TYPE=L                                               
                                                                                
MPID19   SDEF  COLUMNOF=109,LABEL=BILLABLE,MAPID=MAPUNBD,MAXTOT=0,     +        
               GLOBID=UNBG,TYPE=L                                               
                                                                                
MPID20   SDEF  COLUMNOF=109,LABEL=ASSIGNED,MAPID=MAPASSN,MAXTOT=0,     +        
               GLOBID=ASSG,TYPE=L                                               
                                                                                
MPID21   SDEF  COLUMNOF=109,LABEL=BILLABLE,MAPID=MAPUNB2,MAXTOT=0,     +        
               GLOBID=UNBG,TYPE=L                                               
                                                                                
MPID22   SDEF  COLUMNOF=109,LABEL=ORDERED,MAPID=MAPORDI,MAXTOT=0,      +        
               GLOBID=ORDG,TYPE=L                                               
                                                                                
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
         DC    AL2(FE03X-*),AL2(0)                                              
FE03A    DC    AL1(LQ_DLDDQ),AL2(FE03B-FE03A),X'FE03'                           
FE03B    DC    X'00'                                                            
FE03X    EQU   *                                                                
                                                                                
         DC    AL2(FE031X-*),AL2(0)                                             
FE031A   DC    AL1(LQ_DLDDQ),AL2(FE031B-FE031A),X'FE03'                         
FE031B   DC    X'00'                                                            
FE031X   EQU   *                                                                
                                                                                
         DC    AL1(EOT)            End of table                                 
         EJECT                                                                  
***********************************************************************         
* Macro for Report definition                                         *         
***********************************************************************         
                                                                                
         MACRO                                                                  
&TAG     RDEF  &NODEID=,&MAPID=,&TYPE=,&ALIGN=L,&ROW=,&COL=,&H1=,&H2=, +        
               &H3=,&DEC=2,&HIDE=,&SUBTYPE=,&GROUP=,&GLEVEL=,&NOFILT=           
         LCLA  &PROP                                                            
         GBLA  &#HEADS                                                          
         GBLC  &HEADS(8)                                                        
&DINK    SETC  ''''                                                             
                                                                                
&PROP    SETA  X'01'               Sort Accending                               
&TAG     DC    AL2(&TAG.X-&TAG),AL2(0)                                          
.*                                                                              
&TAG.0   DC    AL1(LQ_DLDDQ),AL2(&TAG.1-&TAG.0),X'FE31'                         
&TAG.1   DC    AL1(LQ_RAWDQ),AL2(&TAG.2-&TAG.1),X'0001',AL1(LD_UBINQ)           
         DC    AL1(&NODEID)                                                     
&TAG.2   DC    AL1(LQ_RAWDQ),AL2(&TAG.3-&TAG.2),X'000B',AL1(LD_CBINQ)           
         DC    AL2(&MAPID)                                                      
&TAG.3   DC    AL1(LQ_RAWDQ),AL2(&TAG.4-&TAG.3),X'0002',AL1(LD_CHARQ)           
         DC    C'&TYPE'                                                         
         AIF   ('&TYPE'(1,1) EQ 'C').C                                          
&TAG.4   DC    AL1(LQ_RAWDQ),AL2(&TAG.5-&TAG.4),X'0003',AL1(LD_CHARQ)           
         DC    C'&ALIGN,&ROW,&COL'                                              
&TAG.5   EQU   *                                                                
         AIF   (T'&H1 EQ 'O').C10                                               
&STRING  SETC  '&H1'                                                            
&LEN     SETA  K'&H1                                                            
&LPOS    SETA  ('&H1' INDEX '&DINK')                                            
         AIF   (&LPOS NE 1).TXT                                                 
&LEN     SETA  &LEN-2                                                           
&STRING  SETC  '&STRING'(2,&LEN)                                                
.TXT     ANOP                                                                   
         DC    AL1(LQ_RAWDQ),AL2(&TAG.6-&TAG.5),X'0005',AL1(LD_CHARQ)           
         DC    CL&LEN'&STRING'                                                  
&TAG.6   EQU   *                                                                
         AGO   .C10                                                             
                                                                                
.C       ANOP                                                                   
         AIF   (T'&COL EQ 'O').C1                                               
&TAG.4   DC    AL1(LQ_RAWDQ),AL2(&TAG.5-&TAG.4),X'0003',AL1(LD_CHARQ)           
         DC    C'&COL'                                                          
                                                                                
&TAG.5   EQU   *                                                                
.C1      AIF   (T'&H1 EQ 'O').C2                                                
         AIF   ('&H1'(1,1) EQ '@').C1A                                          
&CHEAD   SETC  ' '                                                              
         AIF   ('&H1'(1,5) EQ 'SPACE').SH1                                      
&CHEAD   SETC  '&H1'                                                            
.SH1     DC    AL1(LQ_RAWDQ),AL2(&TAG.6-&TAG.5),X'0005',AL1(LD_CHARQ)           
         DC    C'&CHEAD'                                                        
&TAG.6   EQU   *                                                                
         AGO   .C2                                                              
                                                                                
.C1A     DC    AL1(LQ_RAWDQ),AL2(&TAG.6-&TAG.5),X'0005',AL1(LD_CHARQ)           
&H1      DC    CL5' '                                                           
&#HEADS  SETA  &#HEADS+1                                                        
&HEADS(&#HEADS) SETC '&H1'                                                      
&TAG.6   EQU   *                                                                
                                                                                
.C2      ANOP                                                                   
         AIF   (T'&H2 EQ 'O').C4                                                
&CHEAD   SETC  ' '                                                              
         AIF   ('&H2'(1,5) EQ 'SPACE').SH2                                      
&CHEAD   SETC  '&H2'                                                            
.SH2     DC    AL1(LQ_RAWDQ),AL2(&TAG.7-&TAG.6),X'0006',AL1(LD_CHARQ)           
         DC    C'&CHEAD'                                                        
.C4      ANOP                                                                   
&TAG.7   EQU   *                                                                
         AIF   (T'&H3 EQ 'O').C6                                                
         DC    AL1(LQ_RAWDQ),AL2(&TAG.8-&TAG.7),X'0007',AL1(LD_CHARQ)           
         DC    C'&H3'                                                           
.C6      ANOP                                                                   
&TAG.8   EQU   *                                                                
         DC    AL1(LQ_RAWDQ),AL2(&TAG.9-&TAG.8),X'0008',AL1(LD_UBINQ)           
         DC    AL1(&DEC)                                                        
.C10     ANOP                                                                   
&TAG.9   EQU   *                                                                
         AIF   (T'&SUBTYPE EQ 'O').C11                                          
         DC    AL1(LQ_RAWDQ),AL2(&TAG.10-&TAG.9),X'000A',AL1(LD_CHARQ)          
         DC    C'&SUBTYPE'                                                      
         AIF   ('&SUBTYPE' EQ 'C').C11                                          
&PROP    SETA  X'00'               Sort only SUBTYPE=C                          
                                                                                
.C11     ANOP                                                                   
         AIF   (T'&HIDE EQ 'O').C12                                             
         AIF   ('&HIDE' NE 'Y').C12                                             
&PROP    SETA  &PROP+X'80'                                                      
                                                                                
.C12     ANOP                                                                   
         AIF   (T'&NOFILT EQ 'O').C13                                           
         AIF   ('&NOFILT' NE 'Y').C13                                           
&PROP    SETA  &PROP+X'40'         No show in quick filt                        
                                                                                
.C13     ANOP                                                                   
&TAG.10  DC    AL1(LQ_RAWDQ),AL2(&TAG.11-&TAG.10),X'000C',AL1(LD_UBINQ)         
         DC    AL1(&PROP)                                                       
&TAG.11  EQU   *                                                                
         AIF   (T'&GROUP EQ 'O').C14                                            
         AIF   (T'&GLEVEL EQ 'O').C14                                           
         DC    AL1(LQ_RAWDQ),AL2(&TAG.12-&TAG.11),X'0011',AL1(LD_CHARQ)         
         DC    C'00&GROUP'                                                      
         DC    C'00&GLEVEL'                                                     
&TAG.12  EQU   *                                                                
.C14     ANOP                                                                   
.*                                                                              
.X       ANOP                                                                   
         DC    X'00'                                                            
&TAG.X   EQU   *                                                                
         MEXIT                                                                  
         MEND                                                                   
         EJECT                                                                  
***********************************************************************         
* Actual Report definition                                            *         
***********************************************************************         
                                                                                
REPTABLE DS    0H                                                               
                                                                                
FE11     DC    AL2(FE11X-*),AL2(0)                                              
FE11A    DC    AL1(LQ_DLDDQ),AL2(FE11B-FE11A),X'FE11'                           
FE11B    DC    AL1(LQ_RAWDQ),AL2(FE11C-FE11B),X'0001',AL1(LD_CHARQ)             
         DC    C'REPORT'                                                        
FE11C    DC    AL1(LQ_RAWDQ),AL2(FE11D-FE11C),X'0002',AL1(LD_CHARQ)             
         DC    C'SCRIBE'                                                        
FE11D    DC    AL1(0)                                                           
FE11X    EQU   *                                                                
                                                                                
FE12     DC    AL2(FE12X-*),AL2(0)                                              
FE12A    DC    AL1(LQ_DLDDQ),AL2(FE12B-FE12A),X'FE12'                           
FE12B    DC    AL1(LQ_RAWDQ),AL2(FE12C-FE12B),X'0001',AL1(LD_CHARQ)             
         DC    C'REPORT'                                                        
FE12C    DC    AL1(LQ_RAWDQ),AL2(FE12D-FE12C),X'0002',AL1(LD_CHARQ)             
         DC    C'XAGY'                                                          
FE12CFMT DC    C' '                                                             
FE12D    DC    AL1(LQ_RAWDQ),AL2(FE12E-FE12D),X'0003',AL1(LD_CHARQ)             
         DC    C'Agency Summary'                                                
FE12E    DC    AL1(LQ_RAWDQ),AL2(FE12F-FE12E),X'0004',AL1(LD_CHARQ)             
         DC    C'001Media'                                                      
FE12F    DC    AL1(LQ_RAWDQ),AL2(FE12G-FE12F),X'0004',AL1(LD_CHARQ)             
         DC    C'002Advertiser'                                                 
FE12G    DC    AL1(LQ_RAWDQ),AL2(FE12Z-FE12G),X'0004',AL1(LD_CHARQ)             
         DC    C'003MOS'                                                        
FE12Z    DC    AL1(0)                                                           
FE12X    EQU   *                                                                
                                                                                
* These generate FE31 objects                                                   
*                                                                               
*                                                                               
                                                                                
REP00    RDEF  NODEID=8,MAPID=MAPSYSCD,TYPE=R,ALIGN=L,ROW=1,COL=1,     +        
               H1=System,SUBTYPE=C,GROUP=1,GLEVEL=1                             
REP01    RDEF  NODEID=8,MAPID=MAPSYSNM,TYPE=R,ALIGN=L,ROW=1,COL=2,     +        
               SUBTYPE=N                                                        
REP02    RDEF  NODEID=8,MAPID=MAPMEDCD,TYPE=R,ALIGN=L,ROW=2,COL=1,     +        
               H1=Media,SUBTYPE=C,GROUP=1,GLEVEL=2                              
REP03    RDEF  NODEID=8,MAPID=MAPMEDNM,TYPE=R,ALIGN=L,ROW=2,COL=2,     +        
               SUBTYPE=N                                                        
REP04    RDEF  NODEID=8,MAPID=MAPMOFCD,TYPE=R,ALIGN=L,ROW=3,COL=1,     +        
               H1='Media Office',SUBTYPE=C                                      
REP05    RDEF  NODEID=8,MAPID=MAPCLTCD,TYPE=R,ALIGN=L,ROW=4,COL=1,     +        
               H1=Client,SUBTYPE=C,GROUP=2,GLEVEL=1                             
REP06    RDEF  NODEID=8,MAPID=MAPCLTNM,TYPE=R,ALIGN=L,ROW=4,COL=2,     +        
               SUBTYPE=N                                                        
REP07    RDEF  NODEID=8,MAPID=MAPAOFCD,TYPE=R,ALIGN=L,ROW=5,COL=1,     +        
               H1='Acc Office',SUBTYPE=C                                        
REP08    RDEF  NODEID=8,MAPID=MAPPRDCD,TYPE=R,ALIGN=L,ROW=6,COL=1,     +        
               H1=Product,SUBTYPE=C,GROUP=2,GLEVEL=2                            
REP09    RDEF  NODEID=8,MAPID=MAPPRDNM,TYPE=R,ALIGN=L,ROW=6,COL=2,     +        
               SUBTYPE=N                                                        
REP10    RDEF  NODEID=8,MAPID=MAPESTCD,TYPE=R,ALIGN=L,ROW=7,COL=1,     +        
               H1=Estimate,SUBTYPE=C,GROUP=2,GLEVEL=3                           
REP11    RDEF  NODEID=8,MAPID=MAPESTNM,TYPE=R,ALIGN=L,ROW=7,COL=2,     +        
               SUBTYPE=N                                                        
REP12    RDEF  NODEID=8,MAPID=MAPESTTY,TYPE=R,ALIGN=L,ROW=7,COL=3,     +        
               SUBTYPE=O                                                        
REP13    RDEF  NODEID=8,MAPID=MAPMOSCD,TYPE=G,ALIGN=L,ROW=8,COL=1,     +        
               H1=MOS,HIDE=Y,SUBTYPE=C,NOFILT=Y                                 
REP14    RDEF  NODEID=8,MAPID=MAPMOSNM,TYPE=R,ALIGN=L,ROW=8,COL=2,     +        
               SUBTYPE=N,GROUP=3,GLEVEL=1                                       
                                                                                
REP27    RDEF  NODEID=8,MAPID=MAPORDI,TYPE=C,COL=1,H2=Ins.Mon,DEC=2,   +        
               SUBTYPE=A,H1=@H1ORDI                                             
REP20    RDEF  NODEID=8,MAPID=MAPORDR,TYPE=C,COL=2,H2=Ordered,DEC=2,   +        
               SUBTYPE=A,H1=@H1ORD                                              
REP21    RDEF  NODEID=8,MAPID=MAPPAID,TYPE=C,COL=3,H2=Cleared,DEC=2,   +        
               SUBTYPE=A,H1=@H1CLR                                              
REP22    RDEF  NODEID=8,MAPID=MAPUNPD,TYPE=C,COL=4,H2=Uncleared,DEC=2, +        
               SUBTYPE==A16-A17,H1=@H1UNC                                       
REP23    RDEF  NODEID=8,MAPID=MAPBLLD,TYPE=C,COL=5,H2=Billed,DEC=2,    +        
               SUBTYPE=A,H1=@H1BIL                                              
REP24    RDEF  NODEID=8,MAPID=MAPUNBD,TYPE=C,COL=6,H2=Billable,DEC=2,  +        
               SUBTYPE==A16-A19,H1=@H1UB1                                       
REP25    RDEF  NODEID=8,MAPID=MAPUNBD,TYPE=C,COL=6,H2=Assigned,DEC=2,  +        
               SUBTYPE=A,H1=@H1OAS                                              
REP26    RDEF  NODEID=8,MAPID=MAPUNB2,TYPE=C,COL=7,H2=Billable,DEC=2,  +        
               SUBTYPE=A,H1=@H1UB2                                              
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
                                                                                
FE61     DC    AL2(FE61X-*),AL2(0)                                              
FE61A    DC    AL1(LQ_DLDDQ),AL2(FE61B-FE61A),X'FE61'                           
FE61B    DC    X'00'                                                            
FE61X    EQU   *                                                                
         DC    AL1(EOT)            End of table                                 
         EJECT                                                                  
***********************************************************************         
* Macro for Generated headings                                        *         
***********************************************************************         
                                                                                
         MACRO                                                                  
&TAG     HTAB                                                                   
         LCLA  &COUNT                                                           
         GBLA  &#HEADS                                                          
         GBLC  &HEADS(8)                                                        
&TAG     DS    0A                                                               
.NEXTHD  ANOP  ,                                                                
&COUNT   SETA  &COUNT+1                                                         
         AIF   (&COUNT GT &#HEADS).HDEOF                                        
         DC    A(&HEADS(&COUNT))                                                
         AGO   .NEXTHD                                                          
.HDEOF   DC    AL1(EOT)                                                         
         MEXIT                                                                  
         MEND                                                                   
                                                                                
HEADTAB  HTAB  ,                   Net/Gross heading fix table                  
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*OUTBLK*'                                                    
OUTBLKL  EQU   (8*K)                                                            
OUTBLK   DC    (OUTBLKL)X'00'                                                   
                                                                                
         DS    0D                                                               
         DC    CL8'*WKBUFF*'                                                    
WKBUFF   DC    (14*K)X'00'                                                      
         EJECT                                                                  
MONTABD  DSECT                     ** Month table **                            
MONTYYMM DS    CL4                 Input YYMM                                   
MONTMONS DS    CL2                 Month sequence number (01-nn)                
MONTMNTH DS    CL6                 Month name                                   
MONTABL  EQU   *-MONTABD           Length of table entry                        
MONTMAXN EQU   48                  Maximum number of table entries              
                                                                                
FILEHDRD DSECT                     ** Input file header record **               
FILESYSC DS    CL1                 System code                                  
FILERCDE DS    CL5                 Report type                                  
         DS    XL22                N/D                                          
                                                                                
FILE$TYP DS    CL1                 ** Dollar type **                            
FILE$NET EQU   C'N'                .  Net   dollars                             
FILE$GRS EQU   C'G'                .  Gross dollars                             
FILEMTYP DS    CL1                 B = Print file includes orderd $             
*                                  by insertion month                           
FILEHDRL EQU   *-FILEHDRD                                                       
                                                                                
DETLRECD DSECT                     ** Input file detail record **               
DETLAGY  DS    CL2                 Agency alpha                                 
DETLMED  DS    CL1                 Media code                                   
DETLAGMD DS    XL1                 Agency/Media or Media                        
DETLCLT  DS    CL3                 Client code                                  
DETLPRD  DS    CL3                 Product code                                 
DETLEST  DS    CL3                 Estimate number                              
                                                                                
DETLTYPE DS    CL1                 Estimate type                                
DETLMDOF DS    CL2                 Media office code                            
DETLACOF DS    CL2                 Accounting office code                       
DETLCACC DS    CL3                 Client access code                           
                                                                                
DETLACAS DS    X                   ** Billable calculation **                   
DETLAACT EQU   1                   Use actual cost for billable                 
DETLAASS EQU   2                   Use assigned cost for billable               
         DS    XL15                N/D                                          
                                                                                
DETLMDNM DS    CL24                Media name                                   
DETLCLNM DS    CL24                Client name                                  
DETLPRNM DS    CL24                Product name                                 
DETLESNM DS    CL24                Estimate name                                
                                                                                
DETLSPTR EQU   15                  Number of DETLYYMM rows for Spot             
DETLNETR EQU   12                  Number of DETLYYMM rows for Net              
DETLPRTR EQU   21                  Number of DETLYYMM rows for Print            
                                                                                
DETLYYMM DS    CL4                 Year/Month                                   
DETLORDR DS    PL8                 Ordered amount                               
DETLPAID DS    PL8                 Paid amount                                  
DETLBILL DS    PL8                 Billed amount                                
DETLYYM1 EQU   *-DETLYYMM          Row width for Spot/Print                     
                                                                                
DETLASSN DS    PL8                 Assigned amount (Network only)               
         ORG   DETLASSN                                                         
DETLORDI DS    PL8                 Ordered $ by insertion mth (Print)           
DETLYYM2 EQU   *-DETLYYMM          Row width for Net                            
                                                                                
AMTTABD  DSECT                     ** Amounts table **                          
AMTMAP#  DS    AL2                 Map code number                              
                                                                                
AMTIND1  DS    X                   ** Indicators **                             
AMTIHIDE EQU   X'01'               .  Hide amount                               
                                                                                
AMTCURR$ DS    AL3                 A(Current  amount)                           
AMTPREV$ DS    PL(PACKLENQ)        Previous amount                              
AMTTABL  EQU   *-AMTTABD                                                        
                                                                                
BSXELD   DSECT                                                                  
BSXEL    DS    X                                                                
BSXELQ   EQU   X'F8'                                                            
BSXLN    DS    XL2                                                              
BSXDATE  DS    XL3                 YYMMDD                                       
BSXTIME  DS    XL3                 HHMMSS                                       
BSXLNQ   EQU   *-BSXELD                                                         
                                                                                
**********************************************************************          
* Included books                                                     *          
**********************************************************************          
         PRINT OFF                                                              
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE GEGENARC                                                       
       ++INCLUDE DDLINKD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREPAS02 02/01/21'                                      
         END                                                                    
