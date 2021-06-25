*          DATA SET TALNK01    AT LEVEL 002 AS OF 10/30/19                      
*PHASE T70401A                                                                  
*INCLUDE WRKIO                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE TALDCPTR                                                               
*INCLUDE TINVCON                                                                
TALNK01  TITLE '- TALENT SYSTEM SERVER SUPPORT ROUTINES 1'                      
TALNK01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TL01**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA & SAVED W/S)                        
         ST    RE,ROU1RELO         SAVE MY RELOCATION FACTOR                    
         SR    RE,RE                                                            
         SLDL  RE,8                BRANCH INDEX HELD IN HOB RF                  
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          ENSURE GOOD INDEX VALUE                      
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                ROUTINE NOT DEFINED                          
         AR    RF,RB               RF=A(ROUTINE)                                
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=TEMPORARY W/S AMOUNT                      
         BZR   RF                                                               
                                                                                
         AHI   R5,7                ROUND AMOUNT TO DOUBLEWORDS                  
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               ACQUIRE STORAGE FROM W/S POOL                
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               AND CLEAR IT                                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                                                               
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
ROUTAB   DS    0XL4                                                             
         DC    AL2(WRKINI-TALNK01),AL2(0)                                       
         DC    AL2(IOEXEC-TALNK01),AL2(IOWORKL)                                 
         DC    AL2(NXTREC-TALNK01),AL2(0)                                       
         DC    AL2(SAVPTRS-TALNK01),AL2(0)                                      
         DC    AL2(UPDPTRS-TALNK01),AL2(0)                                      
         DC    AL2(ADDERRS-TALNK01),AL2(0)                                      
         DC    AL2(CHKFLD-TALNK01),AL2(0)                                       
         DC    AL2(CHKSTAT-TALNK01),AL2(0)                                      
         DC    AL2(VALSTF-TALNK01),AL2(0)                                       
         DC    AL2(SETELEN-TALNK01),AL2(0)                                      
         DC    AL2(CVTINV-TALNK01),AL2(0)                                       
         DC    AL2(ADDREC-TALNK01),AL2(0)                                       
         DC    AL2(ADDTAAC-TALNK01),AL2(0)                                      
         DC    AL2(ADDWID-TALNK01),AL2(0)                                       
         DC    AL2(OUTERR-TALNK01),AL2(0)                                       
         DC    AL2(INITERR-TALNK01),AL2(0)                                      
         DC    AL2(TRNSAGT-TALNK01),AL2(0)                                      
         DC    AL2(OUTTIME-TALNK01),AL2(0)                                      
         DC    AL2(FMTLIST-TALNK01),AL2(0)                                      
         DC    AL2(UPDOAP-TALNK01),AL2(0)                                       
         DC    AL2(PUTREC-TALNK01),AL2(0)                                       
         DC    AL2(ADDFTRK-TALNK01),AL2(0)                                      
         DC    AL2(TSTCLCK-TALNK01),AL2(0)                                      
         DC    AL2(GETELEM-TALNK01),AL2(0)                                      
         DC    AL2(ADDGERRS-TALNK01),AL2(0)                                     
         DC    AL2(VALFLD-TALNK01),AL2(0)                                       
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALIZE WORKING STORAGE VARIABLES                                *         
***********************************************************************         
                                                                                
WRKINI   J     *+12                                                             
         DC    CL8'*WRKINI*'                                                    
         LR    RB,RF                                                            
         USING WRKINI,RB                                                        
         LA    R0,GLVALUES         MOVE LITERALS TO W/S                         
         LHI   R1,GLVALUEL                                                      
         LA    RE,LVALUES                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R1,RELOLST          RELOCATE ADCONS                              
         LHI   R0,RELOLSTN                                                      
         BASR  RE,0                                                             
         L     RF,0(R1)                                                         
         A     RF,ROU1RELO                                                      
         ST    RF,0(R1)                                                         
         AHI   R1,L'RELOLST                                                     
         BCTR  R0,RE                                                            
                                                                                
         LHI   RF,IOAREA1-WORKD    SET ADDRESSES OF I/O AREAS                   
         LA    RF,WORKD(RF)                                                     
         LHI   R0,AIONM                                                         
         LA    R1,AIO1                                                          
         BASR  RE,0                                                             
         ST    RF,0(R1)                                                         
         AHI   RF,IOLENQ                                                        
         AHI   R1,L'AIO1                                                        
         BCTR  R0,RE                                                            
                                                                                
         L     R1,AFACTAB                                                       
         USING FACTABD,R1          EXTRACT COMFACS ADCONS                       
         LHI   R0,FACTABN                                                       
         BASR  RE,0                                                             
         SR    R2,R2                                                            
         ICM   R2,3,FACTDOUT                                                    
         LA    R2,WORKD(R2)                                                     
         SR    R3,R3                                                            
         ICM   R3,3,FACTDIN                                                     
         A     R3,ACOMFACS                                                      
         MVC   0(4,R2),0(R3)                                                    
         AHI   R1,FACTABL                                                       
         BCTR  R0,RE                                                            
                                                                                
         L     R2,ACORPHS          R2=A(CORE PHASE LIST)                        
         LA    R3,APHASES          R3=A(CORE PHASE ADDRESS LIST)                
         LHI   R4,CORPHSN          R4=CORE PHASE COUNT                          
         SR    R0,R0                                                            
         ICM   R0,14,T00A                                                       
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
WRKINI02 IC    R0,0(R2)                                                         
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         AHI   R2,1                BUMP TO THE NEXT ENTRY                       
         AHI   R3,L'APHASES        BUMP TO NEXT ADDRESS                         
         BCT   R4,WRKINI02         DO FOR NUMBER OF PHASES                      
                                                                                
         L     R1,ALP              SET A(SECRET CONTROL BLOCK)                  
         LA    R0,TWAD                                                          
         AHI   R0,SVSECRET-TWAD                                                 
         ST    R0,LP_ASECD-LP_D(R1)                                             
                                                                                
         MVC   DATADISP,=AL2(TLRCELEM-TLRCD)                                    
                                                                                
***********************************************************************         
                                                                                
         GOTO1 VGETFACT,DMCB,0                                                  
                                                                                
         USING FACTSD,R1                                                        
         L     R1,DMCB                                                          
         CLI   FASYSID,1           SET SYSTEM VARIABLE                          
         JNE   *+8                                                              
         MVI   SYSTEM,TESTSYS                                                   
         CLI   FASYSID,11                                                       
         JNE   *+8                                                              
         MVI   SYSTEM,CSCSYS                                                    
         CLI   FASYSID,15                                                       
         JNE   *+8                                                              
         MVI   SYSTEM,FQASYS                                                    
         DROP  R1                                                               
                                                                                
***********************************************************************         
                                                                                
         USING TLSYD,R3                                                         
         LA    R3,IOKEY                                                         
         XC    TLSYKEY,TLSYKEY     READ SYSTEM KEY/RECORD                       
         MVI   TLSYCD,TLSYCDQ                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         DROP  R3                                                               
                                                                                
         USING TASYD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TASYELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVSYSTAT,TASYSTAT   SAVE SYSTEM STATUSES                         
         MVC   SVSYSTA2,TASYSTA2                                                
         J     YES                                                              
         DROP  R4,RB                                                            
                                                                                
         LTORG                                                                  
                                                                                
LVALUES  DS    0D                  ** LITERALS MOVED TO WORKD **                
         DC    V(TWABLD)                                                        
         DC    V(WRKIO)                                                         
         DC    A(FACTAB)                                                        
         DC    A(CORPHS)                                                        
         DC    A(FILTAB)                                                        
         DC    A(CMDTAB)                                                        
         DC    5A(0)                                                            
         DC    CL(L'SPACES)' '                                                  
         DC    X'D9000A'                                                        
         DC    (L'EFFS)AL1(FF)                                                  
LVALUESL EQU   *-LVALUES                                                        
         EJECT                                                                  
***********************************************************************         
* I/O EXECUTIVE                                                       *         
***********************************************************************         
                                                                                
IOEXEC   J     *+12                                                             
         DC    CL8'*IOEXEC*'                                                    
         LR    RB,RF                                                            
         USING IOEXEC,RB                                                        
         USING IOWORKD,RC          RC=A(LOCAL WORKING STORAGE)                  
         ST    R1,IOCTRL           SAVE I/O CONTROL BYTES IN SYSCOND            
         MVI   IOQ,0               ESTABLISH COMMAND QUALIFIER                  
         TM    IOCTRL+3,IOLOCK     TEST READ-FOR-UPDATE                         
         BZ    *+8                                                              
         OI    IOQ,X'80'                                                        
         TM    IOCTRL+3,IORDEL     TEST DELETED RECORDS WANTED                  
         BZ    *+8                                                              
         OI    IOQ,X'08'                                                        
         LH    R1,=AL2(IOALL)      ESTABLISH I/O AREA ADDRESS                   
         N     R1,IOCTRL                                                        
         BZ    IOEX02                                                           
         SRL   R1,12               R1=I/O AREA NUMBER                           
         BCTR  R1,0                                                             
         MHI   R1,IOLENQ                                                        
         A     R1,AIO1                                                          
         STCM  R1,15,IOADDR        SET ADDRESS OF I/O AREA                      
IOEX02   LHI   R1,IOFILES          ESTABLISH FILE                               
         N     R1,IOCTRL                                                        
         BNZ   IOEX04                                                           
         OC    IOFILE,IOFILE                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      SET FILE NAME                                
         OC    IOCMND,IOCMND       FILE GIVEN - SO MUST COMMAND BE              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      SET COMMAND NAME                             
         B     IOEX20                                                           
IOEX04   SRL   R1,8                R1=FILE NUMBER                               
         L     RE,AFILTAB                                                       
         USING FILTABD,RE                                                       
IOEX06   CLI   FILNUM,0                                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLM   R1,1,FILNUM         MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         AHI   RE,FILTABL                                                       
         B     IOEX06                                                           
         MVC   IOFILV,FILNUM       EXTRACT FILE VALUES                          
         L     RE,ACMDTAB          RE=A(I/O COMMAND TABLE)                      
         USING CMDTABD,RE                                                       
         SR    RF,RF                                                            
         LHI   R1,IOCMNDS          ESTABLISH COMMAND                            
         N     R1,IOCTRL                                                        
         BNZ   IOEX08                                                           
         OC    IOCMND,IOCMND       NOT GIVEN - TEST COMMAND NAMED               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     IOEX20                                                           
IOEX08   CLI   CMDFILT,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),CMDFILT                                                 
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),CMDFILT                                                 
         BNE   *+12                                                             
         LA    RE,CMDNTRY                                                       
         B     IOEX10                                                           
         SR    RF,RF                                                            
         ICM   RF,3,CMDTABL                                                     
         AR    RE,RF                                                            
         B     IOEX08                                                           
         USING CMDNTRY,RE          RE=A(COMMAND TABLE ENTRY)                    
IOEX10   CLI   CMDNTRY,0                                                        
         BNE   *+6                                                              
         DC    H'0'                INVALID COMMAND                              
         CLM   R1,1,CMDNUM         MATCH ON COMMAND NUMBER                      
         BE    *+12                                                             
         AHI   RE,CMDNTRYL                                                      
         B     IOEX10                                                           
         MVC   IOCMDV,CMDNAME      EXTRACT COMMAND VALUES                       
                                                                                
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     TEST ADDREC                                  
         BO    IOEX14                                                           
         TM    IOCMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IOEX16                                                           
         ICM   R1,15,IOADDR        FIND THIS I/O AREA DA/WORK                   
         BZ    IOEX12                                                           
         AHI   R1,IODDWQ           DISPLACE TO DA/WORK IN I/O AREA              
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    0(L'IODA,R1),0(R1)                                               
         OC    0(L'IODA,R1),0(R1)                                               
         BZ    *+10                                                             
         MVC   IODA,0(R1)          YES - SET D/A                                
         AHI   R1,L'IODA                                                        
         OC    0(L'IOWORK,R1),0(R1)                                             
         BZ    *+10                                                             
         MVC   IOWORK,0(R1)        YES - SET WORK                               
                                                                                
IOEX12   OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   IOEX14                                                           
         TM    IOFILI,FILIIS       TEST THIS IS A D/A FILE                      
         BNZ   *+14                                                             
         TM    IOFILI2,FILIDI      AND THAT AN I/S FILE IS ATTACHED             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(4),IOCTRL                                                  
         NI    IODUB+2,X'F0'       TURN-OFF FILE INDICATORS                     
         L     R0,IODUB                                                         
         SR    R1,R1                                                            
         IC    R1,IOFILN2                                                       
         SLL   R1,8                                                             
         OR    R1,R0                                                            
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    IOEX14              SUCCESSFUL I/O                               
         BL    IOEXX               EXIT ON BAD I/S ERRORS                       
         TM    IOERR,IOERNF        TEST RECORD-NOT-FOUND                        
         BNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    IOEXX                                                            
         OC    IODA,IODA           TEST DISK ADDRESS SET                        
         BNZ   *+6                                                              
         DC    H'0'                SOMETHING BAD HAPPENED                       
IOEX14   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK,   *        
               IOBRDLST                                                         
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         GOTOR IOTRCE,IOTRFIL                                                   
         ICM   R1,15,IOADDR        PARK DA/WORK FOR THIS I/O AREA               
         BZ    IOEXX                                                            
         AHI   R1,IODDWQ           DISPLACE TO DA/WORK IN I/O AREA              
         MVC   0(L'IODA,R1),IODA                                                
         MVC   L'IODA(L'IOWORK,R1),IOWORK                                       
         B     IOEXX               EXIT TO CALLER                               
                                                                                
IOEX16   TM    IOFILI,FILIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IOEX20                                                           
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IOFILI,FILIVL                                                    
         BZ    IOEX18                                                           
         ICM   R0,15,IOADDR        VL I/S READS INTO IOAREA ADDRESS             
         BNZ   IOEX18                                                           
         DC    H'0'                                                             
                                                                                
IOEX18   GOTOR ,IOCB,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                           
         GOTOR IOTRCE,IOTRDIR+IOTRBEF                                           
         GOTOR VDATAMGR,IOCB                                                    
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         GOTOR IOTRCE,IOTRDIR+IOTRAFT                                           
         TM    IOERR,IOERRS        TEST ANY ERRORS FOUND                        
         BZ    *+12                                                             
         TM    IOERR,IOEDEL        TEST DELETED RECORD FOUND                    
         BZ    IOEXX               NO - EXIT WITH ERROR                         
         TM    IOFILI2,FILIID      TEST D/A FILE ATTCHED TO THIS FILE           
         BZ    IOEXX               NO - EXIT                                    
         SR    R1,R1               YES - EXTRACT DISK ADDRESS                   
         IC    R1,IOFILKL                                                       
         SR    R0,R0                                                            
         IC    R0,IOFILCL                                                       
         AR    R1,R0                                                            
         LA    R1,IOKEY(R1)                                                     
         MVC   IODA,0(R1)                                                       
         ICM   R1,15,IOADDR        PARK DA FOR THIS I/O AREA                    
         BZ    IOEXX                                                            
         AHI   R1,IODDWQ           DISPLACE TO DA/WORK IN I/O AREA              
         MVC   0(L'IODA,R1),IODA                                                
         B     IOEXX                                                            
                                                                                
IOEX20   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                    
         MVC   IOERR,8(R1)                                                      
                                                                                
IOEXX    TM    IOERR,IOERRS        ANY ERRORS?                                  
         JZ    YES                                                              
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         JNZ   EXITH               LOGICAL ERROR                                
         J     EXITL               PHYSICAL ERROR                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT I/O TRACE                                          *         
***********************************************************************         
                                                                                
IOTRCE   CLI   IOTRACE,C' '        TEST TRACE IS ACTIVE                         
         BNHR  RE                                                               
         CLI   IOTRACE,C'N'                                                     
         BER   RE                                                               
         STC   R1,IOTRIND                                                       
         ST    RE,IOSAVERE                                                      
         MVC   IOP,SPACES                                                       
         L     RF,ALP                                                           
         L     RF,LP_ARUNP-LP_D(RF)                                             
         L     RF,RUNPMODE-RUNPARMD(RF)                                         
         L     RF,RMASTC-RUNFACSD(RF)                                           
         MVC   IOVPRNT,MCVPRINT-MASTD(RF)                                       
                                                                                
         L     RF,IOCB+0                                                        
         MVC   IOP(L'IOCMDNM),0(RF)                                             
         L     RF,IOCB+4                                                        
         MVC   IOP+L'IOCMDNM+1(L'IOFILNM),0(RF)                                 
         SR    R2,R2                                                            
         IC    R2,IOFILKL          R2=KEY LENGTH                                
         TM    IOTRIND,IOTRDIR     TEST DIRECTORY I/O                           
         BZ    IOTRCE04                                                         
         MVC   IOP+20(L'IOIKEYI),IOIKEYI                                        
         TM    IOTRIND,IOTRAFT                                                  
         BZ    IOTRCE02                                                         
         MVC   IOP+20(L'IOIKEYO),IOIKEYO                                        
         TM    IOFILI,FILIVL                                                    
         BNZ   IOTRCE02                                                         
         SR    R0,R0                                                            
         IC    R0,IOFILCL                                                       
         AR    R2,R0                                                            
         AHI   R2,4                                                             
                                                                                
IOTRCE02 BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),IOKEY                                                  
         EX    R2,*+8                                                           
         B     *+10                                                             
         TR    IOP+25(0),IOTRTTAB                                               
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
         MVC   IOP,SPACES                                                       
         GOTOR VHEXOUT,IOPARM,IOKEY,IOHEXWRK,1(R2),IOHEXSEP F                   
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),IOHEXWRK                                               
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
         MVC   IOP,SPACES                                                       
         LA    R1,IOHEXWRK+1(R2)                                                
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),0(R1)                                                  
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
         B     IOTRCEX                                                          
                                                                                
IOTRCE04 TM    IOTRIND,IOTRFIL                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         IC    R0,IOFILCL                                                       
         AR    R2,R0               ADD ON CONTROL LENGTH                        
         AHI   R2,6                PLUS L'LENGTH AND LINK AREA                  
         L     R0,IOCB+8                                                        
         GOTOR VHEXOUT,IOPARM,(R0),IOWORK,4,IOHEXTOG                            
         L     R3,IOCB+12                                                       
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),0(R3)                                                  
         EX    R2,*+8                                                           
         B     *+10                                                             
         TR    IOP+25(0),IOTRTTAB                                               
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
         MVC   IOP,SPACES                                                       
         MVC   IOP(L'IODALIT),IODALIT                                           
         MVC   IOP+L'IODALIT(8),IOWORK                                          
         GOTOR VHEXOUT,IOPARM,IOCB+8,IOP+13,1,IOHEXTOG                          
         GOTOR VHEXOUT,IOPARM,(R3),IOHEXWRK,1(R2),IOHEXSEP                      
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),IOHEXWRK                                               
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
         MVC   IOP,SPACES                                                       
         LA    R1,IOHEXWRK+1(R2)                                                
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   IOP+25(0),0(R1)                                                  
         GOTOR IOVPRNT,IOPARM,IOP-1,IOBL01                                      
                                                                                
IOTRCEX  L     RE,IOSAVERE                                                      
         BR    RE                                                               
         DROP  RB,RC                                                            
         EJECT                                                                  
IOHEXSEP DC    C'SEP'                                                           
IOHEXTOG DC    C'TOG'                                                           
IOBL01   DC    C'BL01'                                                          
IOIKEYI  DC    C'IKEY='                                                         
IOIKEYO  DC    C'OKEY='                                                         
IODALIT  DC    C'D/A='                                                          
                                                                                
IOTRTTAB DS    0XL256                                                           
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
                                                                                
         LTORG                                                                  
                                                                                
IOWORKD  DSECT                     ** IOEXEC S/R LOCAL W/S **                   
IODUB    DS    D                                                                
IOCB     DS    6F                                                               
IOPARM   DS    6F                                                               
IOSAVERE DS    A                                                                
IOVPRNT  DS    A                   V(PRINT)                                     
IOCTRL   DS    XL4                 I/O COMMAND WORD                             
IOQ      DS    X                   I/O COMMAND QUALIFIER (RFU/DELETES)          
IOFILV   DS    0XL15               EXTRACTED FILE VALUES (THIS I/O)             
IOFILNO  DS    X                   FILE NUMBER                                  
IOFILNM  DS    CL7                 COMMAND NAME                                 
IOFILI   DS    X                   FILE INDICATORS - 1                          
IOFILI2  DS    X                   FILE INDICATORS - 2                          
IOFILN2  DS    X                   FILE NUMBER 2 (I/S D/A PAIR)                 
IOFILKL  DS    X                   KEY LENGTH                                   
IOFILCL  DS    X                   CONTROL LENGTH                               
IOFILDE  EQU   IOFILCL             DISPLACEMENT TO FIRST ELEMENT                
IOFILML  DS    XL2                 MAXIMUM RECORD LENGTH                        
IOTRIND  DS    XL1                 TRACE INDICATORS                             
IOTRDIR  EQU   X'40'               I/O TO DIRECTORY                             
IOTRFIL  EQU   X'20'               I/O TO FILE                                  
IOTRBEF  EQU   X'01'               BEFORE I/O                                   
IOTRAFT  EQU   X'02'               AFTER I/O                                    
IOCMDV   DS    0XL10               EXTRACTED COMMAND VALUES (THIS I/O)          
IOCMDNM  DS    CL7                 COMMAND NAME                                 
IOCMDNO  DS    X                   COMMAND NUMBER                               
IOCMDI   DS    X                   COMMAND INDICATORS - 1                       
IOCMDI2  DS    X                   COMMAND INDICATORS - 2                       
IOP      DS    CL132                                                            
IOHEXWRK DS    XL220                                                            
IOWORKL  EQU   *-IOWORKD                                                        
TALNK01  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET NEXT RECORD                                                     *         
*                                                                     *         
* NOTE: READ DELETES FLAG (P3/B0) MAY HAVE FOLLOWING VALUES:-         *         
*                                                                     *         
*        X'0' - INCLUDE UNDELETED RECORDS ONLY                        *         
*        C'O' - INCLUDE DELETED RECORDS ONLY                          *         
*        ANY OTHER VALUE - INCLUDE DELETED AND UNDELETED RECORDS      *         
***********************************************************************         
                                                                                
NXTREC   J     *+12                                                             
         DC    CL8'*NXTREC*'                                                    
         LR    RB,RF                                                            
         USING NXTREC,RB                                                        
         L     R0,0(R1)            R0=A(KEY TABLE)                              
         LM    R3,R6,4(R1)         R3=A(KEY SAVE AREA), R4=A(WORK SAVE)         
         LA    R3,0(R3)            R5=A(KEY FILTER ROUTINE)                     
*                                  R6=A(RECORD FILTER ROUTINE)                  
         MVC   BYTE1,8(R1)         SET READ DELETES CONTROL BYTE                
         L     R2,ALP                                                           
         USING LP_D,R2             R2=A(LP_D)                                   
         SR    RE,RE                                                            
         ICM   RE,1,4(R1)                                                       
         SLL   RE,2                                                             
         L     RE,LP_BLKS-L'LP_BLKS(RE)                                         
         ST    RE,LP_ADATA                                                      
         ST    RE,IOADDR                                                        
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME CALL                         
         BNE   NXTREC02                                                         
         MVI   LP_RMODE,LP_RNEXT   RESET FIRST TIME CALL                        
         CLI   0(R1),YESQ          TEST RECORD SET IS WANTED                    
         BNE   NXTREC06                                                         
         LTR   R3,R3               TEST RECORD KEY SAVE AREA PROVIDED           
         BZ    *+10                                                             
         MVC   0(L'IOKEY,R3),IOKEY YES - SAVE CURRENT RECORD KEY                
         XC    IOKEY,IOKEY                                                      
NXTREC02 GOTOR LP_ASETK,DMCB,(0,(R0)),IOKEY,(R4),('FF',LP_D)                    
         BH    NXTREC04                                                         
         LHI   R1,IOHI+IODIR                                                    
         CLI   BYTE1,0             TEST CALLER WANTS DELETED RECORDS            
         BE    *+8                                                              
         AHI   R1,IORDEL           YES - SET CONTROL BIT                        
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   BYTE1,C'O'          TEST WANT DELETED RECORDS ONLY               
         BNE   *+12                                                             
         TM    IOERR,IOEDEL        YES - THEN RECORD MUST BE DELETED            
         BZ    NXTREC02                                                         
         GOTOR LP_ASETK,DMCB,(1,(R0)),IOKEY,(R4),('FF',LP_D)                    
         BNE   NXTREC02                                                         
         LTR   RF,R5               TEST/SET DIRECTORY FILTER ROUTINE            
         BZ    *+10                                                             
         BASR  RE,RF                                                            
         BNE   NXTREC02                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IORDEL'                           
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LTR   RF,R6               SET/TEST FILE FILTER ROUTINE                 
         JZ    YES                                                              
         L     R1,IOADDR           PASS A(RECORD) IN R1                         
         BASR  RE,RF                                                            
         BNE   NXTREC02            DIDN'T PASS FILTERS - GET NEXT               
         J     YES                                                              
NXTREC04 LTR   R3,R3               TEST ANY KEY SAVED                           
         BZ    NXTREC06                                                         
         MVC   IOKEY,0(R3)         YES - RESTORE IT                             
NXTREC06 MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS TO COME                  
         J     NO                  EXIT WITH CC=NOT EQUAL TO CALLER             
         DROP  R2,RB                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO BUILD BLOCK OF ORIGINAL PASSIVE POINTERS          *         
*        ON ENTRY ... IOADDR=A(ORIGINAL RECORD)                       *         
***********************************************************************         
                                                                                
SAVPTRS  J     *+12                                                             
         DC    CL8'*SAVPTR*'                                                    
         LR    RB,RF                                                            
         USING SAVPTRS,RB                                                       
                                                                                
         L     R3,ASVPTRS          R3=A(SAVED POINTER BLOCK)                    
         BRAS  RE,BLDPBLK          BUILD POINTER BLOCK                          
         J     XIT                                                              
         DROP  RB                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO UPDATE POINTERS                                   *         
*        ON ENTRY ... IOADDR=A(UPDATED RECORD)                        *         
***********************************************************************         
                                                                                
UPDPTRS  J     *+12                                                             
         DC    CL8'*UPDPTR*'                                                    
         LR    RB,RF                                                            
         USING UPDPTRS,RB                                                       
                                                                                
         LA    R4,IOKEY            R4=A(KEY)                                    
         L     R2,ASVPTRS          R2=A(SAVED POINTER BLOCK)                    
                                                                                
         USING TLRCD,RE                                                         
         L     RE,IOADDR                                                        
         MVC   HALF1,=AL2(IODIR)   SET DIRECTORY EQUATE                         
         CLI   0(RE),TLCKCDQ                                                    
         JNE   *+10                                                             
         MVC   HALF1,=AL2(IOCHKDIR)                                             
                                                                                
         TM    TLRCSTAT,X'80'      IF RECORD IS BEING DELETED                   
         JZ    UPTRS10                                                          
         BAS   RE,DELKEYS          JUST DELETE SAVED KEYS                       
         J     XIT                                                              
         DROP  RE                                                               
                                                                                
UPTRS10  L     R3,AUPPTRS          R3=A(UPDATED POINTER BLOCK)                  
         BRAS  RE,BLDPBLK          BUILD UPDATED POINTER BLOCK                  
                                                                                
         BAS   RE,KEYCOMP          DO NOT UPDATE UNCHANGED KEYS                 
                                                                                
         MVI   0(R3),X'FF'         NEVER ADD NEW PRIMARY KEY HERE               
                                                                                
         BAS   RE,DELKEYS          DELETE CHANGED SAVED KEYS                    
                                                                                
         BAS   RE,ADDKEYS          ADD CHANGED UPDATED KEYS                     
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO COMPARE KEYS IN SAVED BLOCK VERSUS UPDATED BLOCK  *         
*        IF KEY HAS NOT CHANGED, MARK IT WITH X'FF' TO BE IGNORED     *         
*        ON ENTRY ... R2=A(SAVED POINTER BLOCK)                       *         
***********************************************************************         
                                                                                
KEYCOMP  NTR1                                                                   
KCOMP10  CLI   0(R2),0             WHEN END OF SAVED KEYS REACHED,              
         JE    XIT                 NO MORE EDITING NECESSARY                    
                                                                                
         L     R3,AUPPTRS          R3=A(UPDATED POINTER BLOCK)                  
KCOMP20  CLI   0(R3),0             WHEN END OF UPDATED KEYS REACHED,            
         JE    KCOMP50             BUMP TO NEXT SAVED KEY                       
         BAS   RE,SETLKEY                                                       
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   0(0,R2),0(R3)       IF SAVED POINTER MATCHES                     
         JNE   KCOMP40             UPDATED POINTER                              
KCOMP30  MVI   0(R2),X'FF'         NO NEED TO UPDATE IT                         
         MVI   0(R3),X'FF'                                                      
         J     KCOMP50                                                          
                                                                                
KCOMP40  LA    R3,L'TLDRREC(R3)    BUMP TO NEXT UPDATED KEY                     
         J     KCOMP20                                                          
                                                                                
KCOMP50  LA    R2,L'TLDRREC(R2)    BUMP TO NEXT SAVED KEY                       
         J     KCOMP10                                                          
                                                                                
***********************************************************************         
*        ROUTINE TO DELETE SAVED KEYS THAT HAVE CHANGED               *         
*        ON ENTRY ... R2=A(SAVED POINTER BLOCK)                       *         
*                     R4=A(KEY)                                       *         
***********************************************************************         
                                                                                
         USING TLDRD,R4                                                         
DELKEYS  NTR1                                                                   
DKEYS10  CLI   0(R2),0             WHEN END OF SAVED KEYS REACHED,              
         JE    XIT                 NO MORE DELETING NECESSARY                   
         CLI   0(R2),X'FF'         IF KEY SHOULD NOT BE DELETED,                
         JE    DKEYS20             SKIP IT                                      
                                                                                
         XC    TLDRKEY,TLDRKEY                                                  
         BAS   RE,SETLKEY                                                       
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   TLDRKEY(0),0(R2)    READ SAVED KEY FOR UPDATE                    
         LH    R1,HALF1                                                         
         AHI   R1,IOHIUP                                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BAS   RE,SETLKEY                                                       
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   TLDRKEY(0),0(R2)    KEY MUST BE FOUND                            
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         OI    TLDRSTAT,X'80'      MARK IT DELETED AND WRITE IT BACK            
         LH    R1,HALF1                                                         
         AHI   R1,IOWRITE                                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
                                                                                
DKEYS20  LA    R2,L'TLDRREC(R2)    BUMP TO NEXT SAVED KEY                       
         J     DKEYS10                                                          
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO ADD UPDATED KEYS                                  *         
*        ON ENTRY ... R3=A(UPDATED POINTER BLOCK)                     *         
*                     R4=A(KEY)                                       *         
***********************************************************************         
                                                                                
         USING TLDRD,R4                                                         
ADDKEYS  NTR1                                                                   
         LR    R2,R3               R2=A(UPDATED POINTER BLOCK)                  
                                                                                
AKEYS10  CLI   0(R2),0             WHEN END OF UPDATED KEYS REACHED,            
         JE    XIT                 NO MORE ADDING/CHANGING NECESSARY            
         CLI   0(R2),X'FF'         IF KEY SHOULD NOT BE ADDED/CHANGED,          
         JE    AKEYS60             SKIP IT                                      
                                                                                
         XC    TLDRKEY,TLDRKEY                                                  
         BAS   RE,SETLKEY                                                       
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   TLDRKEY(0),0(R2)    READ UPDATED KEY FOR UPDATE                  
         LH    R1,HALF1                                                         
         AHI   R1,IOHIUPD                                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         J     AKEYS30                                                          
AKEYS20  LH    R1,HALF1                                                         
         AHI   R1,IOSQUPD                                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
AKEYS30  CLC   TLDRKEY,0(R2)                                                    
         JNE   AKEYS50                                                          
                                                                                
         TM    TLDRSTAT,X'80'      IF FOUND, IT MUST BE MARKED DELETED          
         JO    AKEYS40                                                          
         CLC   TLDRDA,IOWORK+4     OR BE POINTING TO THE SAME RECORD            
         JE    AKEYS60                                                          
         DC    H'00'                                                            
                                                                                
AKEYS40  NI    TLDRSTAT,X'FF'-X'80' MARK UNDELETED                              
         MVC   TLDRDA,IOWORK+4      AND WRITE IT BACK                           
         LH    R1,HALF1                                                         
         AHI   R1,IOWRITE                                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         J     AKEYS60                                                          
                                                                                
AKEYS50  MVC   TLDRKEY,0(R2)                                                    
         NI    TLDRSTAT,X'FF'-X'80' IF NOT FOUND, ADD KEY                       
         MVC   TLDRDA,IOWORK+4                                                  
         LH    R1,HALF1                                                         
         AHI   R1,IOADD                                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
                                                                                
AKEYS60  LA    R2,L'TLDRREC(R2)    BUMP TO NEXT UPDATED KEY                     
         J     AKEYS10                                                          
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE TO SET LENGTH OF KEY FOR COMPARES AND MOVES          *         
*        ON ENTRY ... R2=A(KEY)                                       *         
***********************************************************************         
                                                                                
         USING TLDRD,R2                                                         
SETLKEY  CLI   TLDRCD,TLINBCDQ     SPECIAL FOR INVOICE STATUS PTRS.             
         JNE   SLK10               INCLUDE STATUS BYTES IN LENGTH               
         LA    RF,TLDRSTAT+L'TLDRSTAT-TLDRD-1                                   
         BR    RE                                                               
                                                                                
SLK10    LA    RF,L'TLDRKEY-1      ELSE SET LENGTH TO FULL KEY                  
         BR    RE                  RETURN RF=LENGTH FOR EXEC. COMPARE           
         DROP  R2,RB                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO BUILD POINTER BLOCK                               *         
*        ON ENTRY ... IOADDR=A(RECORD)                                *         
*                     R3=A(POINTER BLOCK)                                       
***********************************************************************         
                                                                                
BLDPBLK  NTR1  BASE=*,LABEL=*                                                   
         XC    0(L'TLDRREC+1,R3),0(R3)                                          
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
                                                                                
         USING CPTRD,R4                                                         
         LA    R4,ELEM             PREPARE TO BUILD POINTER BLOCK               
         XC    ELEM,ELEM           BY PUTTING ADDRESSES OF ROUTINES/            
         MVC   CPDATCON,VDATCON    TABLES INTO PARAMETER LIST                   
         MVC   CPADDAY,VADDAY                                                   
         MVC   CPPROTON,VPROTON                                                 
         MVC   CPPROTOF,VPROTOFF                                                
         L     RF,TGACATS                                                       
         AR    RF,RE                                                            
         ST    RF,CPCATTAB                                                      
         L     RF,TGAYEARS                                                      
         AR    RF,RE                                                            
         ST    RF,CPYRSTAB                                                      
         L     RF,TGAUSES                                                       
         AR    RF,RE                                                            
         ST    RF,CPUSETAB                                                      
         DROP  R4,RE                                                            
                                                                                
         L     RF,=V(LDCPTR)       BUILD POINTER BLOCK                          
         A     RF,ROU1RELO                                                      
         GOTO1 (RF),DMCB,(X'C0',IOADDR),(R3),(R4),,IODA                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADD ERROR DETAILS TO ERROR TABLE                  *         
*        ON ENTRY ... P1 BYTE 0 = ERRSTAT EQUATE TO TURN ON           *         
*                     P1        = A(ERROR ENTRY TO ADD)               *         
***********************************************************************         
                                                                                
ADDERRS  J     *+12                                                             
         DC    CL8'*ADDERR*'                                                    
         LR    RB,RF                                                            
         USING ADDERRS,RB                                                       
                                                                                
         USING ERRENTD,R3                                                       
         L     R3,0(R1)            R1=A(ERROR ENTRY)                            
         L     R4,AERRTAB          R4=A(ERROR TABLE)                            
                                                                                
         CLI   EECATY,ERRCATY2     IF ERROR IS A CATEGORY 2                     
         JNE   AERR70                                                           
         CLI   IEROVER,0           AND ERRORS ARE BEING BYPASSED                
         JE    AERR50                                                           
         L     RF,IEROVER                                                       
         ZIC   RE,IEROVER                                                       
         CLC   =AL2(500),0(RF)     IF ALL ERRORS ARE BEING BYPASSED             
         JE    AERR20                                                           
AERR200  CLC   EENUMB,0(RF)        OR THIS ERROR IS BEING BYPASSED              
         JE    AERR20              SET NEGATIVE CONDITION CODE AND EXIT         
         LA    RF,2(RF)                                                         
         BCT   RE,AERR200                                                       
         J     AERR50                                                           
AERR20   LA    RE,BYPSERRS         ADD ERROR TO LIST OF BYPASSED                
AERR30   CLI   0(RE),X'FF'         ERRORS                                       
         JE    AERR40                                                           
         LA    RE,2(RE)                                                         
         J     AERR30                                                           
AERR40   MVC   0(2,RE),EENUMB                                                   
         MVI   2(RE),X'FF'                                                      
         J     NO                  AND RETURN NEGATIVE CONDITION CODE           
                                                                                
AERR50   CLI   IXEROVER,0          IF AN EXPLODED LIST OF ERRORS                
         JE    AERR70              IS PRESENT                                   
         L     RF,IXEROVER                                                      
         ZIC   RE,IXEROVER                                                      
AERR60   CLC   EENUMB,0(RF)        AND IT INCLUDES THIS ERROR                   
         JE    NO                  SET NEGATIVE CONDITION CODE AND EXIT         
         LA    RF,2(RF)                                                         
         BCT   RE,AERR60                                                        
                                                                                
AERR70   CLI   EECATY,ERRCATY3     IF THIS ERROR IS A CATEGORY 3                
         JE    AERR90              ADD IT TO BEGINNING OF TABLE                 
AERR80   CLI   0(R4),X'FF'                                                      
         JE    AERR90              ELSE, FIND NEXT OPEN SLOT                    
         ZIC   RE,0(R4)            IN ERROR TABLE AND ADD IT THERE              
         AR    R4,RE                                                            
         J     AERR80                                                           
AERR90   ZIC   R0,EELEN                                                         
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R4),ERRENTD                                                  
         DROP  R3                                                               
                                                                                
         AR    R4,R0                                                            
         MVI   0(R4),X'FF'         MARK NEW END OF ERROR TABLE                  
         OC    ERRSTAT,0(R1)       SET PROVIDED ERRSTAT EQUATE                  
         OI    ERRSTAT,ESECTRD     SET ERROR ENCOUNTERED STATUS                 
         J     YES                 AND RETURN POSITIVE CONDITION CODE           
         DROP  RB                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO COPY ELEMENT INFO INTO REQUEST MAP                *         
*        ON ENTRY ... P1 BYTE 0 = L'REQUEST MAP FIELD                 *         
*                     P1        = A(REQUEST MAP FIELD)                *         
*                     P2        = A(ELEMENT FIELD)                    *         
*                     P3        = A(ERROR ENTRY)                      *         
*                     P4 BYTE 0 = 0, CHECK IF TRIGGER FIELD CONTAINS  *         
*                                 A VALUE IN P5 LIST. IF IT DOES,     *         
*                                 CLEAR REQUEST FIELD                 *         
*                                 OR                                  *         
*                                 X'FF', CHECK IF TRIGGER FIELD       *         
*                                 CONTAINS A VALUE IN P5 LIST. IF IT  *         
*                                 DOES NOT, CLEAR REQUEST FIELD       *         
*                                 OR                                  *         
*                                 L'TRIGGER FIELD, CHECK IF TRIGGER   *         
*                                 FIELD IS CLEAR. IF IT IS, CLEAR     *         
*                                 REQUEST FIELD)                      *         
*                     P4        = A(TRIGGER FIELD FOR IMPLICIT CLEAR) *         
*                     P5        = A(LIST OF VALUES TO CHECK TRIGGER   *         
*                                   FIELD FOR)                        *         
***********************************************************************         
                                                                                
CHKFLD   J     *+12                                                             
         DC    CL8'*CHKFLD*'                                                    
         LR    RB,RF                                                            
         USING CHKFLD,RB                                                        
                                                                                
         ZICM  R2,1(R1),3          R2=A(REQUEST MAP FIELD)                      
         ZIC   R3,0(R1)            R3=L'REQUEST MAP FIELD                       
         BCTR  R3,0                                                             
                                                                                
         BAS   RE,IMPCLEAR         CHECK IF FIELD IS SHOULD BE                  
         JE    XIT                 IMPLICITY CLEARED                            
                                                                                
         L     R4,4(R1)            R4=A(ELEMENT FIELD)                          
         L     R5,8(R1)            R5=A(ERROR ENTRY)                            
         L     R6,AERRTAB          R6=A(ERROR TABLE)                            
         L     R0,IEROVER          R0=A(BYPASSED ERRORS TABLE)                  
                                                                                
         EX    R3,*+8                                                           
         J     *+10                                                             
         OC    0(0,R2),0(R2)       IF REQUEST MAP FIELD IS BLANK                
         JNZ   CHKFLD10                                                         
         EX    R3,*+8                                                           
         J     *+10                                                             
         OC    0(0,R4),0(R4)       AND ELEMENT FIELD IS NOT BLANK               
         JZ    XIT                                                              
         EX    R3,*+8                                                           
         J     *+10                                                             
         CLC   0(0,R4),SPACES      OR ALL SPACES                                
         JE    XIT                                                              
         BAS   RE,EXPCLEAR         AND FIELD IS NOT BEING EXPLICITLY            
         JE    XIT                 CLEARED                                      
         EX    R3,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),0(R4)       COPY ELEMENT FIELD INTO REQUEST              
         J     XIT                 FIELD AND EXIT                               
                                                                                
CHKFLD10 EX    R3,*+8              IF REQUEST MAP FIELD IS POPULATED            
         J     *+10                                                             
         CLC   0(0,R2),0(R4)       AND DIFFERS FROM ELEMENT FIELD               
         JE    XIT                 FORCE CONFIRMATION OF CHANGE                 
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',(R5))                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE DETERMINES IF FIELD SHOULD BE IMPLICITLY CLEARED     *         
*        AND, IF SO, CLEARS IT                                        *         
*        ON ENTRY ... R2=A(REQUEST MAP FIELD)                         *         
*                     R3=L'REQUEST MAP FIELD                          *         
*                     12(R1) = 0, CHECK IF TRIGGER FIELD CONTAINS     *         
*                                 A VALUE IN 16(R1) LIST. IF IT DOES, *         
*                                 CLEAR REQUEST FIELD                 *         
*                              OR                                     *         
*                              X'FF', CHECK IF TRIGGER FIELD CONTAINS *         
*                                     A VALUE IN 16(R1) LIST. IF IT   *         
*                                     DOES NOT, CLEAR REQUEST FIELD   *         
*                              OR                                     *         
*                              L'TRIGGER FIELD, CHECK IS TRIGGER      *         
*                              FIELD IS CLEAR. IF IT IS, CLEAR        *         
*                              REQUEST FIELD                          *         
*                     12(R1) = A(TRIGGER FIELD FOR IMPLICIT CLEAR)    *         
*                     16(R1) = A(LIST OF VALUES TO CHECK TRIGGER      *         
*                              FIELD FOR)                             *         
***********************************************************************         
                                                                                
IMPCLEAR NTR1                                                                   
         OC    12(4,R1),12(R1)     IF REQUEST FIELD DOES NOT HAVE A             
         JZ    NO                  TRIGGER, RETURN NEGATIVE CC                  
                                                                                
         L     R4,12(R1)           R4=A(TRIGGER FIELD)                          
         L     R5,16(R1)           R5=A(LIST OF VALUES)                         
                                                                                
         ZICM  R0,12(R1),1         IF CHECKING TRIGGER FIELD FOR A              
         JNZ   ICLEAR20            VALUE THAT SHOULD CLEAR THE REQUEST          
ICLEAR10 CLI   0(R5),X'FF'         FIELD ...                                    
         JE    NO                                                               
         CLC   0(1,R5),0(R4)       CLEAR REQUEST FIELD IF THAT VALUE            
         JE    ICLEAR50            IS IN THE TRIGGER FIELD AND RETURN           
         LA    R5,1(R5)            POSITIVE CONDITION CODE                      
         J     ICLEAR10            ELSE, RETURN NEGATIVE CC                     
                                                                                
ICLEAR20 CHI   R0,X'FF'            IF CHECKING TRIGGER FIELD FOR A              
         JNE   ICLEAR40            VALUE THAT SHOULD NOT CLEAR THE              
ICLEAR30 CLI   0(R5),X'FF'         REQUEST FIELD ...                            
         JE    ICLEAR50                                                         
         CLC   0(1,R5),0(R4)       CLEAR REQUEST FIELD IF THAT VALUE            
         JE    NO                  IS NOT IN THE REQUST FIELD AND               
         LA    R5,1(R5)            RETURN POSITIVE CONDITION CODE               
         J     ICLEAR30            ELSE, RETURN NEGATIVE CC                     
                                                                                
ICLEAR40 LR    RE,R0                                                            
         BCTR  RE,0                IF CHECKING WHETHER TRIGGER FIELD            
         EX    RE,*+8              IS EMPTY ...                                 
         J     *+10                IF IT IS, CLEAR REQUEST FIELD AND            
         OC    0(0,R4),0(R4)       RETURN POSITIVE CONDITION CODE               
         JNZ   NO                  ELSE, RETURN NEGATIVE CC                     
                                                                                
ICLEAR50 EX    R3,*+8                                                           
         J     *+10                                                             
         XC    0(0,R2),0(R2)                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE DETERMINES IF FIELD SHOULD BE EXPLICITLY CLEARED     *         
*        AND, IF SO, CLEARS IT                                        *         
*        ON ENTRY ... R5=A(ERROR ENTRY)                               *         
***********************************************************************         
                                                                                
         USING ERRENTD,R5                                                       
EXPCLEAR NTR1                                                                   
         CLI   ICLRMAP,0           IF NO FIELDS ARE BEING CLEARED               
         JE    NO                  RETURN NEGATIVE CONDITION CODE               
                                                                                
         ZIC   RF,ICLRMAP          RF=CLEAR MAP CODES COUNTER                   
         L     RE,ICLRMAP          RE=A(CLEAR MAP CODES ARRAY)                  
                                                                                
ECLEAR10 CLC   EEFIELD,0(RE)       IF THIS MAP CODE SHOULD NOT                  
         JE    ECLEAR20            BE CLEARED, RETURN NEGATIVE                  
         LA    RE,1(RE)            CONDITION CODE                               
         BCT   RF,ECLEAR10                                                      
         J     NO                                                               
*                                  IF THIS MAP CODE SHOULD BE CLEARED           
*                                  RETURN POSITIVE CONDITION CODE               
*                                  AND FORCE CONFIRMATION OF CHANGE             
ECLEAR20 GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',(R5))                          
         J     YES                                                              
         DROP  R5,RB                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO BUILD ELEMENT STATUS FIELD                        *         
*        ON ENTRY ... P1 BYTE 0 = YES STATUS EQUATE                   *         
*                     P1        = A(REQUEST MAP FIELD)                *         
*                     P2 BYTE 0 = NO STATUS EQUATE                    *         
*                     P2        = A(ELEMENT FIELD)                    *         
*                     P3        = A(ERROR ENTRY)                      *         
***********************************************************************         
                                                                                
CHKSTAT  J     *+12                                                             
         DC    CL8'*CHKSTA*'                                                    
         LR    RB,RF                                                            
         USING CHKSTAT,RB                                                       
                                                                                
         L     R2,0(R1)            R2=A(REQUEST MAP FIELD)                      
         L     R3,4(R1)            R3=A(ELEMENT FIELD)                          
         L     R5,8(R1)            R5=A(ERROR ENTRY)                            
                                                                                
         L     RF,ATWA                                                          
         L     R6,AERRTAB          R6=A(ERROR TABLE)                            
         L     R0,IEROVER          R0=A(BYPASSED ERRORS TABLE)                  
                                                                                
         MVI   BYTE2,C'Y'           DEFAULT ORIGINAL SETTING TO YES             
         MVC   BYTE1,0(R3)          SAVE ORIGINAL STATUS BYTE                   
                                                                                
         OC    0(1,R1),0(R1)       IF STATUS BIT MEANS YES                      
         JZ    CSTAT10                                                          
         NC    BYTE1,0(R1)          AND WAS NOT ORIGINALLY ON                   
         JNZ   CSTAT20                                                          
         MVI   BYTE2,C'N'          SET ORIGINAL STATUS TO NO                    
         J     CSTAT20                                                          
                                                                                
CSTAT10  NC    BYTE1,4(R1)         IF STATUS BIT MEANS NO                       
         JZ    CSTAT20             AND WAS ORIGINALLY ON                        
         MVI   BYTE2,C'N'          SET ORIGINAL STATUS AS NO                    
                                                                                
CSTAT20  CLI   0(R2),0             IF INPUT IS PROVIDED                         
         JE    CSTAT30                                                          
         CLC   BYTE2,0(R2)         CHECK IF IT HAS CHANGED                      
         JE    XIT                                                              
         GOTOR (#ADDERR,AADDERR),DMCB,('ESREVIW',(R5))                          
         J     XIT                                                              
                                                                                
CSTAT30  MVC   0(1,R2),BYTE2       IF INPUT IS NOT PROVIDED                     
         J     XIT                 COPY FROM ELEMENT                            
         DROP  RB                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES STAFF/PERSION ID AND SAVES AGENCY/CLIENT   *         
*        LIMITATIONS INTO WSSVR BLOCK                                 *         
*        ON ENTRY ... P1 BYTE 0 X'80' = SAVE AGENCY/CLIENT LIMITS     *         
*                     P1=A(PERSON/STAFF ID)                           *         
***********************************************************************         
                                                                                
VALSTF   J     *+12                                                             
         DC    CL8'*VALSTF*'                                                    
         LR    RB,RF                                                            
         USING VALSTF,RB                                                        
                                                                                
         MVC   BYTE1,0(R1)         BYTE1=SAVE AGY/CLI LIMITS FLAG               
         L     RF,0(R1)            RF=A(PERSON/STAFF ID)                        
                                                                                
         USING LP_D,R1                                                          
         L     R1,ALP                                                           
                                                                                
         USING TLSTD,R3                                                         
         LA    R3,IOKEY                                                         
         XC    TLSTKEY,TLSTKEY     READ FOR STAFF KEY                           
         MVI   TLSTCD,TLSTCDQ                                                   
         MVC   TLSTUSER,LP_USRID                                                
         MVC   TLSTSTAF,0(RF)                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VSTF10                                                           
         DROP  R1,R3                                                            
                                                                                
         GOTOR (#ADDERR,AADDERR),DMCB,ERSTFNFD                                  
         J     NO                  IF NOT FOUND, RETURN ERROR                   
                                                                                
VSTF10   MVI   ACBLKMAX,0          INIT # OF SAVED AGY/CLI LIMIT BLKS           
                                                                                
         TM    BYTE1,X'80'         IF FOUND AND SAVING AGENCY/CLIENT            
         JZ    YES                 LIMITS                                       
         LHI   R0,1                INIT CURRENT AGY/CLI LIMIT BLK               
                                                                                
VSTF20   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TAVAD,R1                                                         
         L     R1,IOADDR          R1=A(STAFF RECORD)                            
         AHI   R1,TLRCELEM-TLRCD                                                
VSTF30   CLI   0(R1),0            FIND FIRST AGENCY/CLIENT LIMIT                
         JE    YES                ELEMENT                                       
         CLI   0(R1),TAVAELQ                                                    
         JE    VSTF40                                                           
         MVI   0(R1),X'FF'        SET ALL OTHERS FOR DELETION                   
         ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VSTF30                                                           
                                                                                
VSTF40   OC    TAVAAGY,TAVAAGY    IF STAFF HAS NO LIMITS, THEN HAS              
         JZ    YES                ACCESS TO ALL (NO NEED TO SV WSSVR)           
         DROP  R1                                                               
                                                                                
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',IOADDR),0                   
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WSSVRBLK                                                      
         MVC   FAWSTOKN(3),=CL3'STF'                                            
         STC   R0,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSASVE                                                
         MVC   FAWSLEN,=H'1960'    SAVE STAFF RECORD INTO WSSVR AREA            
         L     RE,IOADDR                                                        
         LA    RE,TLRCELEM-TLRCD(RE)                                            
         ST    RE,FAWSADR                                                       
         GOTO1 VWSSVR,(R1)                                                      
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         STC   R0,ACBLKMAX         SET # OF SAVED AGY/CLI LIMIT BLKS            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         CLC   IOKEY(TLSTSSEQ-TLSTD),IOKEYSAV                                   
         JNE   YES                 IF MORE STAFF RECORDS EXIST                  
         AHI   R0,1                SET TO ADD NEXT WSSVR BLOCK                  
         J     VSTF20                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
ERSTFNFD DC    AL1(ESTFNFDX-*),AL2(501),AL1(ERRCATY3),AL1(D#W4STF)              
         DC    C'Staff record is not on file'                                   
ESTFNFDX EQU   *                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SAVE ELEMENT LENGTH INTO ELEMENT                  *         
***********************************************************************         
                                                                                
SETELEN  J     *+12                                                             
         DC    CL8'*SETELE*'                                                    
         LR    RB,RF                                                            
         USING SETELEN,RB                                                       
                                                                                
         LA    RE,ELEM+3           RE=A(4TH BYTE OF ELEMENT)                    
         LHI   RF,3                RF=LENGTH COUNTER                            
                                                                                
SELEN10  CLI   0(RE),0             IF BYTE IS NOT BLANK                         
         JE    SELEN20                                                          
         LA    RE,1(RE)            BUMP TO NEXT BYTE                            
         AHI   RF,1                AND ADD 1 TO LENGTH COUNTER                  
         J     SELEN10                                                          
                                                                                
SELEN20  STC   RF,ELEM+1           PUT LENGTH INTO ELEMENT                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO COVERT INVOICE TO DISPLAYABLE FORMAT              *         
*        ON ENTRY ... P1=A(INPUT INVOICE NUMBER)                      *         
*                     P2=A(AREA FOR DISPLAYABLE INVOICE NUMBER)       *         
***********************************************************************         
                                                                                
CVTINV   J     *+12                                                             
         DC    CL8'*CVTINV*'                                                    
         LR    RB,RF                                                            
         USING CVTINV,RB                                                        
                                                                                
         L     R2,0(R1)          R2=A(INPUT INVOICE NUMBER)                     
         L     R3,4(R1)          R3=A(AREA FOR DISPLAYABLE INVOICE)             
                                                                                
         CLI   5(R2),X'FF'       IF INPUT INVOICE NUMBER                        
         JNE   CI10              NEEDS TO BE COMPLEMENTED                       
         MVC   DUB(6),0(R2)      DO SO NOW                                      
         LA    R2,DUB                                                           
         XC    0(6,R2),=6X'FF'                                                  
                                                                                
CI10     L     RF,=V(TINVCON)                                                   
         A     RF,ROU1RELO                                                      
         GOTO1 (RF),DMCB,(R2),(R3),VDATCON                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADD A RECORD                                      *         
*        ON ENTRY ... P1 = I/O AREA EQUATE                            *         
***********************************************************************         
                                                                                
ADDREC   J     *+12                                                             
         DC    CL8'*ADDREC*'                                                    
         LR    RB,RF                                                            
         USING ADDREC,RB                                                        
                                                                                
         STH   R1,HALF1            HALF1 = I/O AREA EQUATE                      
                                                                                
         LR    R2,R1                                                            
         SRL   R2,12                                                            
         BCTR  R2,0                                                             
         MHI   R2,IOLENQ                                                        
         A     R2,AIO1             R2 = A(I/O AREA)                             
                                                                                
         LHI   RE,IODIR                                                         
         LHI   RF,IOFIL                                                         
         CLI   0(R2),TLCKCDQ       RE = DIRECTORY EQUATE                        
         JNE   *+12                RF = FILE EQUATE                             
         LHI   RE,IOCHKDIR                                                      
         LHI   RF,IOCHKFIL                                                      
                                                                                
         AR    RE,R1                                                            
         STH   RE,HALF1            HALF1 = I/O AREA & DIRECTORY EQUATES         
                                                                                
         AR    R1,RF                                                            
         STH   R1,HALF2            HALF2 = I/O AREA & FILE EQUATES              
                                                                                
         LR    R0,R2                                                            
         LHI   R1,4000                                                          
         L     RE,AIO8             COPY RECORD INTO AIO8                        
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         USING TLDRD,R3                                                         
         LA    R3,IOKEY                                                         
         MVC   TLDRKEY,0(R2)       IF KEY EXISTS IN DELETED STATE               
         LH    R1,HALF1                                                         
         AHI   R1,IOHIUPD                                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   TLDRKEY,0(R2)                                                    
         JNE   AR10                                                             
         NI    TLDRSTAT,X'FF'-X'80'                                             
         LH    R1,HALF1            WRITE BACK KEY IN UNDELETED STATE            
         AHI   R1,IOWRITE                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR'                                
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         LH    R1,HALF2            RECORD MUST ALSO BE DELETED                  
         AHI   R1,IOGETRUP         SO GET IT                                    
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         LHI   R3,4000             REPLACE IT WITH RECORD'S                     
         L     RE,AIO8             NEW STATE                                    
         LR    RF,R3                                                            
         MVCL  R2,RE                                                            
                                                                                
         LH    R1,HALF2            AND PUT IT BACK TO FILE                      
         AHI   R1,IOPUTREC                                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         J     XIT                                                              
                                                                                
AR10     LH    R1,HALF2                                                         
         AHI   R1,IOADDREC         IF KEY DOES NOT EXIST                        
         GOTOR (#IOEXEC,AIOEXEC)   ADD RECORD                                   
         JE    XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADD ACTIVITY ELEMENT                              *         
*        ON ENTRY ... P1 BYTE 0 X'80' = RETURN TRANSACTION TIME IN P3 *         
*                     P1 = A(I/O AREA)                                *         
*                     P2 BYTE 0 = SCREEN EQUATE                       *         
*                     P2 = A(STAFF ID)                                *         
*                     P3 = A(SAVED TRANSACTION TIME)                  *         
***********************************************************************         
                                                                                
ADDTAAC  J     *+12                                                             
         DC    CL8'*ADDTAAC'                                                    
         LR    RB,RF                                                            
         USING ADDTAAC,RB                                                       
                                                                                
         L     R4,0(R1)            R4=(I/O AREA)                                
         L     R3,4(R1)            R3=A(STAFF ID)                               
                                                                                
         MVC   BYTE1,0(R1)         BYTE1=RETURN TRANSACTION TIME FLAG           
         MVC   BYTE2,4(R1)         BYTE2=SCREEN EQUATE                          
         MVC   FULL1,8(R1)         FULL1=A(SAVED TRANSACTION TIME)              
                                                                                
         USING LP_D,R1                                                          
         L     R1,ALP                                                           
                                                                                
         USING TAACD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TAACEL,TAACELQ      INITIALIZE ACTIVITY ELEMENT                  
         MVI   TAACLEN,TAACLNQ                                                  
         MVC   TAACID,LP_USRID     PUT USER ID                                  
         MVC   TAACSTAF,0(R3)      STAFF ID                                     
         GOTO1 VDATCON,DMCB,(5,0),(1,TAACCDTE) DATE                             
         TIME  DEC                 TIME                                         
         STCM  R0,14,TAACCTIM      AND SCREEN INTO ELEMENT                      
         MVC   TAACSCR,BYTE2       AND ADD IT                                   
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R1,R2                                                            
                                                                                
         TM    BYTE1,X'80'         IF RETURNING TRANSACTION TIME                
         JZ    XIT                                                              
         L     R1,FULL1            RETURN IT IN SAVED TRANSACTION               
         STCM  R0,14,0(R1)         TIME FIELD                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADD WEB TRANSACTION ID ELEMENT                    *         
*        ON ENTRY ... P1 = A(I/O AREA)                                *         
*                     P2 = A(WEB TRANSACTION ID)                      *         
***********************************************************************         
                                                                                
ADDWID   J     *+12                                                             
         DC    CL8'*ADDWID*'                                                    
         LR    RB,RF                                                            
         USING ADDWID,RB                                                        
                                                                                
         L     R4,0(R1)            R4=(I/O AREA)                                
         L     R3,4(R1)            R3=A(WEB TRANSACTION ID FIELD)               
                                                                                
         USING TAFND,R2                                                         
         LA    R2,ELEM             BUILD WEB APPLICATION ID ELEMENT             
         MVI   TAFNEL,TAFNELQ      AND ADD IT TO I/O AREA                       
         MVI   TAFNLEN,21                                                       
         MVI   TAFNTYPE,TAFNTWEB                                                
         MVC   TAFNNAME(18),0(R3)                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO OUTPUT ERRORS                                     *         
*        ON ENTRY ... P1 = ERROR RECORD MAP NUMBER                    *         
*                     P2 = A(OUTPUT BLOCK FOR LINKIO)                 *         
***********************************************************************         
                                                                                
OUTERR   J     *+12                                                             
         DC    CL8'*OUTERR*'                                                    
         LR    RB,RF                                                            
         USING OUTERR,RB                                                        
                                                                                
         ZICM  R0,2(R1),2          R0 = ERROR RECORD MAP NUMBER                 
         L     R3,4(R1)            R3 = A(OUTPUT BLOCK FOR LINKIO)              
                                                                                
         USING ERRENTD,R2                                                       
         L     R2,AERRTAB          R2 = A(ERROR TABLE)                          
                                                                                
OERR10   CLI   0(R2),X'FF'         YES IF ALL ERRORS HAVE BEEN                  
         JE    XIT                 PROCESSED                                    
                                                                                
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',(R0))                  
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',1),              +        
               ('LD_UBINQ',EENUMB),(L'EENUMB,0)                                 
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',2),              +        
               ('LD_UBINQ',EECATY),(L'EECATY,0)                                 
         GOTOR (RF),(R1),('LIOAPUT',ALIOB),('LIOTRAW',3),              +        
               ('LD_UBINQ',EEFIELD),(L'EEFIELD,0)                               
                                                                                
         ZIC   R4,EELEN                                                         
                                                                                
         MVC   0(250,R3),SPACES                                                 
         LR    RF,R4                                                            
         SHI   RF,6                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R3),EEMSG                                                    
         AHI   RF,1                                                             
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',4),           +        
               ('LD_CHARQ',EEMSG),((RF),0)                                      
                                                                                
         AR    R2,R4               BUMP TO NEXT ERROR                           
         J     OERR10                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE ERROR HANDLING AREAS                   *         
*        ON ENTRY ... P1 = A(ERROR OVERRIDE REQUEST FIELD)            *         
*                     P2 = A(EXPLODED ERROR OVERRIDES)                *         
*                     P3 = A(CLEAR MAP CODES REQUEST FIELD)           *         
***********************************************************************         
                                                                                
INITERR  J     *+12                                                             
         DC    CL8'*INITER*'                                                    
         LR    RB,RF                                                            
         USING INITERR,RB                                                       
                                                                                
         L     RF,AERRTAB                                                       
         MVI   0(RF),X'FF'         INITIALIZE ERROR TABLE                       
         MVI   ERRSTAT,0           AND ERROR STATUS                             
                                                                                
         XC    IXEROVER(4),IXEROVER                                             
         XC    ICLRMAP(4),ICLRMAP                                               
                                                                                
         L     RF,0(R1)                                                         
         MVC   IEROVER,9(RF)       SET ERROR OVERRIDE COUNT                     
         LA    RF,10(RF)                                                        
         STCM  RF,7,AEROVER        SET A(ERROR OVERRIDE ARRAY)                  
                                                                                
         OC    8(4,R1),8(R1)       IF CLEAR MAP CODES ARRAY IS                  
         JZ    IERR10              PROVIDED                                     
         L     RF,8(R1)                                                         
         MVC   ICLRMAP,9(RF)       SET CLEAR MAP CODES COUNT                    
         LA    RF,10(RF)                                                        
         STCM  RF,7,ACLRMAP        SET A(CLEAR MAP CODES ARRAY)                 
                                                                                
IERR10   MVI   BYPSERRS,X'FF'      INITIALIZE BYPASSED ERRORS                   
                                                                                
         OC    4(4,R1),4(R1)       IF EXPLODED ERROR OVERRIDE ARRAYS            
         JZ    XIT                 ARE PROVIDED                                 
         CLI   IEROVER,0           AND ERRORS ARE BEING BYPASSED                
         JE    XIT                                                              
         L     R2,4(R1)                                                         
IERR20   CLI   0(R2),X'FF'                                                      
         JE    XIT                                                              
         L     RF,IEROVER                                                       
         ZIC   RE,IEROVER                                                       
IERR30   CLC   0(2,R2),0(RF)                                                    
         JE    IERR40                                                           
         LA    RF,2(RF)                                                         
         BCT   RE,IERR30                                                        
         ZICM  RE,2(R2),2                                                       
         AR    R2,RE                                                            
         J     IERR20                                                           
IERR40   ZICM  RF,2(R2),2                                                       
         SHI   RF,4                                                             
         SRA   RF,1                                                             
         STC   RF,IXEROVER                                                      
         LA    RF,4(R2)                                                         
         STCM  RF,7,AXEROVER       SET A(EXPLODED ERROR OVERRIDE ARRAY)         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO TRANSLATE AGENT CODE                              *         
*        ON ENTRY ... P1 BYTE 0 = X'80' CHARACTER TO NUMERIC          *         
*                                 X'40' NUMERIC TO CHARACTER          *         
*                     P1        = A(INCOMING AGENT CODE)              *         
*                     P2        = A(TRANSLATED AGENT CODE)            *         
***********************************************************************         
                                                                                
TRNSAGT  J     *+12                                                             
         DC    CL8'*TRNSAG*'                                                    
         LR    RB,RF                                                            
         USING TRNSAGT,RB                                                       
                                                                                
       ++INCLUDE TATRNSAGT                                                      
***********************************************************************         
*        ROUTINE OUTPUTS TIME FIELD                                   *         
*        ON ENTRY ... P1=A(TIME INPUT FIELD)                          *         
*                     P2=A(TIME OUTPUT FIELD)                         *         
***********************************************************************         
                                                                                
OUTTIME  J     *+12                                                             
         DC    CL8'*OUTTIM*'                                                    
         LR    RB,RF                                                            
         USING OUTTIME,RB                                                       
                                                                                
         L     R2,0(R1)          R2=A(INPUT TIME)                               
         OC    0(L'TAINITIM,R2),0(R2)                                           
         JZ    XIT                                                              
                                                                                
         L     R3,4(R1)          R3=A(AREA FOR DISPLAYABLE TIME)                
                                                                                
         MVO   DUB,0(3,R2)       PACK INPUT TIME                                
         OI    DUB+7,X'0F'                                                      
         THMS  DDSTIME=YES       AND ADD TIME DIFFERENTIAL                      
         ST    R0,FULL1                                                         
         AP    DUB,FULL1(4)                                                     
                                                                                
         CP    DUB+4(4),=P'240000'                                              
         JL    *+10                ADJUST TO 24 HOURS IF NECESSARY              
         SP    DUB+4(4),=P'240000'                                              
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(7),DUB+4(4)                                                 
                                                                                
         MVC   0(2,R3),WORK+1      DISPLAY HOURS                                
         MVI   2(R3),C':'                                                       
         MVC   3(2,R3),WORK+3      MINUTES                                      
         MVI   5(R3),C':'                                                       
         MVC   6(2,R3),WORK+5      AND SECONDS                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE FORMATS LIST                                         *         
*        ON ENTRY ... P1=A(LIST FIELD)                                *         
***********************************************************************         
                                                                                
FMTLIST  J     *+12                                                             
         DC    CL8'*FMTLIS*'                                                    
         LR    RB,RF                                                            
         USING FMTLIST,RB                                                       
                                                                                
         L     R2,0(R1)                                                         
         OC    0(4,R2),0(R2)                                                    
         JZ    XIT                                                              
                                                                                
         ZICM  RF,1(R2),3                                                       
         LHI   RE,9                                                             
         TM    ERRSTAT,ESREVIW                                                  
         JZ    *+8                                                              
         LHI   RE,7                                                             
         AR    RF,RE                                                            
         MVC   0(1,R2),0(RF)                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,7,1(R2)                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ENSURES CAST OVERSCALE AMOUNT/PERCENTAGE FIELD       *         
*        REMAINS IN SYNC WITH COMMERCIAL TYPE AND MEDIA               *         
*        ON ENTRY ... P1 BYTE 0 = OVERSCALE AMOUNT/PCT ELEMENT CODE   *         
*                     P1        = A(CAST RECORD)                      *         
*                     P2        = A(COMMERCIAL TYPE)                  *         
*                     P3        = A(COMMERCIAL MEDIA)                 *         
***********************************************************************         
                                                                                
UPDOAP   J     *+12                                                             
         DC    CL8'*UPDOAP*'                                                    
         LR    RB,RF                                                            
         USING UPDOAP,RB                                                        
                                                                                
         ZICM  R4,1(R1),3                                                       
         ST    R4,FULL1            FULL1=A(CAST RECORD)                         
                                                                                
         USING TAOPD,R4                                                         
         MVC   ELCODE,0(R1)        IF OVERSCALE AMOUNT/PERCENTAGE               
         BRAS  RE,GETEL            FIELD IS PRESENT ...                         
         JNE   XIT                                                              
                                                                                
         L     RE,4(R1)                                                         
         MVC   BYTE1,0(RE)         BYTE1=COMMERCIAL TYPE                        
                                                                                
         L     R2,8(R1)            R2=A(COMMERCIAL MEDIA)                       
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFMED',0(R2))                           
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
***********************************************************************         
                                                                                
         MVC   OLENGTH,TAOPLEN     SAVE ORIGINAL LENGTH                         
                                                                                
         ZIC   R0,TAOPNUM          R0 = # SUB ELEMENTS                          
         LA    R2,TAOPSBEL         R2 = A(FIRST SUB ELEMENT)                    
         DROP  R4                                                               
                                                                                
         USING TAOPD,R3                                                         
         LA    R3,ELEM2                                                         
         XC    ELEM2,ELEM2         INITIALIZE NEW OVERSCALE AMOUNT/             
         MVC   TAOPEL,ELCODE       PERCENTAGE ELEMENT                           
         MVI   TAOPLEN,TAOPLNQ                                                  
         LA    R4,TAOPSBEL                                                      
                                                                                
UOAP10   CLC   SPACES(3),0(R2)     IF USE CODE IS ALL                           
         JE    UOAP30                                                           
         CLC   =C'ARE',0(R2)       OR ARE                                       
         JE    UOAP30              THEN IT REMAINS VALID                        
                                                                                
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFUSE',0(R2))                           
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   TGUSMEDS,ALL        ENSURE USE IS VALID FOR MEDIA                
         JE    UOAP20                                                           
         MVC   BYTE,TGUSMEDS                                                    
         NC    BYTE,TGMEEQU                                                     
         JZ    UOAP40                                                           
                                                                                
UOAP20   GOTOR VTYPE,DMCB,BYTE1    ENSURE USE IS VALID FOR COML TYPE            
         JNE   UOAP40                                                           
                                                                                
UOAP30   ZIC   RE,TAOPLEN          IF USE TYPE IS VALID FOR THIS                
         AHI   RE,L'TAOPSBEL       MEDIA AND TYPE                               
         STC   RE,TAOPLEN                                                       
         ZIC   RE,TAOPNUM          ADD IT TO THE NEW ELEMENT                    
         AHI   RE,1                                                             
         STC   RE,TAOPNUM                                                       
         MVC   0(L'TAOPSBEL,R4),0(R2)                                           
         LA    R4,L'TAOPSBEL(R4)                                                
                                                                                
UOAP40   LA    R2,L'TAOPSBEL(R2)   BUMP TO NEXT USE IN ORIGINAL ELEMENT         
         BCT   R0,UOAP10           REPEAT UNTIL NO MORE SUB-ELEMENTS            
                                                                                
         CLC   TAOPLEN,OLENGTH     IF ELEMENT IS CHANGING                       
         JE    XIT                                                              
         DROP  R3                                                               
                                                                                
         USING TAOPD,R4                                                         
         L     R4,FULL1                                                         
         BRAS  RE,GETEL            SET THE ORIGINAL ELEMENT FOR DELETE          
         JE    *+6                                                              
         DC    H'00'                                                            
         MVI   0(R4),X'FF'                                                      
         DROP  R4                                                               
                                                                                
         CLI   ELEM2+TAOPLEN-TAOPD,TAOPLNQ                                      
         JE    XIT                 AND IF ELEMENT SHOULD STILL EXIST            
         L     R4,FULL1                                                         
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),ELEM2,0 ADD IT NOW            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PUT A RECORD                                      *         
*        ON ENTRY ... P1 = I/O AREA EQUATE                            *         
***********************************************************************         
                                                                                
PUTREC   J     *+12                                                             
         DC    CL8'*PUTREC*'                                                    
         LR    RB,RF                                                            
         USING PUTREC,RB                                                        
                                                                                
         STH   R1,HALF1            HALF1 = I/O AREA EQUATE                      
                                                                                
         LR    R2,R1                                                            
         SRL   R2,12                                                            
         BCTR  R2,0                                                             
         MHI   R2,IOLENQ                                                        
         A     R2,AIO1             R2 = A(RECORD TO PUT)                        
                                                                                
         LHI   RE,IODIR                                                         
         LHI   RF,IOFIL                                                         
         CLI   0(R2),TLCKCDQ                                                    
         JNE   *+12                                                             
         LHI   RE,IOCHKDIR                                                      
         LHI   RF,IOCHKFIL         RE = DIRECTORY EQUATE                        
         STH   RF,HALF2            HALF2 = FILE EQUATE                          
                                                                                
         USING TLDRD,R3                                                         
         LA    R3,IOKEY            KEY MUST ALREADY EXIST                       
         MVC   TLDRKEY,0(R2)                                                    
         L     R1,=AL4(IO8)                                                     
         AHI   R1,IORD                                                          
         AR    R1,RE                                                            
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         L     R1,=AL4(IO8)                                                     
         AHI   R1,IOGET                                                         
         AH    R1,HALF2                                                         
         GOTOR (#IOEXEC,AIOEXEC)   GET ORIGINAL RECORD INTO AIO8                
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         L     RF,AIO8             RF = A(ORIGINAL RECORD)                      
                                                                                
         CLC   0(L'TLRCKEY,R2),0(RF)    IF KEY HAS CHANGED                      
         JNE   PR70                     PUT THE RECORD                          
                                                                                
         LA    R2,TLRCELEM-TLRCD(R2)                                            
         LA    RF,TLRCELEM-TLRCD(RF)                                            
                                                                                
PR10     CLI   0(R2),0             IF AT END OF RECORD TO PUT                   
         JNE   PR20                                                             
         CLI   0(RF),0             BUT NOT AT END OF ORIGINAL RECORD            
         JE    NO                  PUT THE RECORD                               
                                                                                
PR20     CLI   0(RF),0             IF NOT AT END OF RECORD TO PUT BUT           
         JE    PR70                AT END OF ORIG RECORD, PUT RECORD            
                                                                                
         CLI   0(R2),TAACELQ       SKIP ACTIVITY ELEMENTS                       
         JE    PR30                                                             
         CLI   0(R2),TAFNELQ       AND WEB APPLICATION RECORD ID                
         JNE   PR40                ELEMENTS IN RECORD TO PUT                    
         CLI   TAFNTYPE-TAFND(R2),TAFNTWRI                                      
         JNE   PR40                                                             
PR30     ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     PR10                                                             
                                                                                
PR40     CLI   0(RF),TAACELQ       SKIP ACTIVITY ELEMENTS                       
         JE    PR50                                                             
         CLI   0(RF),TAFNELQ       AND WEB APPLICATION RECORD ID                
         JNE   PR60                ELEMENTS IN ORIGINAL RECORD                  
         CLI   TAFNTYPE-TAFND(RF),TAFNTWRI                                      
         JNE   PR60                                                             
PR50     ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         J     PR10                                                             
                                                                                
PR60     CLC   1(1,R2),1(RF)       IF ELEMENT LENGTHS DO NOT MATCH              
         JNE   PR70                PUT THE RECORD                               
                                                                                
         ZIC   R1,1(R2)            IF ELEMENT DOES NOT MATCH                    
         BCTR  R1,0                PUT THE RECORD                               
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   0(0,R2),0(RF)                                                    
         JNE   PR70                                                             
                                                                                
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         J     PR10                                                             
                                                                                
PR70     LH    R1,HALF1                                                         
         AH    R1,HALF2                                                         
         AHI   R1,IOPUTREC                                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADD FIXED CYCLE TRACKING RECORD                              *         
*        ON ENTRY ... P1=A(TALNKFCYD PARAMETER BLOCK)                 *         
***********************************************************************         
                                                                                
ADDFTRK  J     *+12                                                             
         DC    CL8'*ADDFTR*'                                                    
         LR    RB,RF                                                            
         USING ADDFTRK,RB                                                       
                                                                                
         USING TALKFCYD,R3                                                      
         L     R3,0(R1)                                                         
                                                                                
         USING TLFTD,R4                                                         
         L     R4,AIO3                                                          
         XC    0(255,R4),0(R4)                                                  
         MVI   TLFTCD,TLFTCDQ      BUILD KEY WITH RECORD CODE                   
         MVC   TLFTSSN,TLFCSSN     SOCIAL SECURITY NUMBER                       
         MVC   TLFTCOM,TLFCCOM     INTERNAL COMMERCIAL NUMBER                   
         MVC   TLFTCAST,TLFCSEQ    CAST INPUT SEQUENCE NUMBER                   
         MVC   TLFTSTRT,TLFCCYS    CYCLE START DATE                             
         XC    TLFTSTRT,=3X'FF'    (COMPLEMENTED)                               
         MVC   TLFTEND,TLFCCYE     AND CYCLE END DATE                           
         XC    TLFTEND,=3X'FF'     (COMPLEMENTED)                               
         MVI   TLFTLEN+1,41                                                     
                                                                                
         LHI   R2,1                INITIALIZE TRACKING NUMBER                   
         MVC   IOKEY(L'TLFTKEY),0(R4)                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         CLC   IOKEY(TLFTTRK-TLFTD),IOKEYSAV                                    
         JNE   AFT10                                                            
         ZICM  R2,IOKEY+TLFTTRK-TLFTD,2                                         
         LCR   R2,R2                                                            
         AHI   R2,1                                                             
                                                                                
AFT10    LCR   R2,R2               COMPLEMENT IT                                
         STCM  R2,3,TLFTTRK        AND MOVE TRACKING NUMBER INTO KEY            
         DROP  R4                                                               
                                                                                
         USING TAGTD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TAGTEL,TAGTELQ      BUILD GUARANTEE TRACKING ELEMENT             
         MVI   TAGTLEN,TAGTLNQ                                                  
         MVC   TAGTSTRT,TLFCACS    WITH APPLICATION CYCLE START DATE            
         MVC   TAGTEND,TLFCACE     APPLICATION CYCLE END DATE                   
         MVC   TAGTPNH,TLFCPNH     P&H AMOUNT                                   
         MVC   TAGTCRD,TLFCAAM     APPLICATION AMOUNT                           
         MVC   TAGTBAL,TLFCBAL     BALANCE                                      
                                                                                
         OC    TLFCGRR,TLFCGRR     IF GRR-COVERED USE IS PROVIDED ...           
         JZ    AFT30                                                            
         CLI   ACTION,ACTADD       ... AND ADDING NEW FIXED CYCLE               
         JNE   AFT20                                                            
         MVC   TAGTUSE,=C'GRR'     SET THE COVERED USE AS GRR                   
         MVC   TAGTTYPE,TLFCGRR    SET THE COVERED USE                          
         J     AFT30                                                            
AFT20    MVC   TAGTUSE,TLFCGRR     ... AND UPDATING EXISTING FIXED              
         OI    TAGTSTAT,TAGTSGRR   CYCLE, SET COVERED USE                       
                                                                                
AFT30    GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),ELEM,0                        
         DROP  R2                                                               
                                                                                
         USING TACMD,R2                                                         
         OC    TLFCCMT,TLFCCMT     IF COMMENT IS PRESENT                        
         JZ    AFT40                                                            
         XC    ELEM,ELEM           INITIALIZE COMMENT                           
         MVI   TACMEL,TACMELQ                                                   
         MVI   TACMLEN,TACMLNQ                                                  
         MVI   TACMTYPE,TACMTYPH   PUT COMMENT INTO ELEMENT                     
         MVC   TACMCOMM(L'TLFCCMT),TLFCCMT                                      
         GOTOR (#SETELEN,ASETELEN)                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         DROP  R2                                                               
                                                                                
AFT40    GOTOR (#ADDTAAC,AADDTAAC),DMCB,(X'80',(R4)),TLFCSTF,TLFCTIME           
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         J     XIT                                                              
         DROP  R3,RB                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TEST CHECK LOCKOUT STATUS                                    *         
*        ON ENTRY ... P1 = A(LOCKET KEY)                              *         
*                     P2 = A(ERROR ENTRY)                             *         
***********************************************************************         
                                                                                
TSTCLCK  J     *+12                                                             
         DC    CL8'*TSTCLC*'                                                    
         LR    RB,RF                                                            
         USING TSTCLCK,RB                                                       
                                                                                
         USING LKKEYD,R3                                                        
         LA    R3,WORK                                                          
         XC    LOCKEY,LOCKEY                                                    
                                                                                
         USING LP_D,RE                                                          
         L     RE,ALP                                                           
         MVC   LOCKSE,LP_SENO                                                   
         DROP  RE                                                               
                                                                                
         L     RE,0(R1)                                                         
         MVC   LOCKKEY,0(RE)       USE LOCKET KEY THAT WAS PASSED               
         DROP  R3                                                               
                                                                                
         L     R3,4(R1)            R3=A(ERROR ENTRY)                            
                                                                                
TCL10    GOTO1 VLOCKET,DMCB,('LKTESTQ',WORK),ACOMFACS                           
         CLI   4(R1),1             IF CHECK LOCKOUT IN PROGRESS                 
         JNE   NO                  ADD ERROR ENTRY                              
         GOTOR (#ADDERR,AADDERR),DMCB,(R3)                                      
         J     YES                                                              
         DROP  RB                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GET ELEMENT                                                  *         
*        ON ENTRY ... P1 BYTE 0 = ELEMENT CODE                        *         
*                     P1        = A(RECORD)                           *         
*                     P2        = SUBELEMENT CODE                     *         
***********************************************************************         
                                                                                
GETELEM  J     *+12                                                             
         DC    CL8'*GETELE*'                                                    
         LR    RB,RF                                                            
         USING GETELEM,RB                                                       
                                                                                
         ZICM  R4,1(R1),3                                                       
         MVC   ELCODE,0(R1)                                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
GE10     BRAS  RE,NEXTEL                                                        
         JNE   NO                                                               
                                                                                
         CLI   4(R1),0                                                          
         JE    GE20                                                             
         CLC   4(1,R1),2(R4)                                                    
         JNE   GE10                                                             
                                                                                
GE20     ST    R4,AELEM                                                         
         J     YES                                                              
         DROP  RB                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO ADD GENERIC ERROR DETAILS TO ERROR TABLE          *         
*        ON ENTRY ... P1 BYTE 0 = GENERIC ERROR TYPE                  *         
*                                 EENMIS = MISSING FIELD              *         
*                                 EENNAL = FIELD NOT ALLOWED          *         
*                                 EENINV = INVALID VALUE              *         
*                     P2 BYTE 0 = EQUATE FOR ERROR FIELD              *         
***********************************************************************         
                                                                                
ADDGERRS J     *+12                                                             
         DC    CL8'*ADDGER*'                                                    
         LR    RB,RF                                                            
         USING ADDGERRS,RB                                                      
                                                                                
         LA    R2,ERMIS            VALID COMMON ERROR TYPES ARE                 
         CLI   0(R1),EENMIS        MISSING INPUT FIELD                          
         JE    AG10                                                             
         LA    R2,ERNAL                                                         
         CLI   0(R1),EENNAL        FIELD NOT ALLOWED                            
         JE    AG10                                                             
         LA    R2,ERINV                                                         
         CLI   0(R1),EENINV        AND INVALID VALUE                            
         JE    AG10                                                             
         DC    H'00'                                                            
                                                                                
AG10     MVC   ELEM,0(R2)          MOVE ERROR INTO ELEM                         
                                                                                
         USING ERRENTD,R2                                                       
         LA    R2,ELEM             MOVE FIELD EQUATE INTO ERROR                 
         MVC   EEFIELD,4(R1)       AND ADD ERROR                                
         GOTOR (#ADDERR,AADDERR),DMCB,ELEM                                      
         OI    ERRSTAT,ESCECTR                                                  
         J     XIT                                                              
         DROP  R2,RB                                                            
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR ADDGERRS                                   *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERMIS    DC    AL1(EMISX-*),AL2(502),AL1(ERRCATY3),AL1(0)                       
         DC    C'Missing field'                                                 
EMISX    EQU   *                                                                
                                                                                
ERNAL    DC    AL1(ENALX-*),AL2(503),AL1(ERRCATY3),AL1(0)                       
         DC    C'Field not allowed'                                             
ENALX    EQU   *                                                                
                                                                                
ERINV    DC    AL1(EINVX-*),AL2(504),AL1(ERRCATY3),AL1(0)                       
         DC    C'Invalid value'                                                 
EINVX    EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD                         *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                                 VFMODE  = MODE                      *         
*                                 VFYORN  = Y OR N                    *         
*                                 VFW4TY  = W4 TYPE                   *         
*                                 VFCAM   = CAMERA                    *         
*                                 VFUNI   = UNION                     *         
*                                 VFOORN  = O OR N                    *         
*                                 VFCOTY  = COMMERCIAL TYPE           *         
*                                 VFMED   = MEDIA                     *         
*                                 VFACTY  = ACTRA TYPE                *         
*                                 VFCTRY  = COUNTRY                   *         
*                                 VFSOPC  = STATE OR PROVINCE         *         
*                                           (BASED ON COUNTRY)        *         
*                                 VFSOP   = STATE OR PROVINCE         *         
*                                 VFSTATE = STATE                     *         
*                                 VFPROV  = PROVINCE                  *         
*                                 VFCITY  = CITY                      *         
*                                 VFNUM   = NUMERIC                   *         
*                                 VFADST  = ADDENDUM STATE            *         
*                                 VFCAT   = CATEGORY                  *         
*                                 VFYEAR  = CONTRACT YEAR             *         
*                                 VFUSE   = USE                       *         
*                                 VFYON   = Y OR O OR N               *         
*                                 VFWID   = WEB APPLICATION ID        *         
*                     P1        = A(FIELD TO VALIDATE)                *         
*                                                                     *         
*                     FOR VFACTY  ...                                 *         
*                     P2 BYTE 0 = X'80' VALIDATE CONSIDERING MEDIA    *         
*                     P2        = A(MEDIA)                            *         
*                                                                     *         
*                     FOR VFNUM ...                                   *         
*                     P2 BYTE 0 = L'FIELD                             *         
*                                                                     *         
*                     FOR VFUSE   ...                                 *         
*                     P2 BYTE 0 = X'80' VALIDATE CONSIDERING COMM'L   *         
*                                       TYPE AND MEDIA                *         
*                     P2        = A(COMMERCIAL TYPE)                  *         
*                     P3        = A(MEDIA)                            *         
*                                                                     *         
***********************************************************************         
                                                                                
VALFLD   J     *+12                                                             
         DC    CL8'*VALFLD*'                                                    
         LR    RB,RF                                                            
         USING VALFLD,RB                                                        
                                                                                
         ZICM  R2,1(R1),3          R2=A(FIELD TO VALIDATE)                      
         BAS   RE,VALMODE          VALIDATE FIELD AS MODE                       
         JE    YES                                                              
         BAS   RE,VALYORN          Y OR N                                       
         JE    YES                                                              
         BAS   RE,VALW4TY          OR W4 TYPE                                   
         JE    YES                                                              
         BAS   RE,VALCAM           OR CAMERA                                    
         JE    YES                                                              
         BAS   RE,VALUNI           OR UNION                                     
         JE    YES                                                              
         BAS   RE,VALOORN          O OR N                                       
         JE    YES                                                              
         BAS   RE,VALCOTY          COMMERCIAL TYPE                              
         JE    YES                                                              
         BAS   RE,VALMED           MEDIA                                        
         JE    YES                                                              
         BAS   RE,VALACTY          ACTRA TYPE                                   
         JE    YES                                                              
         BAS   RE,VALCTRY          COUNTRY                                      
         JE    YES                                                              
         BAS   RE,VALSOPC          STATE/PROVINCE (BASED ON COUNTRY)            
         JE    YES                                                              
         BAS   RE,VALSOP           STATE/PROVINCE                               
         JE    YES                                                              
         BAS   RE,VALSTATE         STATE                                        
         JE    YES                                                              
         BAS   RE,VALPROV          PROVINCE                                     
         JE    YES                                                              
         BAS   RE,VALCITY          CITY                                         
         JE    YES                                                              
         BAS   RE,VALNUM           NUMERIC                                      
         JE    YES                                                              
         BAS   RE,VALADST          ADDENDUM STATE                               
         JE    YES                                                              
         BAS   RE,VALCAT           CATEGORY                                     
         JE    YES                                                              
         BAS   RE,VALYEAR          CONTRACT YEAR                                
         JE    YES                                                              
         BAS   RE,VALUSE           USE                                          
         JE    YES                                                              
         BAS   RE,VALYON           Y OR O OR N                                  
         JE    YES                                                              
         BAS   RE,VALWID           WEB APPLICATION ID                           
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS MODE                 *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
***********************************************************************         
                                                                                
VALMODE  NTR1                                                                   
         CLI   0(R1),VFMODE        IF FIELD IS MODE                             
         JNE   NO                                                               
         CLI   0(R2),1             VALID VALUES ARE RETRIEVE                    
         JE    YES                                                              
         CLI   0(R2),2             VERIFY                                       
         JE    YES                                                              
         CLI   0(R2),3             AND EXECUTE                                  
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS Y OR N               *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
***********************************************************************         
                                                                                
VALYORN  NTR1                                                                   
         CLI   0(R1),VFYORN        IF FIELD IS Y OR N                           
         JNE   NO                                                               
         CLI   0(R2),0             VALID VALUES ARE BLANK                       
         JE    YES                                                              
         CLI   0(R2),C' '          SPACE                                        
         JE    YES                                                              
         CLI   0(R2),C'Y'          Y                                            
         JE    YES                                                              
         CLI   0(R2),C'N'          AND N                                        
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS W4 TYPE              *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
***********************************************************************         
                                                                                
VALW4TY  NTR1                                                                   
         CLI   0(R1),VFW4TY        IF FIELD IS W4 TYPE                          
         JNE   NO                                                               
         CLI   0(R2),TAW4TYCA      VALID VALUES ARE CANADIAN                    
         JE    YES                                                              
         CLI   0(R2),TAW4TYIN      INDIVIDUAL                                   
         JE    YES                                                              
         CLI   0(R2),TAW4TYCO      CORPORATION                                  
         JE    YES                                                              
         CLI   0(R2),TAW4TYES      ESTATE                                       
         JE    YES                                                              
         CLI   0(R2),TAW4TYFO      FOREIGNER                                    
         JE    YES                                                              
         CLI   0(R2),TAW4TYTR      AND TRUSTEE                                  
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS ON/OFF CAMERA        *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
***********************************************************************         
                                                                                
VALCAM   NTR1                                                                   
         CLI   0(R1),VFCAM         IF FIELD IS CAMERA                           
         JNE   NO                                                               
         OC    0(3,R2),0(R2)       VALID VALUES ARE BLANK                       
         JE    YES                                                              
         CLC   0(3,R2),=C'ON '     ON CAMERA                                    
         JE    YES                                                              
         CLC   0(3,R2),=C'OFF'     AND OFF CAMERA                               
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS UNION                *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
*        ON EXIT  ... AENTRY    = A(UNION ENTRY FRON TASYSUNIS        *         
***********************************************************************         
                                                                                
VALUNI   NTR1                                                                   
         CLI   0(R1),VFUNI         IF FIELD IS UNION                            
         JNE   NO                                                               
         XC    AENTRY,AENTRY                                                    
         OC    0(3,R2),0(R2)       VALID VALUES ARE BLANK                       
         JE    YES                                                              
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB                                                       
         L     RF,TGAUNIS                                                       
         DROP  RE                                                               
                                                                                
         USING UNITABD,RF                                                       
         AR    RF,RE               OR THOSE IN UNION TABLE                      
VU10     CLC   UNICDE,0(R2)                                                     
         JE    VU20                                                             
         CLI   UNILNQ(RF),X'FF'                                                 
         JE    NO                                                               
         LA    RF,UNILNQ(RF)                                                    
         J     VU10                                                             
                                                                                
VU20     ST    RF,AENTRY                                                        
         J     YES                                                              
         DROP  RF                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS O OR N               *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
***********************************************************************         
                                                                                
VALOORN  NTR1                                                                   
         CLI   0(R1),VFOORN        IF FIELD IS O OR N                           
         JNE   NO                                                               
         CLI   0(R2),0             VALID VALUES ARE BLANK                       
         JE    YES                                                              
         CLI   0(R2),C' '          SPACE                                        
         JE    YES                                                              
         CLI   0(R2),C'O'          O                                            
         JE    YES                                                              
         CLI   0(R2),C'N'          AND N                                        
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS COMMERCIAL TYPE      *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
***********************************************************************         
                                                                                
VALCOTY  NTR1                                                                   
         CLI   0(R1),VFCOTY        IF FIELD IS COMMERCIAL TYPE                  
         JNE   NO                                                               
         CLI   0(R2),0             VALID VALUES ARE BLANK                       
         JE    YES                                                              
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB                                                       
         L     RF,TGACOMT                                                       
         DROP  RE                                                               
                                                                                
         USING CTYD,RF                                                          
         AR    RF,RE               OR THOSE IN COMMERCIAL TYPE                  
VCT10    CLC   CTYEQU,0(R2)        TABLE                                        
         JE    YES                                                              
         CLI   CTYNEXT,X'FF'                                                    
         JE    NO                                                               
         LA    RF,CTYNEXT                                                       
         J     VCT10                                                            
         DROP  RF                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS MEDIA                *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
***********************************************************************         
                                                                                
VALMED   NTR1                                                                   
         CLI   0(R1),VFMED         IF FIELD IS MEDIA                            
         JNE   NO                                                               
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB                                                       
         L     RF,TGAMEDS                                                       
         DROP  RE                                                               
                                                                                
         USING MEDIAD,RF                                                        
         AR    RF,RE               VALID VALUES ARE THOSE IS MEDIA              
VM10     CLC   0(1,R2),MEDNAME     TABLE                                        
         JE    VM20                                                             
         CLI   MEDNEXT,X'FF'                                                    
         JE    NO                                                               
         LA    RF,MEDNEXT                                                       
         J     VM10                                                             
                                                                                
VM20     MVC   TGMEEQU,MEDEQU                                                   
         J     YES                                                              
         DROP  RF                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS ACTRA TYPE           *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
*                     P2 BYTE 0 = X'80' VALIDATE CONSIDERING MEDIA    *         
*                     P2        = A(MEDIA)                            *         
***********************************************************************         
                                                                                
VALACTY  NTR1                                                                   
         CLI   0(R1),VFACTY        IF FIELD IS ACTRA TYPE                       
         JNE   NO                                                               
         CLI   0(R2),0             VALID VALUES ARE BLANK                       
         JE    YES                                                              
                                                                                
         TM    4(R1),X'80'                                                      
         JZ    VAT20                                                            
         ZICM  R3,5(R1),3                                                       
         CLI   0(R3),C'N'          IF MEDIA IS NEW MEDIA                        
         JNE   VAT10                                                            
         CLI   0(R2),CCTYVDO       VALID VALUES ARE ADO VIDEO                   
         JE    YES                                                              
         CLI   0(R2),CCTYADO       AND ADO AUDIO                                
         JE    YES                                                              
         J     NO                                                               
                                                                                
VAT10    CLI   0(R2),CCTYVDO       OTHERWISE, ADO VIDEO                         
         JE    NO                                                               
         CLI   0(R2),CCTYADO       AND ADO AUDIO ARE NOT ALLOWED                
         JE    NO                                                               
                                                                                
         USING TGTABLES,RE                                                      
VAT20    L     RE,VSYSTAB                                                       
         L     RF,TGACTYPS                                                      
         DROP  RE                                                               
                                                                                
         USING CCTYPD,RF                                                        
         AR    RF,RE               OR THOSE IN ACTRA TYPE TABLE                 
VAT30    CLC   CCTYPEQU,0(R2)                                                   
         JE    YES                                                              
         CLI   CCTYPNXT,X'FF'                                                   
         JE    NO                                                               
         LA    RF,CCTYPNXT                                                      
         J     VAT30                                                            
         DROP  RF                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS COUNTRY              *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     P1        = A(FIELD TO VALIDATE)                *         
*        ON EXIT  ... ACTRY     = A(COUNTRY ENTRY FROM TASYSCTRY)     *         
***********************************************************************         
                                                                                
VALCTRY  NTR1                                                                   
         CLI   0(R1),VFCTRY        IF FIELD IS COUNTRY                          
         JNE   NO                                                               
                                                                                
         XC    ACTRY,ACTRY                                                      
                                                                                
         ZICM  R2,1(R1),3          AND IS NOT BLANK                             
         OC    0(2,R2),0(R2)                                                    
         JZ    YES                                                              
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB                                                       
         L     R3,TGACTRYS                                                      
         DROP  RE                                                               
                                                                                
         USING CTRYTABD,R3                                                      
         AR    R3,RE                                                            
VCRY10   CLC   CTRYCODE,0(R2)      ENSURE COUNTRY IS VALID                      
         JE    VCRY20                                                           
         ZIC   R0,CTRYLEN                                                       
         AR    R3,R0                                                            
         CLI   0(R3),X'FF'                                                      
         JNE   VCRY10                                                           
         J     NO                                                               
                                                                                
VCRY20   ST    R3,ACTRY            SAVE ENTRY IN ACTRY                          
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS STATE/PROVINCE       *         
*        BASED ON COUNTRY                                             *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
*                     ACTRY     = A(COUNTRY ENTRY FROM TASYSCTRY)     *         
*        ON EXIT  ... AENTRY    = A(STATE ENTRY FROM TASYSUNITS       *         
*                                 OR A(PROVINCE ENTRY FROM TASYSCTRY) *         
***********************************************************************         
                                                                                
VALSOPC  NTR1                                                                   
         CLI   0(R1),VFSOPC        IF FIELD IS STATE/PROVINCE                   
         JNE   NO                  BASED ON COUNTRY                             
         OC    0(2,R2),0(R2)       AND FIELD IS NOT BLANK                       
         JZ    YES                                                              
                                                                                
         XC    AENTRY,AENTRY                                                    
                                                                                
***********************************************************************         
                                                                                
         USING CTRYTABD,R3                                                      
         L     R3,ACTRY                                                         
         CLC   CTRYCODE,=C'US'     IF COUNTRY IS US                             
         JNE   VSOPC40                                                          
         DROP  R3                                                               
                                                                                
         USING TGTABLES,RE                                                      
VSOPC10  L     RE,VSYSTAB                                                       
         L     R3,TGAUNITS                                                      
         DROP  RE                                                               
                                                                                
         USING TALUNITD,R3                                                      
         AR    R3,RE                                                            
VSOPC20  CLC   TALUZIPF,SPACES                                                  
         JE    VSOPC30                                                          
         CLC   TALUCODE(2),0(R2)   ENSURE STATE IS VALID US STATE               
         JE    VSOPC60                                                          
VSOPC30  CLI   TALUNEXT,X'FF'                                                   
         JE    NO                                                               
         LA    R3,TALUNEXT                                                      
         J     VSOPC20                                                          
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
         USING CTRYTABD,R3                                                      
VSOPC40  ZIC   R1,CTRYDSP          IF COUNTRY IS NOT US                         
         ZIC   R0,CTRYLEN                                                       
         DROP  R3                                                               
                                                                                
         USING CTRYSUBD,R3                                                      
         AR    R3,R1                                                            
         AR    R0,R3                                                            
VSOPC50  CLC   CTRYPROV,0(R2)      ENSURE STATE/PROVINCE IS VALID               
         JE    VSOPC60             FOR COUNTRY                                  
         LA    R3,CRTYSLNQ(R3)                                                  
         CR    R3,R0                                                            
         JL    VSOPC50                                                          
         J     NO                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
VSOPC60  ST    R3,AENTRY           RETURN A(ENTRY)                              
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS STATE/PROVINCE       *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
*        ON EXIT  ... AENTRY    = A(STATE ENTRY FROM TASYSUNITS       *         
*                                 OR A(PROVINCE ENTRY FROM TASYSCTRY) *         
***********************************************************************         
                                                                                
VALSOP   NTR1                                                                   
         CLI   0(R1),VFSOP         IF FIELD IS STATE/PROVINCE                   
         JNE   NO                                                               
         OC    0(2,R2),0(R2)       AND FIELD IS NOT BLANK                       
         JZ    YES                                                              
                                                                                
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFSTATE',0(R2))                         
         JE    YES                                                              
                                                                                
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFPROV',0(R2))                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS STATE                *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
*        ON EXIT  ... AENTRY    = A(STATE ENTRY FROM TASYSUNITS)      *         
***********************************************************************         
                                                                                
VALSTATE NTR1                                                                   
         CLI   0(R1),VFSTATE       IF FIELD IS STATE                            
         JNE   NO                                                               
         OC    0(2,R2),0(R2)       AND FIELD IS NOT BLANK                       
         JZ    YES                                                              
                                                                                
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFCTRY',=C'US')                         
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFSOPC',0(R2))                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS PROVINCE             *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
*        ON EXIT  ... AENTRY    = A(PROVINCE ENTRY FROM TASYSCTRY)    *         
***********************************************************************         
                                                                                
VALPROV  NTR1                                                                   
         CLI   0(R1),VFPROV        IF FIELD IS PROVINCE                         
         JNE   NO                                                               
         OC    0(2,R2),0(R2)       AND FIELD IS NOT BLANK                       
         JZ    YES                                                              
                                                                                
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFCTRY',=C'CA')                         
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFSOPC',0(R2))                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS CITY                 *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
*        ON EXIT  ... AENTRY    = A(CITY ENTRY FROM TASYSUNITS)       *         
***********************************************************************         
                                                                                
VALCITY  NTR1                                                                   
         CLI   0(R1),VFCITY                                                     
         JNE   NO                                                               
                                                                                
         OC    0(3,R2),0(R2)                                                    
         JZ    YES                                                              
                                                                                
         USING TGTABLES,RE                                                      
VC10     L     RE,VSYSTAB                                                       
         L     R3,TGAUNITS                                                      
         DROP  RE                                                               
                                                                                
         USING TALUNITD,R3                                                      
         AR    R3,RE                                                            
VC20     CLI   TALUCODE+2,C' '                                                  
         JE    VC30                                                             
         CLC   TALUCODE,0(R2)                                                   
         JE    VC40                                                             
VC30     CLI   TALUNEXT,X'FF'                                                   
         JE    NO                                                               
         LA    R3,TALUNEXT                                                      
         J     VC20                                                             
                                                                                
VC40     ST    R3,AENTRY                                                        
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS NUMERIC              *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
*                     P2 BYTE 0 = L'FIELD                             *         
***********************************************************************         
                                                                                
VALNUM   NTR1                                                                   
         CLI   0(R1),VFNUM         IF FIELD IS NUMERIC                          
         JNE   NO                                                               
                                                                                
         ZIC   RE,4(R1)                                                         
VN10     CLI   0(R2),C'0'                                                       
         JL    NO                                                               
         CLI   0(R2),C'9'                                                       
         JH    NO                                                               
         LA    R2,1(R2)                                                         
         BCT   RE,VN10                                                          
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS ADDENDUM STATE       *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
***********************************************************************         
                                                                                
VALADST  NTR1                                                                   
         CLI   0(R1),VFADST        IF FIELD IS ADDENDUM STATE                   
         JNE   NO                                                               
         OC    0(2,R2),0(R2)       AND FIELD IS NOT BLANK                       
         JZ    YES                                                              
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB                                                       
         L     R3,TGAUNITS                                                      
         DROP  RE                                                               
                                                                                
         USING TALUNITD,R3                                                      
         AR    R3,RE                                                            
VAS10    TM    TALUSTAT,TALUOKAD                                                
         JZ    VAS20                                                            
         CLC   TALUCODE(2),0(R2)   ENSURE STATE IS VALID ADDENDUM               
         JE    YES                 STATE                                        
VAS20    CLI   TALUNEXT,X'FF'                                                   
         JE    NO                                                               
         LA    R3,TALUNEXT                                                      
         J     VAS10                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS CATEGORY             *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
*        ON EXIT  ... AENTRY    = A(CATEGORY ENTRY FROM TASYSCATS)    *         
***********************************************************************         
                                                                                
VALCAT   NTR1                                                                   
         CLI   0(R1),VFCAT         IF FIELD IS CATEGORY                         
         JNE   NO                                                               
         XC    AENTRY,AENTRY                                                    
         OC    0(3,R2),0(R2)       AND FIELD IS NOT BLANK                       
         JZ    YES                                                              
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB                                                       
         L     R3,TGACATS                                                       
         DROP  RE                                                               
                                                                                
         USING CATTABD,R3                                                       
         AR    R3,RE                                                            
VCAT10   CLC   CATCDE,0(R2)        ENSURE FIELD IS VALID CATEGORY               
         JE    VCAT20                                                           
         ZIC   RE,CATLEN                                                        
         AR    R3,RE                                                            
         CLI   0(R3),X'FF'                                                      
         JNE   VCAT10                                                           
         J     NO                                                               
                                                                                
VCAT20   ST    R3,AENTRY                                                        
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS CONTRACT YEAR        *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
*        ON EXIT  ... AENTRY    = A(YEAR ENTRY FROM TASYSYEARS        *         
***********************************************************************         
                                                                                
VALYEAR  NTR1                                                                   
         CLI   0(R1),VFYEAR        IF FIELD IS YEAR                             
         JNE   NO                                                               
         XC    AENTRY,AENTRY                                                    
         OC    0(3,R2),0(R2)       AND FIELD IS NOT BLANK                       
         JZ    YES                                                              
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB                                                       
         L     R3,TGAYEARS                                                      
         DROP  RE                                                               
                                                                                
         USING YRTABD,R3                                                        
         AR    R3,RE                                                            
VY10     CLC   YRCDE,0(R2)         ENSURE FIELD IS VALID YEAR                   
         JE    VY20                                                             
         CLI   YRLNQ(R3),X'FF'                                                  
         JE    NO                                                               
         LA    R3,YRLNQ(R3)                                                     
         J     VY10                                                             
                                                                                
VY20     ST    R3,AENTRY                                                        
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS USE                  *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
*                     P2 BYTE 0 = X'80' VALIDATE CONSIDERING COMM'L   *         
*                                       TYPE AND MEDIA                *         
*                     P2        = A(COMMERCIAL TYPE)                  *         
*                     P3        = A(MEDIA)                            *         
*        ON EXIT  ... AENTRY    = A(USE ENTRY FROM TASYSUSES          *         
***********************************************************************         
                                                                                
VALUSE   NTR1                                                                   
         CLI   0(R1),VFUSE         IF FIELD IS USE                              
         JNE   NO                                                               
         XC    AENTRY,AENTRY                                                    
         OC    0(3,R2),0(R2)       AND FIELD IS NOT BLANK                       
         JZ    YES                                                              
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB                                                       
         L     R3,TGAUSES                                                       
         DROP  RE                                                               
                                                                                
         USING USETABD,R3                                                       
         AR    R3,RE                                                            
VUSE10   CLC   USECDE,0(R2)        ENSURE FIELD IS VALID USE                    
         JE    VUSE20                                                           
         LH    RE,USELEN                                                        
         AR    R3,RE                                                            
         CLI   0(R3),X'FF'                                                      
         JNE   VUSE10                                                           
         J     NO                                                               
                                                                                
VUSE20   MVC   TGUSEQU,USEEQU      SET USE EQUATE                               
         MVC   TGUSXUNS,USEEXUNI   EXCLUDED UNIONS                              
         MVC   TGUSMEDS,USEMEDS    VALID MEDIA FOR USE                          
         MVC   TGUSSTA2,USESTAT2   STATUS 2                                     
         MVC   TGUSSTA3,USESTAT3   STATUS 3                                     
         MVC   TGUSSTA4,USESTAT4   STATUS 4                                     
         DROP  R3                                                               
                                                                                
         TM    4(R1),X'80'         IF VALIDATING CONSIDERING COMM'L             
         JZ    VUSE30              TYPE ANE MEDIA                               
         L     R4,4(R1)                                                         
         L     R5,8(R1)                                                         
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFMED',0(R5))                           
         JNE   NO                                                               
         GOTOR VTYPE,DMCB,0(R4)    ENSURE USE IS VALID                          
         JNE   NO                                                               
                                                                                
VUSE30   ST    R3,AENTRY           AND A(USE TABLE ENTRY)                       
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS Y OR O OR N          *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
***********************************************************************         
                                                                                
VALYON   NTR1                                                                   
         CLI   0(R1),VFYON         IF FIELD IS Y OR O OR N                      
         JNE   NO                                                               
         CLI   0(R2),0             VALID VALUES ARE BLANK                       
         JE    YES                                                              
         CLI   0(R2),C' '          SPACE                                        
         JE    YES                                                              
         CLI   0(R2),C'Y'          Y                                            
         JE    YES                                                              
         CLI   0(R2),C'O'          O                                            
         JE    YES                                                              
         CLI   0(R2),C'N'          AND N                                        
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES THE PROVIDED FIELD AS WEB APPLICATION ID   *         
*        ON ENTRY ... P1 BYTE 0 = FIELD TYPE                          *         
*                     R2        = A(FIELD TO VALIDATE)                *         
***********************************************************************         
                                                                                
VALWID   NTR1                                                                   
         CLI   0(R1),VFWID         IF FIELD IS WEB APPLICATION ID               
         JNE   NO                                                               
         CLC   =C'VS',0(R2)        VALID VALUES OLD VITA TV SESSIONS            
         JE    YES                                                              
         CLC   =C'TS',0(R2)        NEW VITA TV SESSIONS                         
         JE    YES                                                              
         CLC   =C'RS',0(R2)        VITA RADIO SESSIONS                          
         JE    YES                                                              
         CLC   =C'VC',0(R2)        OLD VITA TV COMPLETIONS                      
         JE    YES                                                              
         CLC   =C'TC',0(R2)        NEW VITA TV COMPLETIONS                      
         JE    YES                                                              
         CLC   =C'RC',0(R2)        AND VITA RADIO COMPLETIONS                   
         JE    YES                                                              
         J     NO                                                               
         DROP  RB                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAVALTYP                                                       
***********************************************************************         
*              ROUTINE TO TEST 4-BYTE UNION AREA AGAINST EQUATES                
*                                  P1=A(4-BYTE BLOCK OF UNION BYTES             
*                                       TO TEST - TGUNEQUS, TGCAUNIS            
*                                       OR TGUSXUNS)                            
*                                     BYTE 0 X'80'=TEST THIS BLOCK              
*                                                  AGAINST A BLOCK OF           
*                                                  STORAGE INSTEAD OF           
*                                                  STRAIGHT EQUATES             
*                                  P2=UNION EQUATES TO TEST FIRST               
*                                     BYTE FOR                                  
*                                     OR, IF BYTE 0 OF P1 IS X'80',             
*                                     A(4-BYTE BLOCK OF UNION BYTES             
*                                       TO TEST AGAINST 4-BYTE BLOCK            
*                                       OF UNION BYTES SPECIFIED BY             
*                                       P1)                                     
*                                  P3=UNION EQUATES TO TEST SECOND              
*                                     BYTE FOR                                  
*                                     IF BYTE 0 OF P1 IS X'80' THIS             
*                                     PARAMETER SHOULD BE EMPTY                 
*                                  P4=UNION EQUATES TO TEST THIRD               
*                                     BYTE FOR                                  
*                                     IF BYTE 0 OF P1 IS X'80' THIS             
*                                     PARAMETER SHOULD BE EMPTY                 
*                                  P5=UNION EQUATES TO TEST FOURTH              
*                                     BYTE FOR                                  
*                                     IF BYTE 0 OF P1 IS X'80' THIS             
*                                     PARAMETER SHOULD BE EMPTY                 
*              RETURN CONDITION CODE                                            
***********************************************************************         
                                                                                
UTEST    NTR1  BASE=*,LABEL=*                                                   
       ++INCLUDE TAUNITEST                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
NO       DS    0H                  SET CC NOT EQUAL                             
EXITL    LHI   RE,0                SET CC LOW                                   
         J     EXITCC                                                           
EXITH    LHI   RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
YES      LHI   RE,1                SET CC EQUAL                                 
EXITCC   CHI   RE,1                                                             
                                                                                
XIT      XIT1  ,                                                                
                                                                                
FACTAB   DS    0XL(FACTABL)        ** EXTRACTED COMFACS ADDRESSES **            
         DC    AL2(VADDAY-WORKD,CADDAY-COMFACSD)                                
         DC    AL2(VBINSRCH-WORKD,CBINSRCH-COMFACSD)                            
         DC    AL2(VCALLOV-WORKD,CCALLOV-COMFACSD)                              
         DC    AL2(VCASHVAL-WORKD,CCASHVAL-COMFACSD)                            
         DC    AL2(VCUREDIT-WORKD,CCUREDIT-COMFACSD)                            
         DC    AL2(VDATCON-WORKD,CDATCON-COMFACSD)                              
         DC    AL2(VDATVAL-WORKD,CDATVAL-COMFACSD)                              
         DC    AL2(VDATAMGR-WORKD,CDATAMGR-COMFACSD)                            
         DC    AL2(VDDLINK-WORKD,CDDLINK-COMFACSD)                              
         DC    AL2(VRUNIT-WORKD,CRUNIT-COMFACSD)                                
         DC    AL2(VGETDAY-WORKD,CGETDAY-COMFACSD)                              
         DC    AL2(VGETFACT-WORKD,CGETFACT-COMFACSD)                            
         DC    AL2(VGETPROF-WORKD,CGETPROF-COMFACSD)                            
         DC    AL2(VGLOBBER-WORKD,CGLOBBER-COMFACSD)                            
         DC    AL2(VHELLO-WORKD,CHELLO-COMFACSD)                                
         DC    AL2(VHEXIN-WORKD,CHEXIN-COMFACSD)                                
         DC    AL2(VHEXOUT-WORKD,CHEXOUT-COMFACSD)                              
         DC    AL2(VPERVAL-WORKD,CPERVAL-COMFACSD)                              
         DC    AL2(VPERVERT-WORKD,CPERVERT-COMFACSD)                            
         DC    AL2(VRUNIT-WORKD,CRUNIT-COMFACSD)                                
         DC    AL2(VSECRET-WORKD,CSECRET-COMFACSD)                              
         DC    AL2(VSOFDAT-WORKD,CSOFDAT-COMFACSD)                              
         DC    AL2(VSWITCH-WORKD,CSWITCH-COMFACSD)                              
         DC    AL2(VXSORT-WORKD,CXSORT-COMFACSD)                                
         DC    AL2(VPROTON-WORKD,CPROTON-COMFACSD)                              
         DC    AL2(VPROTOFF-WORKD,CPROTOFF-COMFACSD)                            
         DC    AL2(VLINKIO-WORKD,CLINKIO-COMFACSD)                              
         DC    AL2(VWSSVR-WORKD,CWSSVR-COMFACSD)                                
         DC    AL2(VREQTWA-WORKD,CREQTWA-COMFACSD)                              
         DC    AL2(VLOCKET-WORKD,CLOCKET-COMFACSD)                              
         DC    AL2(VMQIO-WORKD,CMQIO-COMFACSD)                                  
FACTABN  EQU   (*-FACTAB)/FACTABL                                               
                                                                                
FACTABD  DSECT                     ** DSECT TO COVER FACTAB ABOVE **            
FACTDOUT DS    AL2                 DISPLACEMENT TO OUTPUT ADDRESS               
FACTDIN  DS    AL2                 DISPLACEMENT TO INPUT ADDRESS                
FACTABL  EQU   *-FACTABD                                                        
TALNK01  CSECT                                                                  
                                                                                
CORPHS   DS    0AL1                ** CORERES PHASES TO LOAD **                 
         DC    AL1(QFALINK)                                                     
         DC    AL1(QTASYSTB)                                                    
CORPHSN  EQU   (*-CORPHS)/L'CORPHS                                              
                                                                                
FILTAB   DS    0X                  ** FILE DEFINITIONS **                       
                                                                                
         DC    AL1(IOTALDIR/256),C'TALDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOTALFIL/256,32,02),AL2(38)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOTALFIL/256),C'TALFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOTALDIR/256,32,02),AL2(IOLENQ)                              
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCHKDIR/256),C'CHKDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOCHKFIL/256,32,02),AL2(38)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCHKFIL/256),C'CHKFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOCHKDIR/256,32,02),AL2(IOLENQ)                              
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCTFILE/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,29),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
         DS    AL1(0)                                                           
                                                                                
CMDTAB   DS    0X                  ** I/O COMMANDS **                           
                                                                                
*                                  INDEX SEQUENTIAL COMMANDS                    
CMDIS    DC    AL1(FILIIS,0),AL2(CMDISX+1-CMDIS)                                
         DC    C'DMRDHI ',AL1(IOHI,0,0)                                         
         DC    C'DMREAD ',AL1(IORD,0,0)                                         
         DC    C'DMRSEQ ',AL1(IOSQ,0,0)                                         
         DC    C'DMADD  ',AL1(IOADD,0,0)                                        
         DC    C'DMWRT  ',AL1(IOWRITE,0,0)                                      
CMDISX   DC    AL1(0)                                                           
                                                                                
*                                  DIRECT ACCESS COMMANDS                       
CMDDA    DC    AL1(FILIDA,0),AL2(CMDDAX+1-CMDDA)                                
         DC    C'GETREC ',AL1(IOHI,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IORD,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOSQ,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOGET,CMDIDARQ,0)                                 
         DC    C'ADDREC ',AL1(IOADDREC,CMDIDADD,0)                              
         DC    C'PUTREC ',AL1(IOPUTREC,CMDIDARQ,0)                              
CMDDAX   DC    AL1(0)                                                           
                                                                                
CMDTABX  DC    AL1(0)                                                           
                                                                                
* TALNKWRK                                                                      
* TALDCPTRD                                                                     
* TASYSEQUS                                                                     
* TALNKFCYD                                                                     
* FALOCKETD                                                                     
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE TALDCPTRD                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TALNKFCYD                                                      
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TALNK01   10/30/19'                                      
         END                                                                    
