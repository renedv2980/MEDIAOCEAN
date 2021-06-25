*          DATA SET NELNK01    AT LEVEL 012 AS OF 06/02/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T30201E                                                                  
*INCLUDE WRKIO                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE PNAME                                                                  
*INCLUDE NETUNWK                                                                
NELNK01  TITLE '- NETWORK SYSTEM SERVER SUPPORT ROUTINES 1'                     
NELNK01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NL01**,RR=RE                                                 
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
         DC    AL2(WRKINI-NELNK01),AL2(0)                                       
         DC    AL2(IOEXEC-NELNK01),AL2(IOWORKL)                                 
         DC    AL2(NXTREC-NELNK01),AL2(0)                                       
         DC    AL2(TYP3TO4-NELNK01),AL2(0)                                      
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
         L     RF,CALLOV                                                        
WRKINI02 IC    R0,0(R2)                                                         
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         AHI   R2,1                BUMP TO THE NEXT ENTRY                       
         AHI   R3,L'APHASES        BUMP TO NEXT ADDRESS                         
         BCT   R4,WRKINI02         DO FOR NUMBER OF PHASES                      
                                                                                
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
LVALUES  DS    0D                  ** LITERALS MOVED TO WORKD **                
         DC    V(TWABLD)                                                        
         DC    V(WRKIO)                                                         
         DC    V(PNAME)                                                         
         DC    V(NETUNWK)                                                       
         DC    A(FACTAB)                                                        
         DC    A(CORPHS)                                                        
         DC    A(FILTAB)                                                        
         DC    A(CMDTAB)                                                        
         DC    A(CONTTAB)                                                       
         DC    A(AIRNTAB)                                                       
         DC    A(MEDIAT)                                                        
         DC    A(BBTAB)                                                         
         DC    A(PRGDEMTB)                                                      
         DC    A(NTIDEMS)                                                       
         DC    A(DISPTAB)                                                       
         DC    25A(0)                                                           
         DC    16F'0'                                                           
         DC    CL(L'SPACES)' '                                                  
         DC    (L'ZEROES)C'0'                                                   
         DC    X'D9000A'                                                        
         DC    (L'EFFS)AL1(FF)                                                  
         DC    C'Y'                                                             
         DC    C'N'                                                             
         DC    C'N/A'                                                           
LVALUESL EQU   *-LVALUES                                                        
                                                                                
***********************************************************************         
* GLOBAL TABLES                                                       *         
***********************************************************************         
                                                                                
CONTTAB  DS    0CL31               PROGRAM CONTENT TYPES                        
         DC    C'R',CL20'Regular',CL10' '                                       
         DC    C'S',CL20'Special',CL10' '                                       
         DC    C'T',CL20'Total(Reg+Spec)',CL10' '                               
         DC    AL1(FF)                                                          
                                                                                
AIRNTAB  DS    0CL31               PROGRAM AIRING TYPES                         
         DC    C'O',CL20'Original',CL10' '                                      
         DC    C'R',CL20'Repeat',CL10' '                                        
         DC    C'C',CL20'Combined',CL10'S'                                      
         DC    C'M',CL20'Multiple',CL10'S'                                      
         DC    AL1(FF)                                                          
                                                                                
MEDIAT   DS    0X                  VAR PARAMETERS BY MEDIA                      
         DC    CL30'Broadcast Network'                                          
         DC    AL1(MEDNET),C'N',C'N',C'NTI',AL1(NETSTAQ),C'A'                   
                                                                                
         DC    CL30'Syndication'                                                
         DC    AL1(MEDSYN),C'N',C'N',C'NTI',AL1(SYNSTAQ),C'A'                   
                                                                                
         DC    CL30'Cable'     '                                                
         DC    AL1(MEDCAB),C'N',C'N',C'NTI',AL1(CBLSTAQ),X'00'                  
                                                                                
*        DC    CL30'Hispanic'                                                   
*        DC    AL1(MEDNHW),C'N',C'W',C'NTI',AL1(HISSTAQ),X'00'                  
         DC    X'FF'                                                            
                                                                                
BBTAB    DS    0X                  BUILDING BLOCK TABLES BY FILE                
         DC    C'PNN',C'ANN'       BROADCAST AND SYNDICATION                    
         DC    C'CNN',C'ANN'       CABLE                                        
         DC    X'FF'                                                            
                                                                                
       ++INCLUDE NEPROGDEMS                                                     
       ++INCLUDE DENTIDEMS                                                      
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
         B     IOEX22                                                           
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
         B     IOEX22                                                           
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
         BO    IOEX16                                                           
         TM    IOCMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IOEX18                                                           
         ICM   R1,15,IOADDR        FIND THIS I/O AREA DA/WORK                   
         BZ    IOEX14                                                           
         AHI   R1,IODDWQ           DISPLACE TO DA/WORK IN I/O AREA              
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    0(L'IODA,R1),0(R1)                                               
         OC    IODAOVER,IODAOVER   TEST OVERRIDE D/A PASSED                     
         BZ    IOEX12                                                           
         MVC   IODA,IODAOVER       YES - SET AND CLEAR                          
         XC    IODAOVER,IODAOVER                                                
         B     IOEX14                                                           
                                                                                
IOEX12   OC    0(L'IODA,R1),0(R1)                                               
         BZ    *+10                                                             
         MVC   IODA,0(R1)          YES - SET D/A                                
         AHI   R1,L'IODA                                                        
         OC    0(L'IOWORK,R1),0(R1)                                             
         BZ    IOEX14                                                           
         MVC   IOWORK,0(R1)        SET WORK                                     
                                                                                
IOEX14   OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   IOEX16                                                           
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
         BE    IOEX16              SUCCESSFUL I/O                               
         BL    IOEXX               EXIT ON BAD I/S ERRORS                       
         TM    IOERR,IOERNF        TEST RECORD-NOT-FOUND                        
         BNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    IOEXX                                                            
         OC    IODA,IODA           TEST DISK ADDRESS SET                        
         BNZ   *+6                                                              
         DC    H'0'                SOMETHING BAD HAPPENED                       
IOEX16   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR DATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK              
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         GOTOR IOTRCE,IOTRFIL                                                   
         ICM   R1,15,IOADDR        PARK DA/WORK FOR THIS I/O AREA               
         BZ    IOEXX                                                            
         AHI   R1,IODDWQ           DISPLACE TO DA/WORK IN I/O AREA              
         MVC   0(L'IODA,R1),IODA                                                
         MVC   L'IODA(L'IOWORK,R1),IOWORK                                       
         B     IOEXX               EXIT TO CALLER                               
                                                                                
IOEX18   TM    IOFILI,FILIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IOEX22                                                           
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IOFILI,FILIVL                                                    
         BZ    IOEX20                                                           
         ICM   R0,15,IOADDR        VL I/S READS INTO IOAREA ADDRESS             
         BNZ   IOEX20                                                           
         DC    H'0'                                                             
                                                                                
IOEX20   GOTOR ,IOCB,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                           
         GOTOR IOTRCE,IOTRDIR+IOTRBEF                                           
         GOTOR DATAMGR,IOCB                                                     
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         GOTOR IOTRCE,IOTRDIR+IOTRAFT                                           
         TM    IOFILI2,FILIID                                                   
         BNZ   *+14                                                             
         L     R1,IOADDR                                                        
         MVC   IOKEY,0(R1)         RETURN KEY IF DIRECTORY ONLY                 
         TM    IOERR,IOERRS        TEST ANY ERRORS FOUND                        
         BZ    *+12                                                             
         TM    IOERR,IOEDEL        TEST DELETED RECORD FOUND                    
         BZ    IOEXX               NO - EXIT WITH ERROR                         
         TM    IOFILI2,FILIID      TEST D/A FILE ATTACHED TO THIS FILE          
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
                                                                                
IOEX22   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR DATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                     
         MVC   IOERR,8(R1)                                                      
                                                                                
IOEXX    TM    IOERR,IOERRS        ANY ERRORS?                                  
         JZ    EXITY                                                            
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
         GOTOR HEXOUT,IOPARM,IOKEY,IOHEXWRK,1(R2),IOHEXSEP F                    
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
         GOTOR HEXOUT,IOPARM,(R0),IOWORK,4,IOHEXTOG                             
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
         GOTOR HEXOUT,IOPARM,IOCB+8,IOP+13,1,IOHEXTOG                           
         GOTOR HEXOUT,IOPARM,(R3),IOHEXWRK,1(R2),IOHEXSEP                       
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
NELNK01  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET NEXT RECORD                                                     *         
*                                                                     *         
* NOTE: X'80' BIT ON IN P3/B0 MEANS I/O TO STATION FILE               *         
***********************************************************************         
                                                                                
NXTREC   J     *+12                                                             
         DC    CL8'*NXTREC*'                                                    
         LR    RB,RF                                                            
         USING NXTREC,RB                                                        
         L     R0,0(R1)            R0=A(KEY TABLE)                              
         LM    R3,R6,4(R1)         R3=A(KEY SAVE AREA), R4=A(WORK SAVE)         
         LA    R3,0(R3)            R5=A(KEY FILTER ROUTINE)                     
         MVC   BYTE1,8(R1)         R6=A(RECORD FILTER ROUTINE)                  
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
         TM    BYTE1,X'80'         TEST I/O TO STATION FILE                     
         BZ    *+8                                                              
         LHI   R1,IOHI+IOSTAFIL                                                 
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR LP_ASETK,DMCB,(1,(R0)),IOKEY,(R4),('FF',LP_D)                    
         BNE   NXTREC02                                                         
         LTR   RF,R5               TEST/SET DIRECTORY FILTER ROUTINE            
         BZ    *+12                                                             
         GOTOR GOFILT                                                           
         BNE   NXTREC02                                                         
         TM    BYTE1,X'80'         TEST I/O TO STATION FILE                     
         JNZ   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IORDEL'                           
         BE    *+14                                                             
         TM    IOERR,IOEDEL        IGNORE DELETED RECORDS (SPILL)               
         BNZ   NXTREC02                                                         
         DC    H'0'                DIE ON OTHER ERRORS                          
         LTR   RF,R6               SET/TEST FILE FILTER ROUTINE                 
         JZ    EXITY                                                            
         L     R1,IOADDR           PASS A(RECORD) IN R1                         
         GOTOR GOFILT                                                           
         BNE   NXTREC02            DIDN'T PASS FILTERS - GET NEXT               
         J     EXITY                                                            
                                                                                
NXTREC04 LTR   R3,R3               TEST ANY KEY SAVED                           
         BZ    NXTREC06                                                         
         MVC   IOKEY,0(R3)         YES - RESTORE IT                             
NXTREC06 MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS TO COME                  
         J     EXITN               EXIT WITH CC=NOT EQUAL TO CALLER             
                                                                                
GOFILT   NTR1  LABEL=NO            CALL RECORD FILTER ROUTINE                   
         L     RE,ALP                                                           
         LM    R2,RB,LP_R2RB-LP_D(RE)                                           
         BASR  RE,RF                                                            
         J     EXIT                                                             
         DROP  R2,RB                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT TYPE-3 DEMO NUMBER TO TYPE-4 DEMO NUMBER                    *         
* INPUT:  DMCB(4) = A(TYPE-3 DEMO NUMBER)                             *         
* OUTPUT: TYPE-4 DEMO NUMBER                                          *         
* TYPE-3 NUMBER IS 1 BYTE                                             *         
* TYPE-4 NUMBER IS 2 BYTES                                            *         
***********************************************************************         
                                                                                
TYP3TO4  J     *+12                                                             
         DC    CL8'*TYP3TO4'                                                    
         LR    RB,RF                                                            
         USING TYP3TO4,RB                                                       
         L     R3,0(R1)            R3=A(TYPE-3 DEMO NUMBER)                     
                                                                                
         L     R4,ANTIDEMS                                                      
         USING NTIDTD,R4                                                        
TY3T4_10 CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   NTDTYP3,0(R3)                                                    
         BE    TY3T4_20                                                         
         LA    R4,NTIDTL(R4)                                                    
         B     TY3T4_10                                                         
                                                                                
TY3T4_20 L     R3,4(R1)                                                         
         MVC   0(L'NTDTYP4,R3),NTDTYP4  RETURN TYPE-4 DEMO NUMBER               
         J     EXIT                                                             
         DROP  RB,R4                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
EXITN    DS    0H                  SET CC NOT EQUAL                             
EXITL    LHI   RE,0                SET CC LOW                                   
         J     EXITCC                                                           
EXITH    LHI   RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
EXITY    LHI   RE,1                SET CC EQUAL                                 
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                                                                
                                                                                
FACTAB   DS    0XL(FACTABL)        ** EXTRACTED COMFACS ADDRESSES **            
         DC    AL2(ADDAY-WORKD,CADDAY-COMFACSD)                                 
         DC    AL2(BINSRCH-WORKD,CBINSRCH-COMFACSD)                             
         DC    AL2(CALLOV-WORKD,CCALLOV-COMFACSD)                               
         DC    AL2(DATCON-WORKD,CDATCON-COMFACSD)                               
         DC    AL2(DATVAL-WORKD,CDATVAL-COMFACSD)                               
         DC    AL2(DATAMGR-WORKD,CDATAMGR-COMFACSD)                             
         DC    AL2(DDLINK-WORKD,CDDLINK-COMFACSD)                               
         DC    AL2(GETDAY-WORKD,CGETDAY-COMFACSD)                               
         DC    AL2(GETFACT-WORKD,CGETFACT-COMFACSD)                             
         DC    AL2(GETPROF-WORKD,CGETPROF-COMFACSD)                             
         DC    AL2(HELLO-WORKD,CHELLO-COMFACSD)                                 
         DC    AL2(HEXIN-WORKD,CHEXIN-COMFACSD)                                 
         DC    AL2(HEXOUT-WORKD,CHEXOUT-COMFACSD)                               
         DC    AL2(SECRET-WORKD,CSECRET-COMFACSD)                               
         DC    AL2(SWITCH-WORKD,CSWITCH-COMFACSD)                               
         DC    AL2(XSORT-WORKD,CXSORT-COMFACSD)                                 
         DC    AL2(RUNIT-WORKD,CRUNIT-COMFACSD)                                 
         DC    AL2(SCANNER-WORKD,CSCANNER-COMFACSD)                             
         DC    AL2(DEMAND-WORKD,CDEMAND-COMFACSD)                               
         DC    AL2(DEMOUT-WORKD,CDEMOUT-COMFACSD)                               
         DC    AL2(DEMADDR-WORKD,CDEMADDR-COMFACSD)                             
         DC    AL2(DEMOMATH-WORKD,CDEMOMTH-COMFACSD)                            
FACTABN  EQU   (*-FACTAB)/FACTABL                                               
                                                                                
FACTABD  DSECT                     ** DSECT TO COVER FACTAB ABOVE **            
FACTDOUT DS    AL2                 DISPLACEMENT TO OUTPUT ADDRESS               
FACTDIN  DS    AL2                 DISPLACEMENT TO INPUT ADDRESS                
FACTABL  EQU   *-FACTABD                                                        
NELNK01  CSECT                                                                  
                                                                                
CORPHS   DS    0AL1                ** CORERES PHASES TO LOAD **                 
         DC    AL1(QFALINK)                                                     
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QTSAR)                                                       
         DC    AL1(QBOOKVAL)                                                    
         DC    AL1(QNETWEEK)                                                    
         DC    AL1(QDAYVAL)                                                     
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QDEFINE)                                                     
         DC    AL1(QNETUNBK)                                                    
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QUNDAY)                                                      
CORPHSN  EQU   (*-CORPHS)/L'CORPHS                                              
                                                                                
FILTAB   DS    0X                  ** FILE DEFINITIONS **                       
                                                                                
         DC    AL1(IOSPTDIR/256),C'SPTDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOSPTFIL/256,13,01),AL2(18)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOUNTDIR/256),C'UNTDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOUNTFIL/256,20,01),AL2(25)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOSPTFIL/256),C'SPTFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOSPTDIR/256,13,01),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOUNTFIL/256),C'UNTFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOUNTDIR/256,20,01),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOSTAFIL/256),C'STATION'                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,15,00),AL2(1000)                                           
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCTFILE/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,29),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENDIR/256),C'GENDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOGENFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIL/256),C'GENFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOGENDIR/256,32,04),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(0)                                                           
                                                                                
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
                                                                                
         PRINT OFF                                                              
       ++INCLUDE NELNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NELNK01   06/02/20'                                      
         END                                                                    
