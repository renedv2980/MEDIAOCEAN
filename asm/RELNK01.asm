*          DATA SET RELNK01    AT LEVEL 001 AS OF 05/16/12                      
*PHASE T82B01A                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE UNBOOK                                                                 
RELNK01  TITLE '- REP System server support routines 1'                         
RELNK01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PL01**,RR=RE                                                 
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
         DC    AL2(WRKINI-RELNK01),AL2(0)                                       
         DC    AL2(IOEXEC-RELNK01),AL2(IOWORKL)                                 
         DC    AL2(NXTREC-RELNK01),AL2(0)                                       
         DC    AL2(GETPID-RELNK01),AL2(0)                                       
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALIZE WORKING STORAGE VARIABLES                                *         
***********************************************************************         
                                                                                
WRKINI   J     *+12                                                             
         DC    CL8'*WRKINI*'                                                    
         LR    RB,RF                                                            
         USING WRKINI,RB                                                        
         LA    R0,SVALUES          MOVE LITERALS TO W/S                         
         LHI   R1,SVALUESL                                                      
         LA    RE,LVALUES                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R1,RELOLST          RELOCATE ADCONS                              
         LA    R0,RELOLSTN                                                      
         BASR  RE,0                                                             
         L     RF,0(R1)                                                         
         A     RF,ROU1RELO                                                      
         ST    RF,0(R1)                                                         
         AHI   R1,L'RELOLST                                                     
         BCTR  R0,RE                                                            
                                                                                
         LHI   RF,IOAREA1-WORKD    SET ADDRESSES OF I/O AREAS                   
         LA    RF,WORKD(RF)                                                     
         LA    R0,AIONM                                                         
         LA    R1,AIO1                                                          
         BASR  RE,0                                                             
         ST    RF,0(R1)                                                         
         AHI   RF,IOLENQ                                                        
         AHI   R1,L'AIO1                                                        
         BCTR  R0,RE                                                            
                                                                                
         L     R1,AFACTAB                                                       
         USING FACTABD,R1          EXTRACT FACILITES LIST ADCONS                
         LHI   R0,FACTABN                                                       
         BASR  RE,0                                                             
         SR    RF,RF                                                            
         SR    R2,R2                                                            
         ICM   R2,3,FACTDOUT                                                    
         LA    R2,WORKD(R2)                                                     
         SR    R3,R3                                                            
         ICM   R3,3,FACTDIN                                                     
         IC    RF,FACTFLST                                                      
         L     RF,FACLISTS(RF)                                                  
         AR    R3,RF                                                            
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         MVC   0(4,R2),0(R3)                                                    
         AHI   R1,FACTABL                                                       
         BCTR  R0,RE                                                            
                                                                                
         L     R2,ACORPHS          R2=A(CORE PHASE LIST)                        
         LA    R3,APHASES          R3=A(CORE PHASE ADDRESS LIST)                
         LA    R4,CORPHSN          R4=CORE PHASE COUNT                          
         SR    R0,R0                                                            
         ICM   R0,14,T00A                                                       
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
WRKINI02 ICM   R0,1,0(R2)          TEST PHASE                                   
         BZ    WRKINI04            NONE, SKIP TO THE NEXT ENTRY                 
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
WRKINI04 AHI   R2,1                BUMP TO THE NEXT ENTRY                       
         AHI   R3,L'APHASES                                                     
         BCT   R4,WRKINI02                                                      
                                                                                
         LHI   R0,QREPFACS         Load                                         
         ICM   R0,B'1110',T00A                                                  
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
         GOTOR (RF),(R1),0,(R0),0                                               
         MVC   AREPFACS,0(R1)                                                   
                                                                                
         GOTOR VDATCON,DMCB,(5,0),(3,TODAYB)                                    
         GOTOR (RF),(R1),,(2,TODAYC)                                            
         GOTOR (RF),(R1),,(1,TODAYP)                                            
                                                                                
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
LVALUES  DS    0D                  ** LITERALS MOVED TO WORKD **                
         DC    V(TWABLD)                                                        
         DC    A(0)                                                             
         DC    A(FACTAB)                                                        
         DC    A(CORPHS)                                                        
         DC    A(FILTAB)                                                        
         DC    A(CMDTAB)                                                        
         DC    V(UNBOOK)                                                        
         DC    4A(0)                                                            
         DC    CL132' '                                                         
         DC    X'D9000A'                                                        
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
IOEX02   LA    R1,IOFILES          ESTABLISH FILE                               
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
         LA    RE,FILTABL(RE)                                                   
         B     IOEX06                                                           
         MVC   IOFILV,FILNUM       EXTRACT FILE VALUES                          
         L     RE,ACMDTAB          RE=A(I/O COMMAND TABLE)                      
         USING CMDTABD,RE                                                       
         SR    RF,RF                                                            
         LA    R1,IOCMNDS          ESTABLISH COMMAND                            
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
         LA    RE,CMDNTRYL(RE)                                                  
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
         LA    R1,L'IODA(R1)                                                    
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
         GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK             
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
         BZ    *+14                                                             
         ICM   R0,15,IOADDR        VL I/S READS INTO IOAREA ADDRESS             
         BNZ   *+6                                                              
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
RELNK01  CSECT                                                                  
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
         LM    R3,R5,4(R1)         R3=A(KEY SAVE AREA), R4=A(WORK SAVE)         
         LA    R3,0(R3)            R4=A(WORK SAVE),R5=A(FILTER ROUTINE)         
         MVC   BYTE1,8(R1)         SET READ DELETES CONTROL BYTE                
         L     R2,ALP                                                           
         USING LP_D,R2                                                          
         MVC   BYTE2,4(R1)                                                      
         NI    BYTE2,LD_IBLKQ                                                   
         SR    RE,RE                                                            
         IC    RE,BYTE2                                                         
         SLL   RE,2                                                             
         L     RE,LP_BLKS-L'LP_BLKS(RE)                                         
         ST    RE,LP_ADATA                                                      
         ST    RE,IOADDR                                                        
         MVC   BYTE2,4(R1)         SET FILE FLAG (X'80'=PUBDIR/PUBFIL)          
         NI    BYTE2,FF-(LD_IBLKQ)                                              
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME CALL                         
         BNE   NXTREC02                                                         
         MVI   LP_RMODE,LP_RNEXT   RESET FIRST TIME CALL                        
         CLI   0(R1),C'Y'          TEST RECORD SET IS WANTED                    
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
         CLI   IOKEY+(L'RCONKEY),FF                                             
         BE    NXTREC02            DROP IGNORABLE BUY RECORDS                   
         GOTOR LP_ASETK,DMCB,(1,(R0)),IOKEY,(R4),('FF',LP_D)                    
         BNE   NXTREC02                                                         
         LHI   R1,IOGET+IOFIL+IORDEL                                            
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LTR   RF,R5               SET/TEST FILTER ROUTINE ADDRESS              
         JZ    EXITY                                                            
         L     R1,IOADDR                                                        
         GOTOR (RF)                                                             
         JE    EXITY                                                            
         B     NXTREC02                                                         
NXTREC04 LTR   R3,R3                                                            
         BZ    NXTREC06                                                         
         MVC   IOKEY,0(R3)         RESTORE SAVED RECORD KEY                     
NXTREC06 MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS TO COME                  
         J     EXITN               EXIT WITH CC=NOT EQUAL TO CALLER             
         DROP  R2,RB                                                            
                                                                                
***********************************************************************         
***********************************************************************         
                                                                                
GETPID   J     *+12                                                             
         DC    CL8'*GETPID*'                                                    
         LR    RB,RF                                                            
         USING GETPID,RB                                                        
                                                                                
         MVC   QHXSUSER,TWAUSRID                                                
         L     RE,VXTRAINF                                                      
         USING XTRAINFD,RE                                                      
         MVC   QHXPID,XIPID                                                     
         DROP  RE                                                               
                                                                                
         L     RF,ALP                                                           
         MVC   QSECAGY,LP_AGY-LP_D(RF)                                          
                                                                                
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CT5REC,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,QSECAGY                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO1'                            
         L     R4,AIO1                                                          
         CLC   IOKEY(25),0(R4)                                                  
         JNE   G_PID20                                                          
                                                                                
         LA    R2,CT5DATA                                                       
G_PID14  CLI   0(R2),0                                                          
         JE    G_PID20                                                          
         CLI   0(R2),CTSEAELQ                                                   
         BE    G_PID16                                                          
         ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         J     G_PID14                                                          
                                                                                
         USING CTSEAD,R2                                                        
G_PID16  CLC   QSECAGY,SPACES                                                   
         JNH   *+10                                                             
         MVC   QSECAGY,CTSEAAID                                                 
         DROP  R4,R2                                                            
                                                                                
G_PID20  XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,QHXSUSER                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO1'                            
         L     R4,AIO1                                                          
         CLC   IOKEY(25),0(R4)                                                  
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,CTIDATA                                                       
G_PID34  CLI   0(R2),0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),CTDSCELQ                                                   
         JE    G_PID40                                                          
         ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         J     G_PID34                                                          
                                                                                
         USING CTDSCD,R2                                                        
G_PID40  MVC   QSGONCHR,CTDSC                                                   
         DROP  R4,R2                                                            
                                                                                
         USING SA0REC,R4                                                        
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,QSECAGY                                                  
         MVC   SA0KNUM,QHXPID                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO1'                            
         L     R4,AIO1                                                          
         CLC   IOKEY(25),0(R4)                                                  
         JNE   PIDMSSER                                                         
                                                                                
         LA    R2,SA0DATA                                                       
G_PID50  CLI   0(R2),0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),SAPALELQ                                                   
         JE    G_PID60                                                          
         ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         J     G_PID50                                                          
                                                                                
         USING SAPALD,R2                                                        
G_PID60  MVC   QPIDCHAR,SAPALPID                                                
         DROP  R4,R2                                                            
                                                                                
G_PID_X  J     EXITY                                                            
                                                                                
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
PIDMSSER LHI   R0,985              PID is missing                               
         J     INVALL                                                           
                                                                                
INVALL   L     RE,ALP              Set error and exit back to DDLINK            
         USING LP_D,RE                                                          
         STCM  R0,3,LP_ERROR                                                    
         J     EXITN                                                            
         DROP  RE                                                               
                                                                                
EXITY    LHI   RF,1                                                             
         J     EXITCC              Equal                                        
EXITL    LHI   RF,0                                                             
         J     EXITCC              Low                                          
EXITH    DS    0H                  High                                         
EXITN    LHI   RF,2                Not equal                                    
EXITCC   CHI   RF,1                Set condition code                           
                                                                                
EXIT     XIT1  ,                                                                
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
FACTAB   DS    0XL(FACTABL)        ** EXTRACTED FACILITIES ADDRESSES **         
         DC    AL1(FACTCOMQ),AL2(VADDAY-WORKD,CADDAY-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VCALLOV-WORKD,CCALLOV-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VCASHVAL-WORKD,CCASHVAL-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VCUREDIT-WORKD,CCUREDIT-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VDATCON-WORKD,CDATCON-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VDATVAL-WORKD,CDATVAL-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VDATAMGR-WORKD,CDATAMGR-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VDDLINK-WORKD,CDDLINK-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VRUNIT-WORKD,CRUNIT-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VGETDAY-WORKD,CGETDAY-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VGETFACT-WORKD,CGETFACT-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VGETPROF-WORKD,CGETPROF-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VGETTXT-WORKD,CGETTXT-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VGLOBBER-WORKD,CGLOBBER-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VHELLO-WORKD,CHELLO-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VHEXIN-WORKD,CHEXIN-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VHEXOUT-WORKD,CHEXOUT-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VPERVAL-WORKD,CPERVAL-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VPERVERT-WORKD,CPERVERT-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VREQTWA-WORKD,CREQTWA-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VRUNIT-WORKD,CRUNIT-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VSCANNER-WORKD,CSCANNER-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VSEARCH-WORKD,CSEARCH-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VSECRET-WORKD,CSECRET-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VSOFDAT-WORKD,CSOFDAT-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VSWITCH-WORKD,CSWITCH-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VXSORT-WORKD,CXSORT-COMFACSD)                  
         DC    AL1(FACTCOMQ),AL2(VBINSRCH-WORKD,CBINSRCH-COMFACSD)              
         DC    AL1(FACTCOMQ),AL2(VLOCKET-WORKD,CLOCKET-COMFACSD)                
         DC    AL1(FACTCOMQ),AL2(VXTRAINF-WORKD,CXTRAINF-COMFACSD)              
FACTABN  EQU   (*-FACTAB)/FACTABL                                               
                                                                                
FACTABD  DSECT                     ** DSECT TO COVER FACTAB ABOVE **            
FACTFLST DS    XL1                 FACILITIES LIST DISPLACEMENT                 
FACTCOMQ EQU   ACOMFACS-FACLISTS   COMFACS                                      
FACTSYSQ EQU   AREPFACS-FACLISTS   MEDFACS                                      
FACTDOUT DS    AL2                 DISPLACEMENT TO OUTPUT ADDRESS               
FACTDIN  DS    AL2                 DISPLACEMENT TO INPUT ADDRESS                
FACTABL  EQU   *-FACTABD                                                        
RELNK01  CSECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
CORPHS   DS    0AL1                ** CORERES PHASES TO LOAD **                 
         DC    AL1(QCENTER)                                                     
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QQSORT)                                                      
         DC    AL1(QFALINK)                                                     
         DC    AL1(QDDLINK)                                                     
         DC    AL1(QTSAR)                                                       
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QCFMIO)                                                      
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QGETBROD)                                                    
CORPHSN  EQU   (*-CORPHS)/L'CORPHS                                              
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
FILTAB   DS    0X                  ** FILE DEFINITIONS **                       
                                                                                
         DC    AL1(IOREPDIR/256),C'REPDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOREPFIL/256,27,01),AL2(32)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOREPFIL/256),C'REPFILE'                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOREPDIR/256,27,01),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENDIR/256),C'GENDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOGENFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIL/256),C'GENFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOGENDIR/256,32,04),AL2(2000)                                
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
                                                                                
* RELNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE RELNKWRK                                                       
         PRINT ON                                                               
                                                                                
* DDOFFICED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
                                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001RELNK01   05/16/12'                                      
         END                                                                    
