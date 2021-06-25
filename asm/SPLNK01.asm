*          DATA SET SPLNK01    AT LEVEL 038 AS OF 12/10/19                      
*PHASE T21E01B                                                                  
*INCLUDE WRKIO                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE SPEDUP                                                                 
*INCLUDE NSIWEEK                                                                
SPLNK01  TITLE '- SPOT SYSTEM SERVER SUPPORT ROUTINES 1'                        
SPLNK01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SL01**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
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
         DC    AL2(WRKINI-SPLNK01),AL2(0)                                       
         DC    AL2(IOEXEC-SPLNK01),AL2(IOWORKL)                                 
         DC    AL2(NXTREC-SPLNK01),AL2(0)                                       
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
                                                                                
         LHI   RF,IODAWKA-WORKD    D/A & DMWORK AREA - REGULAR I/O              
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIODAWKA                                                      
                                                                                
         LHI   RF,IODAWKB-WORKD    D/A & DMWORK AREA - BLOCK I/O                
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIODAWKB                                                      
                                                                                
         LHI   RF,IOAREAS-WORKD    SET ADDRESSES OF I/O AREAS                   
         LA    RF,WORKD(RF)                                                     
         LHI   R0,AIONM                                                         
         LA    R1,AIO1             R1=A(A(I/O AREA))                            
         LA    R2,IOLS             R2=I/O AREA LENGTH TABLE                     
         BASR  RE,0                                                             
         ST    RF,0(R1)                                                         
         AH    RF,0(R2)                                                         
         AHI   R1,L'AIO1                                                        
         AHI   R2,L'IOLS                                                        
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
                                                                                
         LA    R0,DDNDX                                                         
         L     RE,ALP                                                           
         ST    R0,LP_ANDX-LP_D(RE) SET A(MASTER MAP INDEX)                      
                                                                                
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
                                                                                
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
LVALUES  DS    0D                  ** LITERALS MOVED TO WORKD **                
         DC    V(TWABLD)                                                        
         DC    V(WRKIO)                                                         
         DC    A(FACTAB)                                                        
         DC    A(CORPHS)                                                        
         DC    A(FILTAB)                                                        
         DC    A(CMDTAB)                                                        
         DC    V(SPEDUP)                                                        
         DC    V(NSIWEEK)                                                       
         DC    2A(0)                                                            
         DC    CL(L'SPACES)' '                                                  
         DC    X'D9000A'                                                        
         DC    (L'EFFS)AL1(FF)                                                  
LVALUESL EQU   *-LVALUES                                                        
                                                                                
IOLS     DS    0H                  ** LENGTHS OF I/O AREAS **                   
         DC    AL2(IO1LQ)          LENGTH OF I/O AREA 1                         
         DC    AL2(IO2LQ)          LENGTH OF I/O AREA 2                         
         DC    AL2(IO3LQ)          LENGTH OF I/O AREA 3                         
         DC    AL2(IO4LQ)          LENGTH OF I/O AREA 4                         
         DC    AL2(IO5LQ)          LENGTH OF I/O AREA 5                         
         DC    AL2(IO6LQ)          LENGTH OF I/O AREA 6                         
         DC    AL2(IO7LQ)          LENGTH OF I/O AREA 7                         
         DC    AL2(IO8LQ)          LENGTH OF I/O AREA 8                         
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
         OC    IODAOVER,IODAOVER   LEAVE IOWORK INTACT IF IODAOVER SET          
         BNZ   *+10                                                             
         XC    IOWORK,IOWORK       CLEAR IOWORK AREA                            
                                                                                
         CLI   GBYACT,0                                                         
         BE    *+10                                                             
         XC    GETBLK+1(L'GETBLK-1),GETBLK+1                                    
                                                                                
         TM    IOCTRL4,IOLOCK      TEST READ-FOR-UPDATE                         
         BZ    *+8                                                              
         OI    IOQ,IOQLOCK                                                      
         TM    IOCTRL4,IORDEL      TEST READ FOR DELETED RECORDS                
         BZ    *+8                                                              
         OI    IOQ,IOQRDEL                                                      
         TM    IOCTRL4,IORMNT      MAINTENANCE?                                 
         BZ    *+8                                                              
         OI    IOQ,IOQCTRL+IOQMNT                                               
                                                                                
         LH    R1,=AL2(IOALL)      ESTABLISH I/O AREA ADDRESS                   
         TM    IOCTRL4,IOBLK#      TEST USING BLOCK ADDRESS FORMAT              
         BZ    *+8                                                              
         LH    R1,=AL2(IOBLKS)                                                  
         N     R1,IOCTRL                                                        
         BZ    IOEX020             I/O AREA NOT GIVEN                           
                                                                                
         TM    IOCTRL4,IOBLK#      TEST USING BLOCK ADDRESS FORMAT              
         BZ    IOEX010                                                          
         SLL   R1,2                R1=BLOCK NUMBER*4                            
         L     RE,ALP                                                           
         LA    RF,LP_BLKS-L'LP_BLKS-LP_D(R1,RE)                                 
         MVC   IOADDR,0(RF)                                                     
         OC    IOADDR,IOADDR                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SRL   R1,2                                                             
         BCTR  R1,0                                                             
         MHI   R1,L'IODAWKB                                                     
         A     R1,AIODAWKB                                                      
         ST    R1,IOADAWK          I/O D/A & DMWORK SAVE AREA                   
         B     IOEX020                                                          
                                                                                
IOEX010  SRL   R1,12-2             R1=I/O AREA NUMBER*4                         
         LA    RF,AIO1-L'AIO1(R1)                                               
         MVC   IOADDR,0(RF)                                                     
         OC    IOADDR,IOADDR                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SRL   R1,2                R1=I/O AREA NUMBER                           
         BCTR  R1,0                                                             
         MHI   R1,L'IODAWKA                                                     
         A     R1,AIODAWKA                                                      
         ST    R1,IOADAWK          I/O D/A & DMWORK SAVE AREA                   
                                                                                
IOEX020  LHI   R1,IOFILES          ESTABLISH FILE                               
         N     R1,IOCTRL                                                        
         BNZ   IOEX030                                                          
         OC    IOFILE,IOFILE                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      SET FILE NAME                                
         OC    IOCMND,IOCMND       FILE GIVEN - SO MUST COMMAND BE              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      SET COMMAND NAME                             
         B     IOEX150                                                          
                                                                                
IOEX030  SRL   R1,8                R1=FILE NUMBER                               
         L     RE,AFILTAB                                                       
         USING FILTABD,RE                                                       
IOEX040  CLI   FILNUM,0                                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLM   R1,1,FILNUM         MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         AHI   RE,FILTABL                                                       
         B     IOEX040                                                          
         MVC   IOFILV,FILNUM       EXTRACT FILE VALUES                          
                                                                                
         L     RE,ACMDTAB          RE=A(I/O COMMAND TABLE)                      
         USING CMDTABD,RE                                                       
         SR    RF,RF                                                            
         LHI   R1,IOCMNDS          ESTABLISH COMMAND                            
         TM    IOCTRL4,IOBLK#      TEST USING BLOCK ADDRESS FORMAT              
         BZ    *+8                                                              
         LH    R1,=AL2(IOBCMDS)                                                 
         N     R1,IOCTRL                                                        
         BNZ   IOEX050                                                          
         OC    IOCMND,IOCMND       NOT GIVEN - TEST COMMAND NAMED               
         BNZ   IOEX150                                                          
         DC    H'0'                                                             
                                                                                
IOEX050  CLI   CMDFILT,0           TEST END OF FILE COMMAND TABLE               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),CMDFILT                                                 
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),CMDFILT    MATCH FILE NUMBER TO TABLE                   
         BNE   *+12                                                             
         LA    RE,CMDNTRY                                                       
         B     IOEX060                                                          
         SR    RF,RF               BUMP TO NEXT FILE TABLE                      
         ICM   RF,3,CMDTABL                                                     
         AR    RE,RF                                                            
         B     IOEX050                                                          
                                                                                
IOEX060  TM    IOCTRL4,IOBLK#      TEST USING BLOCK ADDRESS FORMAT              
         BZ    IOEX070                                                          
         SRL   R1,16-4             YES - SHIFT COMMAND NUMBER TO LOB            
                                                                                
         USING CMDNTRY,RE          RE=A(COMMAND TABLE ENTRY)                    
IOEX070  CLI   CMDNTRY,0                                                        
         BNE   *+6                                                              
         DC    H'0'                INVALID COMMAND                              
         CLM   R1,1,CMDNUM         MATCH ON COMMAND NUMBER                      
         BE    *+12                                                             
         AHI   RE,CMDNTRYL                                                      
         B     IOEX070                                                          
                                                                                
         MVC   IOCMDV,CMDNAME      EXTRACT COMMAND VALUES                       
                                                                                
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     TEST ADDREC                                  
         BO    IOEX100                                                          
         TM    IOCMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IOEX130                                                          
         ICM   R1,15,IOADAWK       POINT TO I/O D/A & DMWORK AREA               
         BZ    IOEX090                                                          
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW (GETREC)                  
         BZ    *+10                                                             
         XC    0(L'IODA+L'IOWORK,R1),0(R1)                                      
         OC    IODAOVER,IODAOVER   TEST OVERRIDE D/A PASSED                     
         BZ    IOEX080                                                          
         MVC   IODA,IODAOVER       SET IODA                                     
         XC    IODAOVER,IODAOVER   AND CLEAR IOWORK                             
         TM    IOCMDI,CMDIUPDT     TEST PUTREC                                  
         BZ    IOEX090                                                          
         OC    IOWORK,IOWORK       TEST IOWORK NON-ZERO FOR PUTREC              
         BNZ   IOEX090                                                          
         DC    H'0'                                                             
                                                                                
IOEX080  MVC   IODA(L'IODA+L'IOWORK),0(R1)                                      
                                                                                
IOEX090  OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   IOEX100                                                          
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
         BE    IOEX100             SUCCESSFUL I/O                               
         BL    IOEXX               EXIT ON BAD I/S ERRORS                       
         TM    IOERR,IOERNF        TEST RECORD-NOT-FOUND                        
         BNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    IOEXX                                                            
         OC    IODA,IODA           TEST DISK ADDRESS SET                        
         BNZ   IOEX100                                                          
         DC    H'0'                SOMETHING BAD HAPPENED                       
                                                                                
IOEX100  TM    IOCMDI,CMDIUPDT     TEST ADDREC/PUTREC                           
         BZ    IOEX102                                                          
         CLI   IOBRDLST+L'IOBRDLST-1,0                                          
         BE    IOEX102                                                          
         DC    H'0'                IOBRDLST IS FULL OR CORRUPT                  
                                                                                
IOEX102  ICM   R0,15,IOADDR        POINT TO RECORD                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   GBYACT,0                                                         
         BE    IOEX105                                                          
         XC    GETBLK+1(L'GETBLK-1),GETBLK+1                                    
         MVC   GBYDMIN,IOQ                                                      
         MVI   GBYDMOUT,X'FF'                                                   
         LA    RE,IODA                                                          
         ST    RE,GBYDA                                                         
         MVC   GBYIOA,IOADDR                                                    
         LA    RE,IOWORK                                                        
         ST    RE,GBYDMWRK                                                      
         LA    RE,IOBRDLST                                                      
         ST    RE,GBYPRDL                                                       
         MVC   GBYCOMF,ACOMFACS                                                 
         MVC   GBY1OR2,SV1OR2                                                   
         GOTO1 VGETBUY,GETBLK                                                   
         MVC   IOCB(24),DMCB                                                    
         MVC   IOERR,GBYERR                                                     
         B     IOEX110                                                          
                                                                                
IOEX105  GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK,   *        
               IOBRDLST                                                         
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
IOEX110  GOTOR IOTRCE,IOTRFIL                                                   
*                                                                               
         TM    IOCTRL4,IOBLK#      TEST USING BLOCK ADDRESS FORMAT              
         BNZ   IOEX120             YES, WE'RE NOT TESTING THIS CASE             
*                                                                               
         LH    RE,=AL2(IOALL)      ISOLATE THE IO AREA                          
         N     RE,IOCTRL                                                        
         BZ    IOEX120             SO WE CAN FIND THE IOAREA'S LENGTH           
         SRL   RE,12                                                            
         BCTR  RE,0                                                             
         SLL   RE,1                                                             
         LH    RF,=Y(IOLS-IOEXEC)  RE = LOCATE THE CORRECT LENGTH               
         AR    RF,RB                                                            
         AR    RE,RF                                                            
*                                                                               
         LLC   RF,IOCTRL3                                                       
         LR    RF,R0               RF ALSO POINTING TO THE IOAREA               
         LLC   R1,IOFILKL          DISPL INTO RECORD LENGTH                     
         AR    RF,R1               RF NOW POINTING TO THE RECORD LENGTH         
         XR    R1,R1                                                            
         ICM   R1,3,0(RF)                                                       
         CLM   R1,3,0(RE)                                                       
         BNH   IOEX120                                                          
         DC    H'0'                IOAREA NOT BIG ENOUGH FOR RECORD             
*                                                                               
IOEX120  ICM   R1,15,IOADAWK       PARK D/A & DMWORK FOR THIS I/O               
         BZ    IOEXX                                                            
         MVC   0(L'IODA+L'IOWORK,R1),IODA                                       
         B     IOEXX               EXIT TO CALLER                               
                                                                                
IOEX130  TM    IOFILI,FILIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IOEX150                                                          
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IOFILI,FILIVL                                                    
         BZ    IOEX140                                                          
         ICM   R0,15,IOADDR        VL I/S READS INTO IOAREA ADDRESS             
         BNZ   IOEX140                                                          
         DC    H'0'                                                             
                                                                                
IOEX140  GOTOR ,IOCB,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                           
         GOTOR IOTRCE,IOTRDIR+IOTRBEF                                           
*                                                                               
         CLI   GBYACT,0                                                         
         BE    IOEX143                                                          
         XC    GETBLK+1(L'GETBLK-1),GETBLK+1                                    
         MVC   GBYDMIN,IOQ                                                      
         MVI   GBYDMOUT,X'FF'                                                   
         LA    RE,IOKEY                                                         
         ST    RE,GBYKEYIN                                                      
         ST    RE,GBYKEYOT                                                      
         MVC   GBYCOMF,ACOMFACS                                                 
         MVC   GBY1OR2,SV1OR2                                                   
         GOTO1 VGETBUY,GETBLK                                                   
         MVC   IOCB(24),DMCB                                                    
         MVC   IOERR,GBYERR                                                     
         B     IOEX146                                                          
*                                                                               
IOEX143  GOTOR VDATAMGR,IOCB                                                    
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
IOEX146  GOTOR IOTRCE,IOTRDIR+IOTRAFT                                           
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
         TM    IOCMDI,CMDIUPDT     TEST UPDATIVE I/O (DMWRT/DMADD)              
         BNZ   IOEXX               YES - EXIT                                   
         LLC   R1,IOFILKL                                                       
         LLC   R0,IOFILCL                                                       
         AR    R1,R0                                                            
         LA    R1,IOKEY(R1)                                                     
         MVC   IODA,0(R1)                                                       
         ICM   R1,15,IOADAWK       SAVE D/A FOR THIS I/O & CLEAR DMWORK         
         BZ    IOEXX                                                            
         MVC   0(L'IODA,R1),IODA                                                
         XC    L'IODA(L'IOWORK,R1),L'IODA(R1)                                   
         B     IOEXX                                                            
                                                                                
IOEX150  ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR VDATAMGR,IOCB,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                    
         MVC   IOERR,8(R1)                                                      
                                                                                
IOEXX    MVI   GBYACT,0                                                         
         TM    IOERR,IOERRS        ANY ERRORS?                                  
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
IOADAWK  DS    A                   A(I/O D/A & DMWORK SAVE AREA)                
IOCTRL   DS    0XL4                ** I/O COMMAND WORD **                       
IOCTRL1  DS    X                                                                
IOCTRL2  DS    X                                                                
IOCTRL3  DS    X                   I/O AREA#/FILE#                              
IOCTRL4  DS    X                   BLOCK#/READ-FOR-UPDATE/READ DELETES          
IOQ      DS    X                   ** I/O COMMAND QUALIFIER **                  
IOQLOCK  EQU   X'80'               READ-FOR-UPDATE                              
IOQRDEL  EQU   X'08'               READ DELETES                                 
IOQCTRL  EQU   X'20'               CONTROL FLAG FOR SPECIAL COMMANDS            
IOQMNT   EQU   X'04'               - SPCL CMMD - MAINTENANCE UPDATE             
IOFILV   DS    0XL15               ** EXTRACTED FILE VALUES **                  
IOFILNO  DS    X                   FILE NUMBER                                  
IOFILNM  DS    CL7                 COMMAND NAME                                 
IOFILI   DS    X                   FILE INDICATORS - 1                          
IOFILI2  DS    X                   FILE INDICATORS - 2                          
IOFILN2  DS    X                   FILE NUMBER 2 (I/S D/A PAIR)                 
IOFILKL  DS    X                   KEY LENGTH                                   
IOFILCL  DS    X                   CONTROL LENGTH                               
IOFILDE  EQU   IOFILCL             DISPLACEMENT TO FIRST ELEMENT                
IOFILML  DS    XL2                 MAXIMUM RECORD LENGTH                        
IOTRIND  DS    X                   ** TRACE CONTROL **                          
IOTRDIR  EQU   X'40'               I/O TO DIRECTORY                             
IOTRFIL  EQU   X'20'               I/O TO FILE                                  
IOTRBEF  EQU   X'01'               BEFORE I/O                                   
IOTRAFT  EQU   X'02'               AFTER I/O                                    
IOCMDV   DS    0XL10               ** EXTRACTED COMMAND VALUES **               
IOCMDNM  DS    CL7                 COMMAND NAME                                 
IOCMDNO  DS    X                   COMMAND NUMBER                               
IOCMDI   DS    X                   COMMAND INDICATORS - 1                       
IOCMDI2  DS    X                   COMMAND INDICATORS - 2                       
IOP      DS    CL132                                                            
IOHEXWRK DS    XL220                                                            
IOQOUT   DS    X                   DATA MGR OUT BITS                            
IOWORKL  EQU   *-IOWORKD                                                        
SPLNK01  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET NEXT RECORD                                                     *         
*                                                                     *         
* NOTE:- P3/B0 HAS FILE EQUATE/DELETE OPTION (SEE $NXTRXXX EQUATES)   *         
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
         ST    RE,IOADDR                                                        
         TM    BYTE1,$NXTRXAD      TEST INHIBIT SETTING LP_ADATA                
         BNZ   *+8                                                              
         ST    RE,LP_ADATA                                                      
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME CALL                         
         BNE   NXTREC02                                                         
         MVI   LP_RMODE,LP_RNEXT   RESET FIRST TIME CALL                        
         CLI   0(R1),NOQ           TEST RECORD SET IS WANTED                    
         BE    NXTREC08                                                         
         LTR   R3,R3               TEST RECORD KEY SAVE AREA PROVIDED           
         BZ    *+10                                                             
         MVC   0(L'IOKEY,R3),IOKEY YES - SAVE CURRENT RECORD KEY                
         XC    IOKEY,IOKEY                                                      
                                                                                
NXTREC02 GOTOR LP_ASETK,DMCB,(0,(R0)),IOKEY,(R4),('FF',LP_D)                    
         BH    NXTREC06                                                         
         LHI   R1,IODIR                                                         
         TM    BYTE1,$NXTRSTA      TEST I/O TO STATION FILE                     
         BZ    *+8                                                              
         LHI   R1,IOSTAFIL                                                      
         TM    BYTE1,$NXTRCTF      TEST I/O TO CONTROL FILE                     
         BZ    *+8                                                              
         LHI   R1,IOCTFILE                                                      
         TM    BYTE1,$NXTRTRF      TEST I/O TO TRAFFIC FILE                     
         BZ    *+8                                                              
         LHI   R1,IOTRFDIR                                                      
         TM    BYTE1,$NXTRXSP      TEST I/O TO XSPOT FILE                       
         BZ    *+8                                                              
         LHI   R1,IOXSPDIR                                                      
         TM    BYTE1,$NXTRGEN      TEST I/O TO GENERAL FILES                    
         BZ    *+8                                                              
         LHI   R1,IOGENDIR                                                      
* THROUGH TESTING I HAVE DISCOVERED, GETBUY TRASHES KEY                         
* ITS BETTER OFF NOT NOT TO CONVERT, AS ITS REALLY NOT NECESSARY                
*&&DO                                                                           
         CHI   R1,IODIR                                                         
         BNE   *+16                                                             
         CLI   IOKEY,X'10'                                                      
         BL    *+8                                                              
         MVI   GBYACT,GBYHIGH                                                   
*&&                                                                             
         AHI   R1,IOHI                                                          
         TM    BYTE1,$NXTRDEL      TEST DELETED RECORDS REQUIRED                
         BZ    *+8                                                              
         AHI   R1,IORDEL                                                        
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         MVI   GBYACT,0                                                         
         BE    *+14                                                             
         CLI   IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR LP_ASETK,DMCB,(1,(R0)),IOKEY,(R4),('FF',LP_D)                    
         BNE   NXTREC02                                                         
                                                                                
         LTR   RF,R5               TEST/SET DIRECTORY FILTER ROUTINE            
         BZ    *+12                                                             
         GOTOR GOFILT                                                           
         BNE   NXTREC02                                                         
                                                                                
         TM    BYTE1,$NXTRSTA+$NXTRCTF+$NXTRXGR                                 
         JNZ   EXITY                                                            
         LHI   R1,IOFIL                                                         
         TM    BYTE1,$NXTRTRF      TEST I/O TO TRAFFIC FILE                     
         BZ    *+8                                                              
         LHI   R1,IOTRFFIL                                                      
         TM    BYTE1,$NXTRXSP      TEST I/O TO XSPOT FILE                       
         BZ    *+8                                                              
         LHI   R1,IOXSPFIL                                                      
         TM    BYTE1,$NXTRGEN      TEST I/O TO GENERAL FILE                     
         BZ    *+8                                                              
         LHI   R1,IOGENFIL                                                      
                                                                                
         CLI   SV1OR2,0                                                         
         BE    NXTREC03                                                         
         CHI   R1,IOFIL                                                         
         BNE   NXTREC03                                                         
         CLI   IOKEY,X'10'                                                      
         BL    NXTREC03                                                         
         MVI   GBYACT,GBYGET                                                    
                                                                                
NXTREC03 AHI   R1,IOGET+IORDEL                                                  
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         MVI   GBYACT,0                                                         
         BE    NXTREC04                                                         
         TM    IOERR,IOEDEL        IGNORE DELETED RECORDS (SPILL)               
         BNZ   *+6                                                              
         DC    H'0'                DIE ON OTHER ERRORS                          
                                                                                
         LR    R1,R0               R1=A(KEY DRIVER TABLE)                       
         SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         BCTR  RE,0                RE=L'RECORD KEY-1                            
         L     RF,IOADDR           RF=A(RECORD)                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),0(RF)      TEST KEY MATCHES (ACTIVE POINTER)            
         BNE   NXTREC02            NO - IGNORE UNDELETED PASSIVES               
         TM    BYTE1,$NXTRDEL      TEST DELETED RECORDS WANTED                  
         BZ    NXTREC02                                                         
                                                                                
NXTREC04 LTR   RF,R6               SET/TEST FILE FILTER ROUTINE                 
         JZ    EXITY                                                            
         L     R1,IOADDR           PASS A(RECORD) IN R1                         
         GOTOR GOFILT                                                           
         BNE   NXTREC02            DIDN'T PASS FILTERS - GET NEXT               
         J     EXITY                                                            
                                                                                
NXTREC06 LTR   R3,R3               TEST ANY KEY SAVED                           
         BZ    NXTREC08                                                         
         MVC   IOKEY,0(R3)         YES - RESTORE IT                             
                                                                                
NXTREC08 MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS TO COME                  
         J     EXITN               EXIT WITH CC=NOT EQUAL TO CALLER             
                                                                                
GOFILT   NTR1  LABEL=NO            CALL RECORD FILTER ROUTINE                   
         L     RE,ALP                                                           
         LM    R2,RB,LP_R2RB-LP_D(RE)                                           
         BASR  RE,RF                                                            
         J     EXIT                                                             
         DROP  R2,RB                                                            
                                                                                
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
         DC    AL2(VDEMAND-WORKD,CDEMAND-COMFACSD)                              
         DC    AL2(VDEMOVAL-WORKD,CDEMOVAL-COMFACSD)                            
         DC    AL2(VSCANNER-WORKD,CSCANNER-COMFACSD)                            
         DC    AL2(VREPORT-WORKD,CREPORT-COMFACSD)                              
FACTABN  EQU   (*-FACTAB)/FACTABL                                               
                                                                                
FACTABD  DSECT                     ** DSECT TO COVER FACTAB ABOVE **            
FACTDOUT DS    AL2                 DISPLACEMENT TO OUTPUT ADDRESS               
FACTDIN  DS    AL2                 DISPLACEMENT TO INPUT ADDRESS                
FACTABL  EQU   *-FACTABD                                                        
SPLNK01  CSECT                                                                  
                                                                                
CORPHS   DS    0AL1                ** CORERES PHASES TO LOAD **                 
         DC    AL1(QFALINK)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(QSPGETBU)                                                    
         DC    AL1(QRCPACK)                                                     
         DC    AL1(QUNDAY)                                                      
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QTSAR)                                                       
         DC    AL1(QSTAVAL)                                                     
         DC    AL1(QGETRATE)                                                    
         DC    AL1(QGETBRD)                                                     
         DC    AL1(QCABLETB)                                                    
         DC    AL1(QMOBILE)                                                     
         DC    AL1(QQSORT)                                                      
         DC    AL1(QGETIDS)                                                     
         DC    AL1(QSPAUTH)                                                     
         DC    AL1(QGETDARE)                                                    
         DC    AL1(QCFMIO)                                                      
         DC    AL1(QDAYVAL)                                                     
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QBOOKVAL)                                                    
         DC    AL1(QUPVAL)                                                      
         DC    AL1(QGETDEM2)                                                    
         DC    AL1(QSPDEMUP)                                                    
         DC    AL1(QCENTER)                                                     
         DC    AL1(QSPSLNTB)                                                    
         DC    AL1(QGETBUY)                                                     
         DC    AL1(QBLDMGN)                                                     
CORPHSN  EQU   (*-CORPHS)/L'CORPHS                                              
                                                                                
FILTAB   DS    0X                  ** FILE DEFINITIONS **                       
                                                                                
         DC    AL1(IOSPTDIR/256),C'SPTDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOSPTFIL/256,13,01),AL2(18)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOTRFDIR/256),C'TRFDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOTRFFIL/256,13,01),AL2(18)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOXSPDIR/256),C'XSPDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOXSPFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENDIR/256),C'GENDIR '                                     
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(IOGENFIL/256,32,04),AL2(40)                                  
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOSPTFIL/256),C'SPTFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOSPTDIR/256,13,01),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOTRFFIL/256),C'TRFFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOTRFDIR/256,13,01),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOXSPFIL/256),C'XSPFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOXSPDIR/256,32,04),AL2(4000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOGENFIL/256),C'GENFIL '                                     
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(IOGENDIR/256,32,04),AL2(2000)                                
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOSTAFIL/256),C'STATION'                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,15,00),AL2(1000)                                           
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(IOCTFILE/256),C'CTFILE '                                     
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,29),AL2(2000)                                           
         DC    XL5'00'                                                          
                                                                                
         DC    AL1(0)                                                           
                                                                                
CMDTAB   DS    0X                  ** I/O COMMANDS **                           
                                                                                
*                                  INDEX SEQUENTIAL COMMANDS                    
CMDIS    DC    AL1(FILIIS,0),AL2(CMDISX+1-CMDIS)                                
         DC    C'DMRDHI ',AL1(IOHI,0,0)                                         
         DC    C'DMREAD ',AL1(IORD,0,0)                                         
         DC    C'DMRSEQ ',AL1(IOSQ,0,0)                                         
         DC    C'DMADD  ',AL1(IOADD,CMDIUPDT,0)                                 
         DC    C'DMWRT  ',AL1(IOWRITE,CMDIUPDT,0)                               
CMDISX   DC    AL1(0)                                                           
                                                                                
*                                  DIRECT ACCESS COMMANDS                       
CMDDA    DC    AL1(FILIDA,0),AL2(CMDDAX+1-CMDDA)                                
         DC    C'GETREC ',AL1(IOHI,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IORD,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOSQ,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOGET,CMDIDARQ,0)                                 
         DC    C'ADDREC ',AL1(IOADDREC,CMDIDADD+CMDIUPDT,0)                     
         DC    C'PUTREC ',AL1(IOPUTREC,CMDIDARQ+CMDIUPDT,0)                     
CMDDAX   DC    AL1(0)                                                           
                                                                                
CMDTABX  DC    AL1(0)                                                           
         EJECT                                                                  
*                                                                               
DDNDX    LKMMI H,SPTSYSQ           ** MASTER MAP INDEX **                       
                                                                                
         LKMMI D,I#SDINID,P#USASVR,(*,SDINILIT)                 (LNK12)         
         LKMMI D,I#SDMKTD,P#USASVR,(*,SDMKTLIT)                 (LNK12)         
         LKMMI D,I#SDCLTD,P#USASVR,(*,SDCLTLIT)                 (LNK12)         
         LKMMI D,I#SDBRED,P#USASVR,(*,SDBRELIT),RUNNER=B        (LNK12)         
                                                                                
         LKMMI D,I#SDBUYD,P#USASVR,(*,SDBUYLIT),RUNNER=B        (LNK12)         
         LKMMI D,I#NSDBYD,P#USASVR,(*,NSBDLLIT),RUNNER=B        (LNK12)         
         LKMMI D,I#SDNGDL,P#USASVR,(*,SDNGDLIT),RUNNER=B        (LNK12)         
         LKMMI D,I#STALST,P#USASVR,(*,SLSTDLIT),RUNNER=B        (LNK12)         
         LKMMI D,I#SDMLID,P#USASVR,(*,SDMLILIT),RUNNER=B        (LNK12)         
         LKMMI D,I#SDSLID,P#USASVR,(*,SDSLILIT),RUNNER=B        (LNK12)         
         LKMMI D,I#SDBUYR,P#USASVR,(*,SDSINLIT)                 (LNK12)         
                                                                                
         LKMMI D,I#SDMSTR,P#SDMSTR,(*,SDMSTLIT),RUNNER=B        (LNK12)         
         LKMMI D,I#SDPIDS,P#SDPIDS,(*,SPIDSLIT)                 (LNK12)         
                                                                                
         LKMMI D,I#PCCNFG,P#USASVR,(*,SPCCFLIT),RUNNER=B        (LNK12)         
                                                                                
         LKMMI D,I#SDMGED,P#OMSVR,(*,SDMGELIT),RUNNER=B         (LNK32)         
         LKMMI D,I#SDORDD,P#OMSVR,(*,SDORDLIT),RUNNER=B         (LNK32)         
         LKMMI D,I#SDORDL,P#OMSVR,(*,SDORLLIT),RUNNER=B         (LNK32)         
         LKMMI D,I#SDBOXD,P#SDBOXD,(*,SDBOXLIT),RUNNER=B        (LNK32)         
                                                                                
         LKMMI US,I#SDBYHD,P#SDBULD,(*,SDBHDLIT)                (LNK14)         
         LKMMI US,I#SDBADD,P#SDBULD,(*,SDBADLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBCHA,P#SDBULD,(*,SDBCHLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBDEL,P#SDBULD,(*,SDBDLLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBSKD,P#SDBULD,(*,SDBSKLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBSD,P#SDBULD,(*,SDBSDLIT),UPDATIVE=Y      (LNK14)         
         LKMMI US,I#SDBSPL,P#SDBULD,(*,SDBSPLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBSPC,P#SDBULD,(*,SDSPTLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBALC,P#SDBULD,(*,SDBALLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBMGD,P#SDBULD,(*,SDMKGLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBBYD,P#SDBULD,(*,SDBYDLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBPBD,P#SDBULD,(*,SDPBDLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBORB,P#SDBULD,(*,SDORBLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBPKG,P#SDBULD,(*,SDPKGLIT),UPDATIVE=Y     (LNK14)         
         LKMMI US,I#SDBSPS,P#SDBULD,(*,SDBSSLIT),UPDATIVE=Y     (LNK14)         
                                                                                
         LKMMI US,I#SDMGBH,P#SDBUL2,(*,SDMGBDLT)                (LNK15)         
         LKMMI US,I#SDMGAP,P#SDBUL2,(*,SDFAPLIT),UPDATIVE=Y     (LNK15)         
         LKMMI US,I#SDMGRJ,P#SDBUL2,(*,SDFRJLIT),UPDATIVE=Y     (LNK15)         
         LKMMI US,I#SDMGPP,P#SDBUL2,(*,SDFPPLIT),UPDATIVE=Y     (LNK15)         
         LKMMI US,I#SDMGSP,P#SDBUL2,(*,SDFSALIT),UPDATIVE=Y     (LNK15)         
                                                                                
         LKMMI D,I#SDSCMD,P#SDSCMD,(*,SDSCMLIT)                 (LNK32)         
         LKMMI D,I#SDBYRD,P#SDBYRD,(*,SDBYRLIT)                 (LNK32)         
         LKMMI D,I#SDSALD,P#SDSALD,(*,SDSALLIT)                 (LNK32)         
         LKMMI D,I#SDOCOD,P#SDOCOD,(*,SDOCOLIT)                 (LNK32)         
         LKMMI D,I#SDSCOD,P#SDOCOD,(*,SDSCOLIT)                 (LNK32)         
         LKMMI D,I#SDMCOD,P#SDOCOD,(*,SDMCOLIT)                 (LNK32)         
         LKMMI D,I#SDRD4D,P#SDRD4D,(*,RD4RDLIT)                 (LNK32)         
         LKMMI D,I#SDOMSD,P#SDOMSD,(*,SDOMSLIT)                 (LNK32)         
         LKMMI D,I#SDOMMD,P#SDOMMD,(*,SDOMMLIT),RUNNER=B        (LNK32)         
         LKMMI D,I#SDSOLD,P#SDSOLD,(*,SDSOLLIT),RUNNER=B        (LNK32)         
         LKMMI D,I#SDSUBV,P#SDSUBV,(*,SDSBVLIT)                 (LNK32)         
         LKMMI D,I#SDADBV,P#SDADBV,(*,SDABVLIT)                 (LNK32)         
         LKMMI D,I#SDSODD,P#SDSODD,(*,SDSODLIT),RUNNER=B        (LNK32)         
         LKMMI D,I#SDCFOC,P#SDCFOC,(*,SDCFCLIT),RUNNER=B        (LNK32)         
         LKMMI D,I#SDOOAR,P#SDOOAR,(*,SDOARLIT)                 (LNK32)         
         LKMMI D,I#SDOAPR,P#SDOAPR,(*,SDAPRLIT)                 (LNK32)         
         LKMMI D,I#SDOREJ,P#SDOREJ,(*,SDREJLIT)                 (LNK32)         
         LKMMI D,I#SDOSAP,P#SDOSAP,(*,SDSAPLIT)                 (LNK32)         
         LKMMI D,I#SDOAPY,P#SDOAPY,(*,SDAPYLIT)                 (LNK32)         
         LKMMI D,I#SDOOFC,P#SDOOFC,(*,SDOFCLIT)                 (LNK32)         
         LKMMI D,I#SDOUMD,P#SDOUMD,(*,SDUMDLIT),UPDATIVE=Y      (LNK32)         
                                                                                
         LKMMI UN,I#SDPERD,P#SDOMUN,(*,SDABVLIT)                (LNK16)         
         LKMMI UN,I#SDOSAV,P#SDOMUN,(*,SDOSVLIT),UPDATIVE=Y     (LNK16)         
         LKMMI UN,I#SDOMHD,P#SDOMUN,(*,SDOHDLIT),UPDATIVE=Y     (LNK16)         
         LKMMI UN,I#SDBCNF,P#SDOMUN,(*,SDBCFLIT),UPDATIVE=Y     (LNK16)         
         LKMMI UN,I#SDOEXS,P#SDOMUN,(*,SDOESLIT),UPDATIVE=Y     (LNK16)         
         LKMMI UN,I#SDMGCH,P#SDOMUN,(*,SDFCHLIT),UPDATIVE=Y     (LNK16)         
         LKMMI UN,I#SDMETA,P#SDMETA,(*,SMETALIT)                (LNK16)         
                                                                                
         LKMMI D,I#DEBOKL,P#DEBOKL,(*,DBOKLLIT)                 (LNK17)         
         LKMMI D,I#DEUPGV,P#DEUPGV,(*,DVPJLLIT)                 (LNK17)         
         LKMMI D,I#DEPRGD,P#DEPRGD,(*,DPRGDLIT),RUNNER=Y        (LNK17)         
         LKMMI D,I#DEMOLU,P#DEMOLU,(*,DEMODLIT),RUNNER=Y        (LNK17)         
         LKMMI D,I#PRPRGD,P#PDMOLU,(*,PDMODLIT),RUNNER=Y        (LNK17)         
         LKMMI D,I#DECTYL,P#DEMSVR,(*,DCTYLLIT)                 (LNK17)         
                                                                                
         LKMMI D,I#SDREVD,P#REVDLD,(*,SDRVDLIT),RUNNER=B        (LNK19)         
         LKMMI D,I#SDWRKD,P#REVDLD,(*,SDWKDLIT),RUNNER=B        (LNK19)         
         LKMMI D,I#SDRSDR,P#REVDLD,(*,SDRSRLIT)                 (LNK19)         
         LKMMI D,I#SDRLDR,P#REVDLD,(*,SDRLRLIT)                 (LNK19)         
         LKMMI D,I#SDWSDR,P#REVDLD,(*,SDWSRLIT)                 (LNK19)         
         LKMMI D,I#SDALDR,P#REVDLD,(*,SDALRLIT)                 (LNK19)         
                                                                                
         LKMMI D,I#SDDEVD,P#DEVDLD,(*,SDDDVLIT)                 (LNK19)         
         LKMMI UN,I#SDDWKH,P#DEVULD,(*,SDUDVLIT)                (LNK19)         
         LKMMI UN,I#SDDWDT,P#DEVULD,(*,SDUDDLIT)                (LNK19)         
         LKMMI UN,I#SDDWND,P#DEVULD,(*,SDUDELIT)                (LNK19)         
                                                                                
         LKMMI UN,I#SDRSKU,P#REVULD,(*,SDSKULIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDWSKU,P#REVULD,(*,SDWKULIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRSNM,P#REVULD,(*,SDRNMLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRSDT,P#REVULD,(*,SDRDTLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRSDM,P#REVULD,(*,SDRDMLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRSBK,P#REVULD,(*,SDRBKLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRSUP,P#REVULD,(*,SDRUPLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRCME,P#REVULD,(*,SDRCMLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRSNT,P#REVULD,(*,SDRNTLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRSCP,P#REVULD,(*,SDRCPLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRSND,P#REVULD,(*,SDRNDLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRSEN,P#REVULD,(*,SDRNELIT),UPDATIVE=Y     (LNK1A)         
                                                                                
         LKMMI UN,I#SDRLKU,P#REVULD,(*,SDLKULIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDAVKU,P#REVULD,(*,SDAKULIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLDT,P#REVULD,(*,SDLDTLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLPM,P#REVULD,(*,SDRPHLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLCM,P#REVULD,(*,SDLCMLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLNT,P#REVULD,(*,SDLNTLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLDM,P#REVULD,(*,SDLDMLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLSD,P#REVULD,(*,SDLSDLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLUP,P#REVULD,(*,SDLUPLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLSP,P#REVULD,(*,SDLSPLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLRT,P#REVULD,(*,SDLRTLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLHI,P#REVULD,(*,SDLHILIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLOR,P#REVULD,(*,SDLORLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLPK,P#REVULD,(*,SDLPKLIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLAA,P#REVULD,(*,SDLAALIT),UPDATIVE=Y     (LNK1A)         
         LKMMI UN,I#SDRLND,P#REVULD,(*,SDLNDLIT),UPDATIVE=Y     (LNK1A)         
                                                                                
         LKMMI UN,I#SDRXBH,P#REVXFR,(*,SDXBHLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXBD,P#REVXFR,(*,SDXBDLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXBS,P#REVXFR,(*,SDXBSLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXBC,P#REVXFR,(*,SDXBCLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXBI,P#REVXFR,(*,SDXBILIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXOD,P#REVXFR,(*,SDXODLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXOP,P#REVXFR,(*,SDXOPLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXSD,P#REVXFR,(*,SDXSDLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXSP,P#REVXFR,(*,SDXSPLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXUP,P#REVXFR,(*,SDXUPLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXOR,P#REVXFR,(*,SDXORLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXPK,P#REVXFR,(*,SDXPKLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXC2,P#REVXFR,(*,SDXC2LIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXPU,P#REVXFR,(*,SDXPULIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXRC,P#REVXFR,(*,SDXRCLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXAB,P#REVXFR,(*,SDXABLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXND,P#REVXFR,(*,SDXNDLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXCK,P#REVXFR,(*,SDXCKLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXLK,P#REVXFR,(*,SDXLKLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXDR,P#REVXFR,(*,SDXDRLIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDRXAA,P#REVXFR,(*,SDXAALIT),UPDATIVE=Y     (LNK1B)         
         LKMMI UN,I#SDMGAC,P#REVXFR,(*,SDMACLIT),UPDATIVE=Y     (LNK1B)         
                                                                                
         LKMMI D,I#COCAMB,P#CANSVR,(*,COCABLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#COOCAM,P#CANSVR,(*,COCAOLIT)                 (LNK22)         
         LKMMI D,I#COORDB,P#CANSVR,(*,COORBLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#COORDD,P#CANSVR,(*,COODELIT)                 (LNK22)         
         LKMMI D,I#COOWRK,P#CANSVR,(*,COOWSLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#COCONB,P#CANSVR,(*,COCOBLIT)                 (LNK22)         
         LKMMI D,I#COOSUM,P#CANSVR,(*,COOSULIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDMKMP,P#CANSVR,(*,CDMMPLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDSTMP,P#CANSVR,(*,CDSMPLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDDMMP,P#CANSVR,(*,CDDMPLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDPGGO,P#CANSVR,(*,CDPGGLIT),RUNNER=B        (LNK22)         
                                                                                
         LKMMI D,I#CDINID,P#CANSVR,(*,CDINILIT)                 (LNK22)         
         LKMMI D,I#CDCLTB,P#CANSVR,(*,CDCLTLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDMKTB,P#CANSVR,(*,CDMKTLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDSSTB,P#CANSVR,(*,CDSSTLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDNSTB,P#CANSVR,(*,CDNSTLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDSTAV,P#CANSVR,(*,CDSTVLIT)                 (LNK22)         
         LKMMI D,I#CDESTB,P#CANSVR,(*,CDESTLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDBUYD,P#CANSVR,(*,CDBUYLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDSBDL,P#CANSVR,(*,CDSBDLIT)                 (LNK22)         
         LKMMI D,I#CDSNDL,P#CANSVR,(*,CDSNDLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDSHWB,P#CANSVR,(*,CDSHWLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDGVPR,P#CANSVR,(*,CDGVPLIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDMGAN,P#CANSVR,(*,CDMGALIT),RUNNER=B        (LNK22)         
         LKMMI D,I#CDSDDM,P#CANSVR,(*,CDSDDLIT),UPDATIVE=Y      (LNK22)         
         LKMMI D,I#CDPRDB,P#CANSVR,(*,CDPRBLIT)                 (LNK22)         
         LKMMI D,I#CDDPTB,P#CANSVR,(*,CDDPBLIT)                 (LNK22)         
         LKMMI D,I#CDACTV,P#CANSVR,(*,CDACTLIT)                 (LNK22)         
         LKMMI D,I#CDORAC,P#CANSVR,(*,CDORALIT)                 (LNK22)         
         LKMMI D,I#DECAND,P#DEMSVR,(*,CDDLULIT),RUNNER=Y        (LNK22)         
         LKMMI D,I#DECSBL,P#DEMSVR,(*,CDSBLLIT)                 (LNK22)         
         LKMMI D,I#CDPIND,P#CANSVR,(*,CDPINLIT),RUNNER=B        (LNK22)         
                                                                                
         LKMMI UN,I#CDSBHD,P#CANBUP,(*,CDBHDLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDSDCD,P#CANBUP,(*,CDBDCLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDSBUY,P#CANBUP,(*,CDBLDLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDSSPT,P#CANBUP,(*,CDSPTLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDSCOM,P#CANBUP,(*,CDBLCLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDSCON,P#CANBUP,(*,CDBCILIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDSDMV,P#CANBUP,(*,CDDVLLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDSSPL,P#CANBUP,(*,CDSDVLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDNTEL,P#CANBUP,(*,CDNSVLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDTXEL,P#CANBUP,(*,CDTAXLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDCOS2,P#CANBUP,(*,CDCS2LIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDEBUY,P#CANBUP,(*,CDEBYLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDENET,P#CANBUP,(*,CDENWLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDPDMV,P#CANBUP,(*,CDPBDLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CDPSPL,P#CANBUP,(*,CDPBSLIT),UPDATIVE=Y     (LNK23)         
         LKMMI UN,I#CESTLK,P#CANBUP,(*,CESTLKUL),UPDATIVE=Y     (LNK23)         
                                                                                
         LKMMI UO,I#PNGCHG,P#CANPNG,(*,CPCHGLIT),UPDATIVE=Y     (LNK2B)         
         LKMMI UO,I#PNGCPY,P#CANPNG,(*,CPCPYLIT),UPDATIVE=Y     (LNK2B)         
         LKMMI UO,I#PNGDSP,P#CANPNG,(*,CPDSPLIT),UPDATIVE=Y     (LNK2B)         
         LKMMI UO,I#PNGASP,P#CANPNG,(*,CPASPLIT),UPDATIVE=Y     (LNK2B)         
         LKMMI UO,I#PNGEOR,P#CANPNG,(*,CPEORLIT),UPDATIVE=Y     (LNK2B)         
         LKMMI UO,I#PNGVPT,P#CANPNG,(*,CPVPTLIT),UPDATIVE=Y     (LNK2B)         
                                                                                
         LKMMI UN,I#COCAMP,P#CANOUP,(*,COCAMLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#COSTES,P#CANOUP,(*,COSESLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#COSTEL,P#CANOUP,(*,COSELLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#COPROG,P#CANOUP,(*,COPRGLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#CODEMO,P#CANOUP,(*,CODEMLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#CONPCT,P#CANOUP,(*,COPCTLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#COGOAL,P#CANOUP,(*,COGOLLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#COGFCT,P#CANOUP,(*,COFCTLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#COORDR,P#CANOUP,(*,COORDLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#COCONT,P#CANOUP,(*,COCONLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#COSPL,P#CANOUP,(*,COSPLLIT),UPDATIVE=Y      (LNK24)         
         LKMMI UN,I#COEOR,P#CANOUP,(*,COEORLIT),UPDATIVE=Y      (LNK24)         
         LKMMI UN,I#COMKMP,P#CANOUP,(*,COMMPLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#COSTMP,P#CANOUP,(*,COSMPLIT),UPDATIVE=Y     (LNK24)         
         LKMMI UN,I#CODMMP,P#CANOUP,(*,CODMPLIT),UPDATIVE=Y     (LNK24)         
                                                                                
         LKMMI D,I#MFMMED,P#MFMMED,(*,MFMMDLIT)                 (LNK10)         
         LKMMI D,I#MFMCLT,P#MFMCLT,(*,MFMCLLIT),RUNNER=B        (LNK10)         
         LKMMI D,I#MFMPRD,P#MFMPRD,(*,MFMPRLIT),RUNNER=B        (LNK10)         
         LKMMI D,I#MFMVEN,P#MFMVEN,(*,MFMPULIT),RUNNER=B        (LNK10)         
         LKMMI D,I#MFMMKT,P#MFMMKT,(*,MFMMKLIT),RUNNER=B        (LNK10)         
         LKMMI D,I#MFMMST,P#MFMMST,(*,MFMMSLIT),RUNNER=B        (LNK10)         
         LKMMI D,I#MVANDL,P#MVANDL,(*,MVANDLIT),RUNNER=B        (LNK10)         
                                                                                
         LKMMI D,I#CFMIDL,P#CFMINI,(*,SCFMILIT)                 (LNK10)         
         LKMMI D,I#CFMCDL,P#CFMCDL,(*,SCFMCLIT),RUNNER=B        (LNK10)         
                                                                                
         LKMMI D,I#MKTGDL,P#MKTGDL,(*,MKTGDLIT)                 (LNK10)         
         LKMMI D,I#CLTGDL,P#CLTGDL,(*,CLTGDLIT)                 (LNK10)         
         LKMMI D,I#TCMLDL,P#TCMLDL,(*,TCMLDLIT)                 (LNK10)         
         LKMMI D,I#THOUDL,P#TCMLDL,(*,THOUDLIT)                 (LNK10)         
         LKMMI D,I#TRCPDL,P#TRCPDL,(*,TRCPDLIT)                 (LNK10)         
         LKMMI D,I#T0PRDL,P#T0PRDL,(*,T0PRFLIT)                 (LNK10)         
         LKMMI D,I#TCLADL,P#TCLADL,(*,TCLASLIT)                 (LNK10)         
         LKMMI D,I#CTXTDL,P#CTXTDL,(*,CTXTLIT)                  (LNK10)         
         LKMMI D,I#CMLDL,P#TCMLDL,(*,TCMLDLIT)                  (LNK10)         
         LKMMI D,I#SLENVF,P#SLENVF,(*,SLENVLIT)                 (LNK10)         
         LKMMI D,I#SLENDL,P#SLENDL,(*,SLENDLIT)                 (LNK10)         
         LKMMI D,I#MKGSDL,P#MKTGDL,(*,MKGSDLIT)                 (LNK10)         
         LKMMI D,I#TCLTPH,P#MFMSVR,(*,TCLPHLIT)                 (LNK10)         
         LKMMI D,I#STXTDL,P#MFMSVR,(*,STXTDLIT)                 (LNK10)         
         LKMMI D,I#TPATDL,P#TPATDL,(*,TPATDLIT)                 (LNK10)         
         LKMMI D,I#CNTTDL,P#MFMSVR,(*,CNTTDLIT)                 (LNK10)         
         LKMMI D,I#FLGHDL,P#MFMSVR,(*,FLGHDLIT)                 (LNK10)         
         LKMMI D,I#MLSTDL,P#MFMSVR,(*,MLSTDLIT)                 (LNK10)         
         LKMMI D,I#AUTODL,P#MFMSVR,(*,TAUTODL)                  (LNK10)         
         LKMMI D,I#AUTOUP,P#MFMSVR,(*,TAUTOUP)                  (LNK10)         
         LKMMI D,I#AUTRUN,P#MFMSVR,(*,TAUTORUN)                 (LNK10)         
         LKMMI D,I#AUTSOO,P#MFMSVR,(*,TAUTOSOO)                 (LNK10)         
         LKMMI D,I#AUTEND,P#MFMSVR,(*,TAUTOEND)                 (LNK10)         
         LKMMI D,I#OFFCDL,P#MFMSVR,(*,TOFFCLIT)                 (LNK10)         
                                                                                
         LKMMI D,I#AMSRUN,P#MFMSVR,(*,TAMSRUN)                  (LNK10)         
         LKMMI D,I#AMSEND,P#MFMSVR,(*,TAMSEND)                  (LNK10)         
                                                                                
***      LKMMI D,I#INIDLD,P#INIDLD,(*,DLINILIT)                 (LNK11)         
***      LKMMI D,I#BUYWIZ,P#BUYWIZ,(*,DLWIZLIT)                 (LNK11)         
***      LKMMI D,I#BUYDLD,P#BUYDLD,(*,DLBUYLIT),RUNNER=B        (LNK11)         
                                                                                
         LKMMI U,I#SPTULD,P#SPTULD,(*,ULALCLIT)                 (LNK13)         
         LKMMI U,I#SPDULD,P#SPDULD,(*,ULSPDLIT)                 (LNK13)         
                                                                                
         LKMMI D,I#SDFUAD,P#FUASVR,(*,DLFUALIT),RUNNER=B        (LNK20)         
         LKMMI D,I#SDPGMD,P#FUASVR,(*,DLPGMLIT),RUNNER=B        (LNK20)         
                                                                                
         LKMMI UN,I#SDCLTU,P#CFMUP,(*,CFMCULIT),UPDATIVE=Y      (LNK25)         
         LKMMI UN,I#SDPRDU,P#CFMUP,(*,CFMPULIT),UPDATIVE=Y      (LNK25)         
                                                                                
         LKMMI UN,I#STCMLU,P#STRUP,(*,TCMLULIT),UPDATIVE=Y      (LNK26)         
         LKMMI UN,I#STPATU,P#STRUP,(*,TPATULIT),UPDATIVE=Y      (LNK26)         
                                                                                
         LKMMI US,I#SPGOLU,P#SPGOLU,(*,TTXTUGOL),UPDATIVE=Y     (LNK39)         
                                                                                
         LKMMI US,I#SPESTU,P#SPESTU,(*,TTXTUEST),UPDATIVE=Y     (LNK67)         
                                                                                
         LKMMI E                                                                
                                                                                
SDINILIT DC    C'Desktop Initial Download'                                      
SPCCFLIT DC    C'PCPak Configuration Download'                                  
SDMKTLIT DC    C'Desktop Market Download'                                       
SDCLTLIT DC    C'Desktop Client Download'                                       
SDBRELIT DC    C'Desktop Brand/Estimate Download'                               
SDBUYLIT DC    C'Desktop Old Buy Download'                                      
NSBDLLIT DC    C'Desktop New Buy Download'                                      
SDNGDLIT DC    C'Desktop New Goal Mkt Dates Download'                           
SLSTDLIT DC    C'Desttop Station List Download'                                 
SMETALIT DC    C'SuperDesk Metadata Download'                                   
SPIDSLIT DC    C'SuperDesk PIDS Download'                                       
SDMLILIT DC    C'Desktop Market Lockin Download'                                
SDSLILIT DC    C'Desktop Station Lockin Download'                               
SDSINLIT DC    C'Desktop Single Buy Download'                                   
SDMGELIT DC    C'Desktop Makegood Download'                                     
SDORDLIT DC    C'Desktop Order Download'                                        
SDBOXLIT DC    C'Desktop Batch Order Export Download'                           
SDMSTLIT DC    C'Desktop MStreet Station Download'                              
SDORLLIT DC    C'Desktop Locked Order Download'                                 
SDDDVLIT DC    C'Desktop Schedule Guidelines Download Response'                 
SDUDVLIT DC    C'Desktop Schedule Guidelines Upload'                            
SDUDDLIT DC    C'Desktop Schedule Guidelines Week Data'                         
SDUDELIT DC    C'Desktop End Schedule Guidelines Upload'                        
SDMGBDLT DC    C'Desktop Makegood Buy Header'                                   
SDBHDLIT DC    C'Desktop Buy Header'                                            
SDBADLIT DC    C'Desktop Buy Add'                                               
SDBCHLIT DC    C'Desktop Buy Change'                                            
SDBDLLIT DC    C'Desktop Buy Delete'                                            
SDBSKLIT DC    C'Desktop Buy Sked'                                              
SDBSDLIT DC    C'Desktop Buy Daily Sked'                                        
SDBSSLIT DC    C'Desktop Buy Separate Spot'                                     
SDBSPLIT DC    C'Desktop Buy Split'                                             
SDSPTLIT DC    C'Desktop Buy Spot Change'                                       
SDBALLIT DC    C'Desktop Buy Allocate'                                          
SDMKGLIT DC    C'Desktop Buy Makegood'                                          
SDBYDLIT DC    C'Desktop Buy Demos'                                             
SDPBDLIT DC    C'Desktop Post Buy Demos'                                        
SDORBLIT DC    C'Desktop Buy Orbits'                                            
SDPKGLIT DC    C'Desktop Buy Packages'                                          
SDSCMLIT DC    C'Desktop Standard Comment Download'                             
SDBYRLIT DC    C'Desktop Buyer Download'                                        
SDSALLIT DC    C'Desktop Rep Radio Person Download'                             
SDOCOLIT DC    C'Desktop OCOM Download'                                         
SDSCOLIT DC    C'Desktop SCOM Download'                                         
SDMCOLIT DC    C'Desktop MCOM Download'                                         
RD4RDLIT DC    C'Desktop Read For Routes Download'                              
SDOMSLIT DC    C'Desktop Single Order Download'                                 
SDOMMLIT DC    C'Desktop Multiple Order Download'                               
SDSOLLIT DC    C'Desktop Subscribed Order Download'                             
SDSBVLIT DC    C'Desktop Subscription Validation Download'                      
SDABVLIT DC    C'Desktop Personal Directory Validation'                         
SDSODLIT DC    C'Desktop Search Order Download'                                 
SDCFCLIT DC    C'Desktop Check For Changes'                                     
SDOARLIT DC    C'Desktop Order Activity Response'                               
SDAPRLIT DC    C'Desktop Offer Approve Response'                                
SDREJLIT DC    C'Desktop Offer Reject Response'                                 
SDSAPLIT DC    C'Desktop Offer Self-Apply Response'                             
SDAPYLIT DC    C'Desktop Offer Apply Response'                                  
SDOFCLIT DC    C'Desktop Offer Change Response'                                 
SDUMDLIT DC    C'Desktop Upload Makegood Demos'                                 
SDOSVLIT DC    C'Desktop Order Save/Xmit'                                       
SDOHDLIT DC    C'Desktop Order/Makegood Header'                                 
SDBCFLIT DC    C'Desktop Order Buyer-Confirm'                                   
SDMACLIT DC    C'Desktop Offer Action'                                          
SDFCHLIT DC    C'Desktop Offer Change'                                          
SDFAPLIT DC    C'Desktop Offer Approve'                                         
SDFRJLIT DC    C'Desktop Offer Reject'                                          
SDFPPLIT DC    C'Desktop Offer Apply'                                           
SDFSALIT DC    C'Desktop Offer Self-Apply'                                      
SDOESLIT DC    C'Desktop Order Export'                                          
SDRSRLIT DC    C'Desktop RevSheet Download Response'                            
SDRLRLIT DC    C'Desktop RevLine Download Response'                             
SDWSRLIT DC    C'Desktop WorkSheet Download Response'                           
SDALRLIT DC    C'Desktop Avail Download Response'                               
SDRVDLIT DC    C'Desktop Revision Download'                                     
SDWKDLIT DC    C'Desktop Work Download'                                         
SDSKULIT DC    C'Desktop Revision Sheet Key Upload'                             
SDWKULIT DC    C'Desktop Work Sheet Key Upload'                                 
SDRNMLIT DC    C'Desktop Revision/Work Name'                                    
SDRDTLIT DC    C'Desktop Revision/Work Data'                                    
SDRDMLIT DC    C'Desktop Revision/Work Demos'                                   
SDRBKLIT DC    C'Desktop Revision/Work Books'                                   
SDRUPLIT DC    C'Desktop Revision/Work Upgrades'                                
SDRCMLIT DC    C'Desktop Revision/Work Cumes'                                   
SDRNTLIT DC    C'Desktop Revision/Work Notes'                                   
SDRCPLIT DC    C'Desktop Revision/Work Copied From'                             
SDRNDLIT DC    C'Desktop End Revision/Work Header'                              
SDRNELIT DC    C'Desktop End Revision/Work Notes'                               
SDLKULIT DC    C'Desktop Desktop Revision Line Key Upload'                      
SDAKULIT DC    C'Desktop Desktop Avail Line Key Upload'                         
SDLDTLIT DC    C'Desktop RevLine/Avail Data'                                    
SDRPHLIT DC    C'Desktop Revision/Work Day/Time Period Override Data'           
SDLCMLIT DC    C'Desktop RevLine/Avail Comments'                                
SDLNTLIT DC    C'Desktop RevLine/Avail Notes'                                   
SDLDMLIT DC    C'Desktop RevLine/Avail Estimate Demos'                          
SDLSDLIT DC    C'Desktop RevLine/Avail Spill Demos'                             
SDLUPLIT DC    C'Desktop RevLine/Avail Upgrades'                                
SDLSPLIT DC    C'Desktop RevLine/Avail Spots'                                   
SDLRTLIT DC    C'Desktop RevLine/Avail Effective Rates'                         
SDLHILIT DC    C'Desktop RevLine/Avail Hiatus'                                  
SDLORLIT DC    C'Desktop RevLine/Avail Orbits'                                  
SDLPKLIT DC    C'Desktop RevLine/Avail Packages'                                
SDLAALIT DC    C'Desktop RevLine/Avail Auto-Avail UUID'                         
SDLNDLIT DC    C'Desktop RevLine/Avail End'                                     
SDXBHLIT DC    C'Desktop Revision Transfer Buy Header'                          
SDXABLIT DC    C'Desktop Revision XFR Buy Average Books'                        
SDXBDLIT DC    C'Desktop Revision XFR Buy Desc Data'                            
SDXBSLIT DC    C'Desktop Revision XFR Buy Spots'                                
SDXBCLIT DC    C'Desktop Revision XFR Buy Comments'                             
SDXBILIT DC    C'Desktop Revision XFR Buy Contract/ID'                          
SDXODLIT DC    C'Desktop Revision XFR Buy Orig Demos'                           
SDXOPLIT DC    C'Desktop Revision XFR Buy Orig Post Buy Demos'                  
SDXSDLIT DC    C'Desktop Revision XFR Buy Spill Demos'                          
SDXSPLIT DC    C'Desktop Revision XFR Buy Post Buy Spill Demos'                 
SDXUPLIT DC    C'Desktop Revision XFR Buy Upgrade Formula'                      
SDXORLIT DC    C'Desktop Revision XFR Buy Orbit Data'                           
SDXPKLIT DC    C'Desktop Revision XFR Buy Package Data'                         
SDXNDLIT DC    C'Desktop Revision XFR Buy Record END'                           
SDXC2LIT DC    C'Desktop Revision XFR Buy Cost 2'                               
SDXPULIT DC    C'Desktop Revision XFR Buy Purpose Code'                         
SDXRCLIT DC    C'Desktop Revision XFR Buy Reason Code'                          
SDXDRLIT DC    C'Desktop Revision XFR Buy DARE Trace'                           
SDXAALIT DC    C'Desktop Revision XFR Buy Auto-Avail UUID'                      
SDXCKLIT DC    C'Desktop Revision XFR Checksum Test'                            
SDXLKLIT DC    C'Desktop Revision XFR Locked Order Check'                       
CDINILIT DC    C'Canadian Desktop Initial Download'                             
CDCLTLIT DC    C'Canadian Desktop Client Browse'                                
CDMKTLIT DC    C'Canadian Desktop Market Browse'                                
CDSSTLIT DC    C'Canadian Desktop Selective Station Browse'                     
CDNSTLIT DC    C'Canadian Desktop Network Station Browse'                       
CDSTVLIT DC    C'Canadian Desktop Station Validation'                           
CDESTLIT DC    C'Canadian Desktop Estimate Browse'                              
CDBUYLIT DC    C'Canadian Desktop Buy Download'                                 
CDSBDLIT DC    C'Canadian Desktop Single Buy Download'                          
CDSNDLIT DC    C'Canadian Desktop Single Network Buy Download'                  
CDSHWLIT DC    C'Canadian Desktop Showcode Browse'                              
CDGVPLIT DC    C'Canadian Desktop GvP Report'                                   
CDPGGLIT DC    C'Canadian Desktop P&&G Goal Download'                           
CDMGALIT DC    C'Canadian Desktop Makegood Analysis'                            
CDSDDLIT DC    C'Canadian Desktop SHOWDEF/DEMODEF/DEMOVER Maint'                
CDPRBLIT DC    C'Canadian Desktop Product Browse'                               
CDDPBLIT DC    C'Canadian Desktop Daypart Browse'                               
CDDLULIT DC    C'Canadian Desktop Demo Look-Ups'                                
CDACTLIT DC    C'Canadian Desktop Date/Time Download'                           
CDORALIT DC    C'Canadian Dekstop Order Activity Download'                      
CDSBLLIT DC    C'Canadian Desktop Station Book List'                            
CDBHDLIT DC    C'Canadian Desktop Buy Header'                                   
CDBDCLIT DC    C'Canadian Desktop Buy Demo Codes'                               
CDBLDLIT DC    C'Canadian Desktop Buy Line Details'                             
CDSPTLIT DC    C'Canadian Desktop Spot Details'                                 
CDBLCLIT DC    C'Canadian Desktop Buy Line Comments'                            
CDBCILIT DC    C'Canadian Desktop Buy Contract ID'                              
CDDVLLIT DC    C'Canadian Desktop Buy Demo Values'                              
CDSDVLIT DC    C'Canadian Desktop Spill Demos'                                  
CDNSVLIT DC    C'Canadian Desktop Network Station'                              
CDTAXLIT DC    C'Canadian Desktop Tax Values'                                   
CDCS2LIT DC    C'Canadian Desktop Cost2 Values'                                 
CDEBYLIT DC    C'Canadian Desktop End Buy'                                      
CDENWLIT DC    C'Canadian Desktop End of Network'                               
CDPBDLIT DC    C'Canadian Desktop Post Buy Demos'                               
CDPBSLIT DC    C'Canadian Desktop Post Buy Spill Demos'                         
CESTLKUL DC    C'Canadian Desktop Estimate Lock/Unlock'                         
COCABLIT DC    C'Canadian Desktop Campaign Browse'                              
COCAOLIT DC    C'Canadian Desktop Campaign Open'                                
COORBLIT DC    C'Canadian Desktop Order Browse'                                 
COODELIT DC    C'Canadian Desktop Order Detail'                                 
COOWSLIT DC    C'Canadian Desktop Open Worksheet Download'                      
COCOBLIT DC    C'Canadian Desktop Contact Browse'                               
COOSULIT DC    C'Canadian Desktop Order Summary'                                
CDMMPLIT DC    C'Canadian Desktop Market Mapping'                               
CDSMPLIT DC    C'Canadian Desktop Station Mapping'                              
CDDMPLIT DC    C'Canadian Desktop Demo Category Mapping'                        
COCAMLIT DC    C'Canadian Desktop Campaign Upload'                              
COSESLIT DC    C'Canadian Desktop Station/Estimate Upload'                      
COSELLIT DC    C'Canadian Desktop Station List Upload'                          
COPRGLIT DC    C'Canadian Desktop Program Upload'                               
CODEMLIT DC    C'Canadian Desktop Demo Upload'                                  
COPCTLIT DC    C'Canadian Desktop Network% Upload'                              
COGOLLIT DC    C'Canadian Desktop Goal Upload'                                  
COFCTLIT DC    C'Canadian Desktop Goal Factor Upload'                           
COORDLIT DC    C'Canadian Desktop Order Upload'                                 
COCONLIT DC    C'Canadian Desktop Contact Upload'                               
COSPLLIT DC    C'Canadian Desktop Spill Upload'                                 
COEORLIT DC    C'Canadian Desktop End Of Record Upload'                         
CPCHGLIT DC    C'Canadian Desktop Pinergy Change Request'                       
CPCPYLIT DC    C'Canadian Desktop Pinergy Copy Request'                         
CPDSPLIT DC    C'Canadian Desktop Pinergy Delete Spots'                         
CPASPLIT DC    C'Canadian Desktop Pinergy Add Spots'                            
CPEORLIT DC    C'Canadian Desktop End Of Pinergy Request'                       
CPVPTLIT DC    C'Canadian Desktop Pinergy Validate Pointer'                     
COMMPLIT DC    C'Canadian Market Mapping Record Upload'                         
COSMPLIT DC    C'Canadian Station Mapping Record Upload'                        
CODMPLIT DC    C'Canadian Demo Cat. Mapping Rec Upload'                         
CDPINLIT DC    C'Canadian Desktop Pinergy Buy Download'                         
DBOKLLIT DC    C'Demo Book List'                                                
DVPJLLIT DC    C'Demo Validate Projection List'                                 
DPRGDLIT DC    C'DELNK Demo Lookup'                                             
DEMODLIT DC    C'SPLNK Demo Lookup'                                             
PDMODLIT DC    C'PRISMA Demo Lookup'                                            
MFMMDLIT DC    C'MFM Media Download'                                            
MFMCLLIT DC    C'MFM Client Download'                                           
MFMPRLIT DC    C'MFM Product Download'                                          
MFMPULIT DC    C'MFM Supplier Download'                                         
MFMMKLIT DC    C'MFM Market Download'                                           
MFMMSLIT DC    C'MFM M-Street Download'                                         
MVANDLIT DC    C'MediaVantage Download'                                         
DLINILIT DC    C'Canadian Steward Initial Download'                             
DLWIZLIT DC    C'Canadian Steward Buy Download Wizard'                          
DLBUYLIT DC    C'Canadian Steward Buy Download'                                 
ULALCLIT DC    C'Canadian Steward Spot Allocation Upload'                       
ULSPDLIT DC    C'Canadian Steward Spod Allocation Upload'                       
DLFUALIT DC    C'Film Usage Analysis Download Server'                           
DCTYLLIT DC    C'County Listing for a State'                                    
SCFMILIT DC    C'CFM Initial download'                                          
SCFMCLIT DC    C'CFM Client download'                                           
CFMCULIT DC    C'CFM Client Upload'                                             
CFMPULIT DC    C'CFM Product Upload'                                            
DLPGMLIT DC    C'Program Name Download Server'                                  
MKTGDLIT DC    C'Market Group Download'                                         
CLTGDLIT DC    C'Client Group Download'                                         
TCMLDLIT DC    C'Traffic - Commercial Download'                                 
THOUDLIT DC    C'Traffic - Production House Download'                           
TRCPDLIT DC    C'Traffic - Spot Ship Recap Download'                            
TCMLULIT DC    C'Traffic - Commercial Upload'                                   
T0PRFLIT DC    C'Traffic - Profile Records Download'                            
TCLASLIT DC    C'Traffic - Comclass Record Download'                            
TTXTULIT DC    C'Traffic - Commercial Text COMTEXT Upload'                      
CTXTLIT  DC    C'Traffic - Commercial Text COMTEXT Download'                    
SLENVLIT DC    C'Spot length verifier'                                          
SLENDLIT DC    C'Spot length download'                                          
MKGSDLIT DC    C'Market Group Scheme download'                                  
STXTDLIT DC    C'Special Text (STEXT) download'                                 
CNTTDLIT DC    C'Contact record download'                                       
FLGHDLIT DC    C'Flight record download'                                        
MLSTDLIT DC    C'Market list record download'                                   
TCLPHLIT DC    C'Traffic - Client Production House Download'                    
TPATULIT DC    C'Traffic - Pattern Upload'                                      
TPATDLIT DC    C'Traffic - Pattern Download'                                    
TAUTODL  DC    C'Traffic - Auto/gen Download'                                   
TAUTOUP  DC    C'Traffic - Auto/gen/Upload Selects'                             
TAUTOSOO DC    C'Traffic - Autogen/Soon'                                        
TAUTOEND DC    C'Traffic - Autogen/dummy'                                       
TAUTORUN DC    C'Traffic - Autogen/dummy req'                                   
TAMSRUN  DC    C'Traffic - Ams/Gen Req'                                         
TAMSUP   DC    C'Traffic - Ams/Sub'                                             
TAMSEND  DC    C'Traffic - Ams/Sub'                                             
TOFFCLIT DC    C'Office codes download'                                         
TTXTUGOL DC    C'Spot goals upload'                                             
TTXTUEST DC    C'Spot esthdr upload'                                            
                                                                                
                                                                                
* SPLNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
       ++INCLUDE GEMAPEQUS                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038SPLNK01   12/10/19'                                      
         END                                                                    
