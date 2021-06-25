*          DATA SET SRTOP00    AT LEVEL 006 AS OF 11/23/20                      
*PHASE T10500A                                                                  
SRTOP00  TITLE 'SRTOP - REMOTE PRINTER OUTPUT TRANSLATOR WITH RESTART'          
         PRINT NOGEN                                                            
SRTOP    CSECT                                                                  
         NMODL WRKX-WRKD,**$TOP**,RA,R9,R8,RR=R5                                
         USING WRKD,RC                                                          
         ST    R1,SAVR1            SAVE A(PARAM LIST)                           
         ST    R5,RELO                                                          
         L     R2,0(R1)            R2=A(SYS FAC LIST)                           
         USING SYSFACD,R2                                                       
         L     R7,12(R1)           R7=A(COM FAC LIST)                           
         USING COMFACSD,R7                                                      
         MVC   AGETRET,CGETRET     SAVE A(GET RETAIN DATE/TIME ROUT)            
         MVC   ADATCON,CDATCON                                                  
*                                                                               
         L     RE,VPRQENTS         SAVE ADR AND LEN OF PRQ ENTRIES              
         LA    RE,6(RE)                                                         
         ST    RE,APRQES                                                        
         LA    RE,L'PNTRY                                                       
         ST    RE,LPRQES-2                                                      
*                                                                               
         LA    RE,L'PQINDEX        SET AND SAVE PRTQUE DATA                     
         STH   RE,CINDXLN                                                       
         L     RE,VISGENQ                                                       
         ST    RE,FIWENQ                                                        
         L     R5,=A(CIREC-WRKD)   SET CORE/DISK ADDR OF CI REC                 
         LA    R5,WRKD(R5)                                                      
         ST    R5,ACIREC                                                        
         AH    R5,PQBLKLN                                                       
         ST    R5,ACIRECX          SAVE A(END OF BUFFER)                        
         XC    VGENIDS,VGENIDS                                                  
         XC    DCIREC,DCIREC                                                    
         MVC   PRTQID,PRTQUE                                                    
*                                                                               
         L     RE,VSSB             EXTRACT SSB DATA                             
         USING SSBD,RE                                                          
         MVI   XLAT,0              CLEAR CHARACTER SET FALGS                    
         TM    SSBSTAT6-SSBD(RE),SSB6ALLC                                       
         BZ    *+8                                                              
         OI    XLAT,X'01'          SET FACPAK USES ALL CHR TRANSLATE            
         MVC   RECLEN,SSBTWAL                                                   
         IC    RE,SSBSYSID                                                      
         SLL   RE,4                                                             
         STC   RE,SYSID            SET FACPAK SYSTEM ID                         
         B     INIT                                                             
         DROP  RE                                                               
*                                                                               
EXIT     L     R1,PBUFF            SET LENGTH OF DATA IN BUFFER HEADER          
         SR    R3,R1                                                            
         AHI   R1,-2                                                            
         TM    MODE+2,MODEDIB      TEST IF DATA MOVED TO BUFFER                 
         BO    *+6                                                              
         SR    R3,R3               NO - SET DATA LEN TO ZERO                    
         STH   R3,0(R1)                                                         
EXITX    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* SAVE PRINTER TERMINAL DATA                                          *         
***********************************************************************         
INIT     L     RE,SAVR1            POINT TO PRINTER UTL ENTRY                   
         L     RE,8(RE)                                                         
         USING UTLD,RE                                                          
         LA    RE,0(RE)                                                         
         ST    RE,APRNUTL          SAVE PRINTER DATA                            
         MVC   PNUM,TNUM                                                        
         MVC   PSTAT1(2),TSTAT1                                                 
         MVC   PSTAT3(3),TSTAT3                                                 
         MVC   PCTRY,TCTRY                                                      
         NI    TSTAT2,255-X'02'    TURN OFF FIRST TIME FLAG                     
         MVC   PTYPE,TTYPE                                                      
         MVC   PBUFF,TBUFF         SAVE PRINTER BUFFER ADDR                     
         MVI   APRNQ,0                                                          
         MVC   APRNQ+1(3),TPRNT    SAVE PRINTER QUEUE ADDR                      
         MVC   PRNSYM,TSYM         SAVE PRINTER SYMBOLIC ID                     
*                                                                               
         GOTO1 VTICTOC,DMCB,C'SGET'                                             
         MVC   TIMEHMS,DMCB        SAVE TIME AS P'0HHMMSS+'                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         TIME  BIN                 R0=TIME,R1=DATE                              
         STM   R0,R1,MVSTIME                                                    
         OI    MVSDATE+3,X'0F'                                                  
*                                                                               
         MVC   MAXNAKS,=H'20'      DEFAULT NAK COUNT (MAX VALUE 255)            
         XC    MODE,MODE                                                        
         XC    PAGNADR,PAGNADR     SET NO PAGE NUMBER IN BUFFER                 
         MVI   SVPREX,0                                                         
         MVI   ENQ,0               SET NO ENQUEUE OF PRTQUE                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,SHMUSS,ATTACH,MEMORY,0,0                           
         ICM   R1,15,DMCB+12       A(SHARED MEMORY)                             
         JZ    *+2                                                              
         ST    R1,FIWSHA                                                        
*                                                                               
         L     R6,APRNQ            R6=A(PRINTER QUEUE HEADER)                   
         USING PRQD,R6                                                          
         MVC   PRSVHMS,TIMEHMS     SET LAST ACTIVE TIME IN HEADER               
         MVC   SVSTAT(3),PRSTAT                                                 
         MVC   PATT1,PRQATTR       SAVE PRINTER ATTRIBUTES                      
         MVC   PATT2,PRQATT2                                                    
         MVC   PATT3,PRQATT3                                                    
         MVC   PATTR,PRQATTR       COPY OF FIRST ATTRIBUTE                      
         MVC   PESC,PRQESCN                                                     
         CLI   PESC,0              ..... IF VALUE IN NEW LOCATION               
         BNE   *+18                ..... THEN USE IT                            
         CLI   PRQESC,0            ..... ESC CHR USED TO BE HERE                
         BE    *+10                ..... NOW HOB OF 31 BIT A(UTL)               
         MVC   PESC,PRQESC         ..... DELETE THESE WHEN PERM                 
         MVC   MSHIFTS,PRQNE       SAVE MAXIMUM NUMBER OF SHIFTS                
         MVI   NSHIFTS,0                                                        
*                                                                               
         L     R5,ACIREC           R5=A(PRTQUE BLOCK)                           
         USING PQRECD,R5                                                        
*                                                                               
         L     R3,PBUFF            R3=A(NEXT PRINTER BUFFER LOCN)               
         MVC   PBUFFLEN,PRQBUFFL                                                
         OC    PRQBUFFL,PRQBUFFL                                                
         BNZ   *+10                                                             
         MVC   PBUFFLEN,LPRNBUFF   SET DEFAULT PRINTER BUFFER LEN               
*                                                                               
         CLI   PRPRTQA,0           TEST IF PRTQ ID IS KNOWN                     
         BE    *+10                                                             
         MVC   PRTQID+4(1),PRPRTQA                                              
         GOTO1 VDATAMGR,DMCB,(X'00',BUFFER),PRTQID,,,(R5)                       
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   PRTQADTF,4(R5)      FILE EXT NUM AND A(DTF)                      
         MVC   CIDATA,12(R5)       SAVE PRTQ FILE DATA                          
*                                                                               
         CLI   PRPRTQA,0           TEST IF PRTQ ID IS KNOWN                     
         BE    INIT3               NO-CANT POINT TO INDEXES YET                 
         SAM31                                                                  
         MVC   FIWRES,SPACES       SET RESOURCE                                 
         MVC   FIWRES(L'PRTQID),PRTQID                                          
         BRAS  RE,FIRSET           SET ADDRESSES FOR THIS RESOURCE              
         JNE   *+2                 INVALID PRTQ                                 
         BRAS  RE,FIRSLOCK         PUT A SHARED LOCK ON THE PRINT QUEUE         
         SAM24                                                                  
*                                                                               
INIT3    TM    PTYPE,TTYPERMC      REMOTE MINI COMPUTER                         
         BO    RMC                                                              
         TM    PTYPE,TTYPE327      BSC PRINTER                                  
         BO    BSC                                                              
         TM    PTYPE,TTYPEICC      TWX PRINTER                                  
         BO    TWX                                                              
         TM    PTYPE,TTYPE378      RJE PRINTER IBM 3780                         
         BO    RJE                                                              
                                                                                
***********************************************************************         
* REMOTE MINI COMPUTER SIMULATING A PRINTER                           *         
*                                                                     *         
* ALL BUFFERS START WITH A 10 BYTE CONTROL HDR MSG $$X2B..... WHERE   *         
*                                                                     *         
* $$           IDENTIFIES THIS BUFFER AS REMOTE PRINTER TYPE DATA     *         
* X            BUFFER TYPE CHR (SEE BELOW)                            *         
* 2            RELEASE NUMBER OF THIS VERSION OF HEADER               *         
* B            RELATIVE BUFFER NUMBER (0-9) OF LAST 10 BUFFERS SENT   *         
*                                                                     *         
* $$A20.....   FIRST BUFFER OF DAY                                    *         
* $$B2B.....   FIRST BUFFER OF SESSION                                *         
* $$C2B.....   NORMAL END OF REPORT                                   *         
* $$D2B.....   ABNORMAL END OF REPORT                                 *         
* $$E2B.....   NOTHING MORE TO DO THIS SESSION                        *         
* $$P2BINNNN   NORMAL DATA BUFFER - I=B'1111.RLF'                     *         
*              F=FIRST BUFFER OF REPORT BIT                           *         
*              L=LAST BUFFER OF REPORT BIT                            *         
*              R=RESTARTED REPORT BIT                                 *         
***********************************************************************         
RMC      OI    PATTR,ATTRPAG+ATTRCHK                                            
         MVC   MAXNAKS,=H'128'                                                  
         BAS   RE,CHKPNT                                                        
*                                                                               
RMCA     MVC   0(4,R3),BSCSOB      SET START OF BUFFER CONTROL CHRS             
         LA    R3,4(R3)                                                         
         MVC   0(L'MINIH,R3),MINIH SET START OF BUFFER HEADER                   
*                                                                               
RMCB     TM    PRSVSTAT,X'80'      TEST FIRST BUFFER OF DAY                     
         BO    RMCC                NO                                           
         OI    PRSVSTAT,X'80'      YES SET FIRST BUFF FLAG                      
         MVI   PRSVRBN,X'FF'       AND SET RELATIVE BUFF NUM                    
         XC    PRSVRBB,PRSVRBB                                                  
         MVI   2(R3),C'A'                                                       
         OI    MODE+2,MODEDIB+MODECTL                                           
         B     RMCI                                                             
*                                                                               
RMCC     CLI   PRSVXAC,0           TEST IF EXTERNAL ACTION SET                  
         BE    RMCH                                                             
         CLI   PRSVXAC,1           TEST RESHIP                                  
         BE    RMCD                                                             
         XC    PRSVXAC(4),PRSVXAC  CLEAR EXTERNAL DATA                          
         B     RMCH                                                             
*                                                                               
RMCD     TM    PRSVXACF,X'80'      TEST FIRST RESHIP REQUEST                    
         BO    RMCD1                                                            
         OI    PRSVXACF,X'80'      SET AND LOG FIRST RESHIP MESSAGE             
         MVI   2(R3),C'M'                                                       
         L     RF,=A(LOGMINI)                                                   
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
RMCD1    MVC   FULL(1),PRSVXAC1    READ IN REQUESTED BUFFER                     
         BAS   RE,BUFREAD                                                       
         BE    RMCD2                                                            
         XC    PRSVXAC(4),PRSVXAC  CLEAR EXTERNAL DATA IF ERROR                 
         B     RMCH                                                             
RMCD2    CLC   PRSVXAC1,PRSVRBN    TEST IF THIS RESHIP IS CURRENT               
         BNE   RMCD3               NO                                           
         XC    PRSVXAC(4),PRSVXAC  YES WE HAVE FINISHED RESHIP                  
         B     RMCD4                                                            
RMCD3    LLC   RE,PRSVXAC1         BUMP REQUESTED REL BUFF NUM                  
         LA    RE,1(RE)                                                         
         CH    RE,MAXRBN           TEST WRAP AT MAXIMUM                         
         BL    *+6                                                              
         SR    RE,RE                                                            
         STC   RE,PRSVXAC1                                                      
RMCD4    B     EXITX               EXIT WITH RESHIP BUFF IN CORE                
*                                                                               
RMCH     CLI   PRSTAT1,0           TEST FOR STAT1 ACTIONS                       
         BE    RMCI                                                             
*                                                                               
RMCH1    TM    PRSTAT1,PRS1SOS     TEST START OF SESSION                        
         BZ    RMCH2                                                            
         NI    PRSTAT1,255-PRS1SOS                                              
         MVI   2(R3),C'B'                                                       
         OI    MODE+2,MODEDIB+MODECTL                                           
         B     RMCI                                                             
*                                                                               
RMCH2    TM    PRSTAT1,PRS1EOS     TEST END OF SESSION PENDING                  
         BZ    RMCH3                                                            
         NI    PRSTAT1,255-PRS1EOS                                              
         MVI   2(R3),C'E'                                                       
         OI    MODE+2,MODEDIB+MODECTL                                           
         OI    PRSTAT1,PRS1EOSX    SET END OF SESSION DONE                      
         B     RMCI                                                             
*                                                                               
RMCH3    TM    PRSTAT1,PRS1ARS     TEST AUTO RESTART PENDING                    
         BZ    RMCH4                                                            
         NI    PRSTAT1,255-PRS1ARS                                              
         MVI   2(R3),C'B'                                                       
         OI    MODE+2,MODEDIB+MODECTL                                           
         OI    PRSTAT1,PRS1ARSX    SET AUTO RESTART DONE                        
         B     RMCI                                                             
*                                                                               
RMCH4    TM    PRSTAT1,PRS1MRS     TEST MANUAL RESTART PENDING                  
         BZ    RMCH5                                                            
         NI    PRSTAT1,255-PRS1MRS                                              
         MVI   2(R3),C'B'                                                       
         OI    MODE+2,MODEDIB+MODECTL                                           
         OI    PRSTAT1,PRS1MRSX    SET MANUAL RESTART DONE                      
         B     RMCI                                                             
*                                                                               
RMCH5    TM    PRSTAT1,PRS1MRSX+PRS1ARSX   TEST RESTART DONE                    
         BZ    RMCH6                                                            
         NI    PRSTAT1,255-PRS1MRSX                                             
         LA    RF,PAGNUM0                                                       
RMCH5A   TM    PRSTAT1,PRS1ARSX    RESTART PAGE FOR AUTO                        
         BZ    RMCH5B                                                           
         NI    PRSTAT1,255-PRS1ARSX                                             
RMCH5B   ST    RF,SAVRF            SAVE A(PAGE NUMBER ROUTINE)                  
         MVI   FULL,0                                                           
         CLC   PRADDR,PR1CIFST     TEST IF START OF REPORT                      
         BNE   RMCH5C              NO                                           
         LA    RE,PQDATA-PQINDEX                                                
         CH    RE,PRDISP                                                        
         BNE   RMCH5C                                                           
         MVI   FULL,X'01'          SET VALUE FOR FIRST PAGE                     
         XC    PRLNCTR,PRLNCTR     SET COUNTERS FOR START OF REPORT             
         XC    PRPAGES,PRPAGES                                                  
         XC    PRLINES,PRLINES                                                  
         XC    PRBUFFS,PRBUFFS                                                  
         MVI   PRLPP,0             SET START OF REPORT FLAG                     
RMCH5C   MVC   CIADDR,PR1CIFST     READ FIRST CI OF REPORT                      
         OC    CIADDR,CIADDR                                                    
         BZ    RMCI                                                             
         BAS   RE,READCI                                                        
         BNE   RMCI                                                             
         OI    FULL,X'04'          SET REMOTE MINI RESTART                      
         L     RF,=A(MINIDATA)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF               FORMAT REPORT EXTENDED HDR                   
         MVI   0(R3),NL                                                         
         MVI   1(R3),FF                                                         
         LA    R3,2(R3)                                                         
         L     RF,SAVRF                                                         
         BASR  RE,RF               SET PAGE NUMBER IN BUFFER                    
         OI    MODE+2,MODEDIB                                                   
         ST    R3,PBUFFSOD         SAVE START OF DATA IN BUFFER                 
         B     RMCJ                                                             
*                                                                               
RMCH6    TM    PRSTAT1,PRS1EOSX    TEST END OF SESSION DONE                     
         BZ    RMCI                                                             
         MVI   PRSTAT,0            SET RMC INACTIVE                             
         MVI   PRSTAT1,0                                                        
         L     RF,=A(CHKWRT)                                                    
         A     RF,RELO                                                          
         BASR  RE,RF               WRITE INACTIVE QUEUE TO DISK                 
         B     RMCS                CLOSE DEST AND EXIT                          
*                                                                               
RMCI     TM    MODE+2,MODEHDR      TEST IF EXTENDED HEADER                      
         BO    RMCI1               YES                                          
         LA    R3,L'MINIH(R3)      NO TERMINATE HDR WITH NL                     
         MVI   0(R3),NL                                                         
         LA    R3,1(R3)                                                         
RMCI1    ST    R3,PBUFFSOD         SAVE START OF DATA IN BUFFER                 
         TM    MODE+2,MODECTL                                                   
         BZ    RMCJ                                                             
         B     RMCX                EXIT IF CTL MSG                              
*                                                                               
RMCJ     TM    PNEX,PREXEOR        TEST END OF REPORT PENDING                   
         BO    RMCN                                                             
         TM    PRSTAT,PRSSP        TEST IF STOP NOW REQUESTED                   
         BO    RMCS1                                                            
         OC    PRCIADDR,PRCIADDR   TEST IF EMPTY QUEUE                          
         BNZ   RMCK                                                             
         TM    PRQMODE,X'80'       TEST IF AUTO MODE                            
         BZ    RMCS                                                             
         OC    PNSRCID,PNSRCID     TEST IF EMPTY FIRST QUEUE ENTRY              
         BZ    RMCS                                                             
         CLI   PRSTAT,0            TEST NULL STATUS                             
         BE    RMCS                                                             
         MVI   MODE+3,1            SET AUTO INIT MODE                           
         XC    MODE(2),MODE                                                     
         XC    MODE+4(4),MODE+4                                                 
         B     RMCR                FIND FIRST REPORT ENTRY                      
*                                                                               
RMCK     TM    PRSTAT,X'70'        TEST IF FLUSH REQUESTED                      
         BZ    RMCL                                                             
         L     R5,ACIREC           READ CURRENT CI REC                          
         MVC   CIADDR,PRADDR                                                    
         BAS   RE,READCI                                                        
         BNE   *+14                                                             
         CLC   PQSRCID,PR1KEY      CHECK REPORT MATCHES                         
         BE    RMCM                                                             
         OI    MODE+2,MODERER      SET ERROR END OF REPORT                      
         B     RMCM                GO TO END OF REPORT                          
*                                                                               
RMCL     BAS   RE,NXTLIN           GET NEXT PRINT LINE INTO P                   
         BZ    *+12                                                             
         OI    MODE+2,MODERER      SET ERROR END OF REPORT                      
         B     RMCM                                                             
         TM    FLINE,X'01'         TEST START OF REPORT                         
         BZ    RMCL1                                                            
         MVI   FULL,X'01'          SET REMOTE MINI START-OF-REPORT              
         L     RF,=A(MINIDATA)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BNZ   *+8                                                              
         ST    R3,PBUFFSOD         SAVE START OF DATA IN BUFFER                 
         TM    PATT2,ATTRAFP                                                    
         BZ    RMCL1                                                            
         LA    R1,1                                                             
         BAS   RE,PAGNUM1          SET PAGE<FORM> FOR AFP SHUTTLE               
*                                                                               
RMCL1    TM    FLINE,X'80'         TEST END OF REPORT                           
         BO    RMCM                                                             
         L     RF,=A(COMPRESS)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF               COMPRESS PRINT LINE DATA                     
*                                                                               
RMCL2    LH    R1,NUMNL            SET R1 TO TOTAL CHR COUNT                    
         CHI   R1,255                                                           
         BNE   RMCL2A                                                           
         LA    R1,9                ADJUST FOR PAGE NUMBERING                    
RMCL2A   AH    R1,NUMCHR                                                        
         BAS   RE,TSTFIT                                                        
         BL    RMCX                EXIT IF NO ROOM IN BUFFER                    
*                                                                               
RMCL3    LH    R1,NUMCHR           MOVE DATA CHRS TO BUFFER                     
         LTR   R1,R1                                                            
         BZ    RMCL3A                                                           
         MOVE  ((R3),(R1)),P                                                    
         AH    R3,NUMCHR                                                        
RMCL3A   CLI   NUMNL+1,X'FF'       MOVE CNTL CHRS TO BUFFER                     
         BNE   RMCL3B                                                           
         MVI   0(R3),NL                                                         
         MVI   1(R3),FF                                                         
         LA    R3,2(R3)                                                         
         BAS   RE,PAGNUM           SET PAGE NUMBER IN BUFFER                    
         B     RMCL4                                                            
RMCL3B   LH    R1,NUMNL            MOVE NL CTL CHRS TO BUFFER                   
         LTR   R1,R1                                                            
         BNP   RMCL4                                                            
         MVI   0(R3),NL                                                         
         CLI   LCC,X'01'           TEST FOR DATA AND SPACE ZERO                 
         BNE   RMCL3C                                                           
         MVI   0(R3),CR            USE CR INSTEAD OF NL                         
         LA    R3,1(R3)                                                         
         B     RMCL4                                                            
RMCL3C   AHI   R1,-2               REPLICATE REQUIRED NUM OF NL CHRS            
         BL    RMCL3D                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),0(R3)                                                    
RMCL3D   AHI   R1,2                                                             
         AR    R3,R1                                                            
*                                                                               
RMCL4    OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
         BAS   RE,BMPLAP           BUMP LINE AND PAGE PRINTED COUNTER           
         BNE   RMCL5                                                            
         LTR   R0,R0               NEW PAGE - R0 IS END PAGE NUMBER             
         BZ    RMCL5                                                            
         CR    R1,R0               R1 IS NEW PAGE NUMBER                        
         BNH   RMCL5                                                            
         OI    PNEX,PREXRPP        SET END OF PART PRINT FLAG                   
         B     RMCM                                                             
*                                                                               
RMCL5    LH    RE,PRDISP           BUMP TO NEXT PRINT LINE                      
         AH    RE,LLINE                                                         
         STH   RE,PRDISP                                                        
         CLI   NUMNL+1,X'FF'       SAVE HDR1 IF FIRST NEW PAGE                  
         BNE   RMCL                                                             
         OC    PRHDR1P(4),PRHDR1P                                               
         BNZ   RMCL                                                             
         MVC   PRHDR1P,PRHDR1                                                   
         B     RMCL                BACK FOR NEXT PRINT LINE                     
*                                                                               
RMCM     OC    PRLNCTR,PRLNCTR     END OF REPORT - SKIP TO TOP OF PAGE          
         BZ    RMCM4               ALREADY THERE                                
         CLI   PRLPP,X'80'         REPORT DOES NOT WANT SKIP TO TOP             
         BE    RMCM4                                                            
         LA    R1,3                SET FORMS FEED LENGTH                        
         BAS   RE,TSTFIT                                                        
         BL    RMCX                WONT FIT IN BUFFER                           
*                                                                               
RMCM2    OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
         XC    PRLNCTR,PRLNCTR                                                  
         MVI   0(R3),NL                                                         
         MVI   1(R3),FF                                                         
         MVI   2(R3),NL                                                         
         LA    R3,3(R3)                                                         
*                                                                               
RMCM4    OC    PRHDR1P(4),PRHDR1P  SAVE HDR1 IF FIRST NEW PAGE                  
         BNZ   *+10                                                             
         MVC   PRHDR1P,PRHDR1                                                   
         OI    PNEX,PREXEOR        SET END OF REPORT PENDING                    
         TM    MODE+2,MODERER                                                   
         BZ    RMCM4A                                                           
         OI    PNEX,PREXRER        SET END OF REPORT DUE TO ERROR               
         B     RMCX                                                             
RMCM4A   L     RE,PBUFF                                                         
         LA    RE,4(RE)            POINT TO START OF RMC HEADER                 
         OI    5(RE),X'02'         SET RMC END OF REPORT FLAG                   
         B     RMCX                                                             
*                                                                               
RMCN     MVC   SVPREX,PNEX         RESET END OF REPORT PENDING                  
         NI    PNEX,X'F0'                                                       
         TM    SVPREX,PREXRER      TEST ERROR END OF REPORT                     
         BO    RMCN3                                                            
**NOP**  BAS   RE,SETCPY           ONLY SINGLE COPY SENT TO SHUTTLE             
**NOP**  BNZ   RMCL                                                             
         TM    SVPREX,PREXRLU+PREXRPP                                           
         BNZ   RMCN2                                                            
         TM    PATT2,ATTRSSS       TEST SET SPECIAL STATUS                      
         BO    RMCN4                                                            
*                                                                               
RMCN1    MVI   STATY,X'01'         FULL PRINT                                   
         MVI   STANI,255-PQSTPG-PQSTAC-PQSTHO                                   
         MVI   STAOI,PQSTSE        SET REPORT STATUS TO SENT                    
         OI    MODE+2,MODEEOR      SET END OF REPORT MODE                       
         B     RMCN5                                                            
*                                                                               
RMCN2    MVI   STATY,X'02'         PART PRINT OR LINE UP                        
         MVI   STANI,255-PQSTPG                                                 
         MVI   STAOI,0             SET INDEX STATUS TO NOT PNTG                 
         OI    MODE+2,MODERPP      SET PARTIAL END OF REPORT                    
         B     RMCN5                                                            
*                                                                               
RMCN3    MVI   STATY,X'02'         ERROR REPORT                                 
         MVI   STANI,255-PQSTPG-PQSTAC-PQSTHO                                   
         MVI   STAOI,PQSTHO        SET INDEX STATUS TO HOLD                     
         OI    MODE+2,MODERPP      SET PARTIAL END OF REPORT                    
         B     RMCN5                                                            
*                                                                               
RMCN4    MVI   STATY,X'01'         FULL PRINT WITH SET SPECIAL STATUS           
         MVI   STANI,255-PQSTPG-PQSTAC-PQSTHO                                   
         MVI   STAOI,PQSTHO+PQSTSE SET REPORT STATUS TO HOLD                    
         OI    MODE+2,MODEEOR      SET END OF REPORT MODE                       
         B     RMCN5                                                            
*                                                                               
RMCN5    MVC   CIADDR,PRCIADDR     CHANGE REPORT STATUS                         
         MVC   STAID,PR1KEY        CHECK USER ID MATCHES                        
         BAS   RE,STAT                                                          
         BZ    RMCR                IGNORE ERRORS IN STATUS CHANGE               
*                                                                               
RMCR     BAS   RE,NXTREP           FIND NEXT REPORT FOR PRINTING                
         BNE   RMCERR              DISK ERROR                                   
         OC    CIADDR,CIADDR       TEST FOR END OF CURRENT QUEUE                
         BZ    RMCQ                                                             
         TM    RESULT+1,X'80'      TEST/SET REPORT IS LINE UP                   
         BZ    *+8                                                              
         OI    PNEX,PREXRLU                                                     
         XC    STAID,STAID         DONT CHECK USER ID                           
         MVI   STANI,255                                                        
         MVI   STAOI,PQSTPG                                                     
         MVI   STATY,X'02'                                                      
         BAS   RE,STAT             SET INDEX STATUS TO PRINTING                 
         BNZ   RMCERR                                                           
         BAS   RE,SETQUE           SET NEW QUEUE HEADER FOR REPORT              
*                                                                               
RMCR1    CLI   MODE+3,1            TEST FIRST REPORT FOR AUTO MODE              
         BNE   RMCR2                                                            
         MVI   MODE+3,0            YES - RESET FIRST AUTO FLAG                  
         MVI   MODE+2,0                                                         
         B     RMCL                BACK FOR FIRST REPORT DATA                   
*                                                                               
RMCR2    L     RE,PBUFF            SET END OF REPORT CONTROL BUFF               
         LA    RE,4(RE)                                                         
         TM    SVPREX,PREXEOR                                                   
         BZ    RMCR3                                                            
         MVI   2(RE),C'C'          SET NORMAL END OF REPORT                     
         TM    SVPREX,PREXRER                                                   
         BZ    *+8                                                              
         MVI   2(RE),C'D'          SET ABNORMAL END OF REPORT                   
         OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
*                                                                               
RMCR3    TM    PRSTAT,PRSSR        TEST STOP AT END OF REPORT                   
         BZ    RMCX                                                             
         B     RMCS1                                                            
*                                                                               
RMCQ     BAS   RE,NXTQUE           FIND NEXT QUEUE ENTRY                        
         BZ    RMCR                BACK FOR NEW QUEUE ENTRY                     
         L     RE,PBUFF            NO MORE ENTRYS                               
         LA    RE,4(RE)                                                         
         TM    SVPREX,PREXEOR      TEST END OF REPORT                           
         BO    RMCQ1                                                            
         MVI   2(RE),C'E'          SET END OF SESSION                           
         OI    MODE+2,MODEDIB+MODECTL                                           
         MVI   PRSTAT,0            SET RMC INACTIVE                             
         L     RF,=A(CHKWRT)                                                    
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     RMCX                                                             
RMCQ1    MVI   2(RE),C'C'          SET NORMAL END OF REPORT                     
         TM    SVPREX,PREXRER                                                   
         BZ    *+8                                                              
         MVI   2(RE),C'D'          SET ABNORMAL END OF REPORT                   
         OI    MODE+2,MODEDIB+MODECTL                                           
         MVI   PRSTAT,PRSACTV      SET RMC ACTIVE TO SEND LAST MSG              
         MVI   PRSTAT1,PRS1EOS                                                  
         B     RMCX                EXIT WITH END OF SESSION PENDING             
*                                                                               
RMCS     NI    PRSTAT,X'F0'        SET RMC LOGICALLY STOPPED                    
         BAS   RE,STOP             PHYSICALLY STOP DEVICE                       
         B     RMCX                                                             
RMCS1    NI    PRSTAT,X'F0'        SET RMC LOGICALLY STOPPED                    
         BAS   RE,STOP             PHYSICALLY STOP DEVICE                       
         B     RMCX                                                             
*                                                                               
RMCERFR  MVI   RESULT,X'01'        FILE CI READ ERROR                           
         B     RMCERR                                                           
RMCERFF  MVI   RESULT,X'04'        FILE CI FORMAT ERROR                         
         B     RMCERR                                                           
RMCERR   DC    H'0'                                                             
*                                                                               
RMCX     L     RE,PBUFF            SET BUFFER INFO IN RMC HEADER                
         LA    RE,4(RE)                                                         
*                                                                               
RMCX1    LLC   RF,PRSVRBN          BUMP RELATIVE BUFFER NUMBER                  
         LA    RF,1(RF)                                                         
         CH    RF,MAXRBN           TEST WRAP AT MAXIMUM                         
         BL    *+6                                                              
         SR    RF,RF                                                            
         STC   RF,PRSVRBN                                                       
         STC   RF,4(RE)            SET REL BUFF NUM AT RMCHDR+4                 
         OI    4(RE),X'F0'                                                      
         SLL   RF,1                                                             
         LA    RF,BUFBITS(RF)                                                   
         OC    PRSVRBB,0(RF)       SET REL BUFF BIT VALUE                       
*                                                                               
RMCX2    CLC   0(4,RE),MINIH       BUMP DATA BUFFER NUMBER                      
         BNE   RMCX3                                                            
         LH    RF,PRBUFFS                                                       
         LA    RF,1(RF)                                                         
         CHI   RF,10000            TEST WRAP AT 10000                           
         BL    *+6                                                              
         SR    RF,RF                                                            
         STH   RF,PRBUFFS                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  6(4,RE),DUB         SET DATA BUFF NUM AT RMCHDR+6                
*                                                                               
RMCX3    MVI   0(R3),EM            SET END OF BUFFER CONTROL CHRS               
         MVI   1(R3),ETX                                                        
         LA    R3,2(R3)                                                         
*                                                                               
         L     RF,=A(LOGMINI)      OUTPUT LOG DATA FOR RMC                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         MVC   FULL(1),PRSVRBN                                                  
         BAS   RE,BUFWRT           WRITE COPY OF BUFFER TO TEMPSTR              
         BE    EXIT                                                             
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BINARY SYNCHRONOUS PRINTER                                          *         
***********************************************************************         
BSC      L     R1,SAVR1            TEST IF LOADED BY PQTEST                     
         CLC   32(8,R1),=C'PQTSRTOP'                                            
         BNE   BSC0                                                             
         AHI   R1,-8                                                            
         CLC   0(8,R1),=C'PQTSRTOP'                                             
         BNE   BSC0                                                             
         NI    PATTR,255-ATTRCHK   NO CHECKPOINT IN PQTEST MODE                 
         B     *+8                                                              
BSC0     OI    PATTR,ATTRCHK       SET TO WRITE CHECKPOINT RECORD               
         MVI   CRCHR,CR            SET CARRIAGE RETURN CHR TO CR                
         TM    PATTR,ATTRNCR                                                    
         BZ    *+8                                                              
         MVI   CRCHR,NL            ELSE USE NL IF NOT SUPPORTED                 
*                                                                               
         TM    PRSTAT1,PRS1PAGE    DO NOT CHECKPOINT ON SECOND PASS             
         BO    *+8                                                              
         BAS   RE,CHKPNT                                                        
*                                                                               
         XC    TRDATA,TRDATA       INITIALISE TRACE DATA AREA                   
         MVC   TRSTAT(5),PRSTAT    COPY START VALUES TO TRACE DATA              
         MVC   TRSTAT3(1),PRSTAT3                                               
         MVC   TRNEX,PNEX                                                       
         MVC   TRKEY,PR1KEY                                                     
*                                                                               
BSCI     MVC   0(4,R3),BSCSOB      SET START OF BUFFER CONTROL CHRS             
         LA    R3,4(R3)                                                         
         ST    R3,PBUFFSOD         SAVE START OF DATA IN BUFFER                 
         CLI   PRSTAT1,0                                                        
         BE    BSCI1               NO STAT1 FLAGS                               
*                                                                               
BSCI0A   TM    PRSTAT1,PRS1SOS+PRS1ARS+PRS1MRS                                  
         BZ    BSCI0R                                                           
         TM    PRSTAT1,PRS1PAGE    TEST IF WE HAVE SKIPPED TO NEW PAGE          
         BO    BSCI0AX             YES                                          
         TM    PRSTAT1,PRS1SOS                                                  
         BZ    BSCI0A1                                                          
         TM    PATTR,ATTRNSK       TEST IF NO SKIP AT START OF SESSION          
         BZ    BSCI0A1             NO                                           
         OI    PRSTAT1,PRS1PAGE    YES SET THAT WE HAVE SKIPPED                 
         B     BSCI0AX                                                          
BSCI0A1  MVI   0(R3),FF            SKIP TO TOP OF PAGE                          
         MVC   1(1,R3),CRCHR                                                    
         LA    R3,2(R3)                                                         
         OI    PRSTAT1,PRS1PAGE    DONE SKIP TO TOP OF PAGE                     
         OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
         B     BSCX                                                             
BSCI0AX  EQU   *                                                                
*                                                                               
BSCI0R   TM    PRSTAT1,PRS1ARS+PRS1MRS  TEST MANUAL/AUTO RESTART                
         BZ    BSCI0S                                                           
         NI    PRSTAT1,255-PRS1MRS-PRS1PAGE                                     
         LA    RF,PAGNUM0                                                       
         ST    RF,SAVRF            SAVE A(PAGE NUMBER ROUTINE)                  
         TM    PRSTAT1,PRS1ARS     TEST AUTO RESTART                            
         BZ    *+8                                                              
         NI    PRSTAT1,255-PRS1ARS                                              
         CLC   PRADDR,PR1CIFST     TEST RESTART AT START OF REPORT              
         BNE   BSCI0X              NO                                           
         LA    RE,PQDATA-PQINDEX                                                
         CH    RE,PRDISP                                                        
         BNE   BSCI0X                                                           
         XC    PRLNCTR,PRLNCTR     SET COUNTERS FOR START OF REPORT             
         XC    PRPAGES,PRPAGES                                                  
         XC    PRLINES,PRLINES                                                  
         XC    PRBUFFS,PRBUFFS                                                  
         B     BSCI0X                                                           
*                                                                               
BSCI0S   TM    PRSTAT1,PRS1SOS     TEST START OF SESSION                        
         BZ    BSCI1                                                            
         L     RF,=A(ESCDATA)                                                   
         A     RF,RELO                                                          
         BASR  RE,RF               SET ESCAPE CHRS IF ANY                       
         NI    PRSTAT1,255-PRS1SOS-PRS1PAGE                                     
         LA    RF,PAGNUM                                                        
         ST    RF,SAVRF            SAVE A(PAGE NUMBER ROUTINE)                  
*                                                                               
BSCI0X   TM    PATTR,ATTRPAG       TEST PAGE NUMBERING                          
         BO    *+12                YES                                          
         LA    RF,PAGNUMNO         NO SAVE A(NO PAGE NUMBER ROUTINE)            
         ST    RF,SAVRF                                                         
         BCTR  R3,0                                                             
         CLI   0(R3),NL            DROP NL IF PRESENT                           
         BE    *+8                                                              
         LA    R3,1(R3)                                                         
         L     RF,SAVRF                                                         
         BASR  RE,RF               SET RESTART PAGE NUMBER IN BUFFER            
*                                                                               
BSCI1    TM    PNEX,PREXEOR        TEST END OF REPORT PENDING                   
         BO    BSCN                                                             
         TM    PRSTAT,PRSSP        TEST IF STOP NOW REQUESTED                   
         BO    BSCS1                                                            
         OC    PRCIADDR,PRCIADDR   TEST IF EMPTY QUEUE                          
         BNZ   BSCI2                                                            
         TM    PRQMODE,X'80'                                                    
         BZ    BSCS                DEACTIVATE FOR TEMP QUEUE ENTRYS             
         OC    PNSRCID,PNSRCID                                                  
         BZ    BSCS                                                             
         CLI   PRSTAT,0                                                         
         BE    BSCS                                                             
         MVI   MODE+3,1            SET AUTO INIT MODE                           
         XC    MODE(2),MODE                                                     
         XC    MODE+4(4),MODE+4                                                 
         B     BSCR                FIND FIRST REPORT ENTRY                      
*                                                                               
BSCI2    TM    PRSTAT,X'70'        TEST IF FLUSH REQUESTED                      
         BZ    BSCL                                                             
         L     R5,ACIREC           READ CURRENT CI REC                          
         MVC   CIADDR,PRADDR                                                    
         BAS   RE,READCI                                                        
         BNE   *+14                                                             
         CLC   PQSRCID,PR1KEY      CHECK REPORT MATCHES                         
         BE    BSCM                                                             
         OI    MODE+2,MODERER      SET ERROR END OF REPORT                      
         B     BSCM                GO TO END OF REPORT                          
*                                                                               
BSCL     BAS   RE,NXTLIN           GET NEXT PRINT LINE INTO P                   
         BZ    *+12                                                             
         OI    MODE+2,MODERER      SET ERROR END OF REPORT                      
         B     BSCM                                                             
BSCL0    TM    PR1FLAG,PR1FPP      TEST START OF PARTIAL PRINT REPORT           
         BZ    BSCL0C                                                           
         NI    PR1FLAG,255-PR1FPP                                               
BSCL0A   LA    RF,PAGNUM0          SET SPECIAL START PAGE NUMBER                
         CLC   PNSUBID(2),=H'1'                                                 
         BNE   BSCL0D                                                           
         LA    RF,PAGNUM1          SET PAGE NUMBER ONE                          
         B     BSCL0D                                                           
BSCL0C   TM    FLINE,X'01'         TEST ACTUAL START OF REPORT                  
         BZ    BSCL1                                                            
         LA    RF,PAGNUM1          SET PAGE NUMBER ONE                          
BSCL0D   TM    PATTR,ATTRPAG       TEST PAGE NUMBERS PRINTING                   
         BO    *+8                                                              
         LA    RF,PAGNUMNO                                                      
         ST    RF,SAVRF                                                         
BSCL0F   TM    PATT3,ATTRPSSR      SET POSTCRIPT START-OF-REPORT                
         BZ    BSCL0G                                                           
         L     RF,=A(PSSRDATA)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
BSCL0G   TM    SVSTAT1,PRS1SOS+PRS1ARS+PRS1MRS  ALREADY DONE PAGE NUM           
         BNZ   BSCL1                                                            
         CLI   CRCHR,NL            IF USING NL THEN CANT SET PAGE ONE           
         BE    BSCL1                                                            
         LA    R1,1                                                             
         L     RF,SAVRF                                                         
         BASR  RE,RF               SET PAGE ONE IN BUFFER                       
BSCL1    TM    FLINE,X'80'         TEST END OF REPORT                           
         BO    BSCM                                                             
*                                                                               
BSCL2    LH    R1,NUMNL            SET R1 TO TOTAL CHR COUNT                    
         CHI   R1,255                                                           
         BNE   BSCL2A                                                           
         LA    R1,3+6              NL/FF/CR/99999/NL                            
         TM    PATT2,ATTRAFP                                                    
         BZ    BSCL2A                                                           
         AHI   R1,7                AJUST FOR <AFPFM>                            
BSCL2A   AH    R1,NUMCHR                                                        
         BAS   RE,TSTFIT                                                        
         BL    BSCX                EXIT IF NO ROOM IN BUFFER                    
*                                                                               
BSCL3    LH    R1,NUMCHR           MOVE DATA CHRS TO BUFFER                     
         LTR   R1,R1                                                            
         BZ    BSCL3A                                                           
         MOVE  ((R3),(R1)),P                                                    
         AH    R3,NUMCHR                                                        
BSCL3A   CLI   NUMNL+1,X'FF'       MOVE CNTL CHRS TO BUFFER                     
         BNE   BSCL3B                                                           
         MVI   0(R3),NL                                                         
         MVI   1(R3),FF                                                         
         MVC   2(1,R3),CRCHR                                                    
         LA    R3,3(R3)                                                         
         CLC   NUMCHR,=H'132'      DROP NL IF FULL PRINT LINE                   
         BNE   BSCL3A1                                                          
         TM    PATT2,ATTR132       UNLESS OVERRIDE BY AT2=B                     
         BO    BSCL3A1                                                          
         AHI   R3,-3                                                            
         MVI   0(R3),FF                                                         
         MVC   1(1,R3),CRCHR                                                    
         LA    R3,2(R3)                                                         
BSCL3A1  LA    RF,PAGNUM           TEST IF PAGE NUMBER REQUIRED                 
         TM    PATTR,ATTRPAG                                                    
         BO    *+8                                                              
         LA    RF,PAGNUMNO                                                      
         BCTR  R3,0                                                             
         CLI   0(R3),NL            DROP NL IF PRESENT                           
         BE    *+8                                                              
         LA    R3,1(R3)                                                         
         BASR  RE,RF               SET PAGE NUMBER IN BUFFER                    
         B     BSCL4                                                            
BSCL3B   LH    R1,NUMNL            MOVE NL CTL CHRS TO BUFFER                   
         CLC   NUMCHR,=H'132'                                                   
         BNE   BSCL3B1             TEST FULL PRINT LINE                         
         TM    PATT2,ATTR132                                                    
         BO    BSCL3B1             AT2=B MEANS ALWAYS SEND NL                   
         AHI   R1,-1                                                            
         BNP   BSCL3B1                                                          
         MVI   0(R3),C' '          SET SP/NL/.. INSTEAD OF NL/NL/..             
         LA    R3,1(R3)                                                         
BSCL3B1  LTR   R1,R1                                                            
         BNP   BSCL4                                                            
         MVI   0(R3),NL                                                         
         CLI   LCC,X'01'           TEST FOR DATA AND SPACE ZERO                 
         BNE   BSCL3C                                                           
         MVI   0(R3),CR            USE CR INSTEAD OF NL                         
         LA    R3,1(R3)                                                         
         B     BSCL4                                                            
BSCL3C   AHI   R1,-2               REPLICATE REQUIRED NUM OF NL CHRS            
         BL    BSCL3D                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),0(R3)                                                    
BSCL3D   AHI   R1,2                                                             
         AR    R3,R1                                                            
*                                                                               
BSCL4    OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
         BAS   RE,BMPLAP           BUMP LINE AND PAGE PRINTED COUNTER           
         BNE   BSCL5                                                            
         LTR   R0,R0               NEW PAGE - R0 IS END PAGE NUMBER             
         BZ    BSCL4A                                                           
         CR    R1,R0               R1 IS NEW PAGE NUMBER                        
         BNH   BSCL5                                                            
         OC    PAGNADR,PAGNADR     TEST IF PAGE NUMBER IN BUFFER                
         BZ    *+8                                                              
         L     R3,PAGNADR          REMOVE REDUNDANT NEXT PAGE NUMBER            
         OI    PNEX,PREXRPP        SET END OF PART PRINT FLAG                   
         B     BSCM                                                             
BSCL4A   TM    PQATTB,PQATERR      TEST IF REPORT IS IN ERROR STATUS            
         BZ    BSCL5                                                            
         C     R1,MAXERRP          TEST MAX PAGES TO PRINT FOR ERROR            
         BNH   BSCL5                                                            
         OI    MODE+2,MODERER      SET ERROR END OF REPORT                      
         B     BSCM                                                             
*                                                                               
BSCL5    LH    RE,PRDISP           BUMP TO NEXT PRINT LINE                      
         AH    RE,LLINE                                                         
         STH   RE,PRDISP                                                        
         CLI   NUMNL+1,X'FF'       SAVE HDR1 IF FIRST NEW PAGE                  
         BNE   BSCL                                                             
         OC    PRHDR1P(4),PRHDR1P                                               
         BNZ   BSCL                                                             
         MVC   PRHDR1P,PRHDR1                                                   
         B     BSCL                BACK FOR NEXT PRINT LINE                     
*                                                                               
BSCM     TM    MODE+2,MODERER      END OF REPORT                                
         BZ    BSCM1                                                            
         BAS   RE,SETERR           MOVE ERROR MESSAGE TO BUFFER                 
*                                                                               
BSCM1    OC    PRLNCTR,PRLNCTR     SKIP TO TOP OF PAGE                          
         BZ    BSCM4               ALREADY THERE                                
         CLI   PRLPP,X'80'         REPORT DOES NOT WANT SKIP TO TOP             
         BE    BSCM4                                                            
         LA    R1,3                SET FORMS FEED LENGTH                        
         BAS   RE,TSTFIT                                                        
         BL    BSCX                WONT FIT IN BUFFER                           
*                                                                               
BSCM2    OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
         XC    PRLNCTR,PRLNCTR                                                  
         MVI   0(R3),NL                                                         
         MVI   1(R3),FF                                                         
         MVC   2(1,R3),CRCHR                                                    
         LA    R3,3(R3)                                                         
*                                                                               
BSCM4    OC    PRHDR1P(4),PRHDR1P  SAVE HDR1 IF FIRST NEW PAGE                  
         BNZ   *+10                                                             
         MVC   PRHDR1P,PRHDR1                                                   
         OI    PNEX,PREXEOR        SET END OF REPORT PENDING                    
         TM    MODE+2,MODERER                                                   
         BZ    *+8                                                              
         OI    PNEX,PREXRER        SET END OF REPORT DUE TO ERROR               
         B     BSCX                                                             
*                                                                               
BSCN     MVC   SVPREX,PNEX         RESET END OF REPORT PENDING                  
         NI    PNEX,X'F0'                                                       
         OI    TRFLAGS,X'80'       TRACE SET END OF REPORT                      
         TM    SVPREX,PREXRER      TEST ERROR END OF REPORT                     
         BO    BSCN3                                                            
         BAS   RE,SETCPY           TEST FOR MULTIPLE COPYS                      
         BNZ   BSCL                                                             
         TM    SVPREX,PREXRLU+PREXRPP                                           
         BNZ   BSCN2                                                            
         TM    PATT2,ATTRSSS       TEST SET SPECIAL STATUS                      
         BO    BSCN4                                                            
*                                                                               
BSCN1    MVI   STATY,X'01'         FULL PRINT                                   
         MVI   STANI,255-PQSTPG-PQSTAC-PQSTHO                                   
         MVI   STAOI,PQSTPR        SET REPORT STATUS TO PRTD                    
         OI    MODE+2,MODEEOR      SET END OF REPORT MODE                       
         B     BSCN5                                                            
*                                                                               
BSCN2    MVI   STATY,X'02'         PART PRINT OR LINE UP                        
         MVI   STANI,255-PQSTPG                                                 
         MVI   STAOI,0             SET INDEX STATUS TO NOT PNTG                 
         OI    MODE+2,MODERPP      SET PARTIAL END OF REPORT                    
         B     BSCN5                                                            
*                                                                               
BSCN3    MVI   STATY,X'01'         ERROR REPORT                                 
         MVI   STANI,255-PQSTPG-PQSTAC-PQSTHO                                   
         MVI   STAOI,PQSTHO        SET REPORT STATUS TO HOLD                    
         OI    MODE+2,MODERPP      SET PARTIAL END OF REPORT                    
         B     BSCN5                                                            
*                                                                               
BSCN4    MVI   STATY,X'01'         FULL PRINT WITH SET SPECIAL STATUS           
         MVI   STANI,255-PQSTPG-PQSTAC-PQSTHO                                   
         MVI   STAOI,PQSTHO+PQSTPR SET REPORT STATUS TO HOLD                    
         OI    MODE+2,MODEEOR      SET END OF REPORT MODE                       
         B     BSCN5                                                            
*                                                                               
BSCN5    MVC   CIADDR,PRCIADDR     CHANGE REPORT STATUS                         
         MVC   STAID,PR1KEY        CHECK USER ID MATCHES                        
         BAS   RE,STAT                                                          
         BZ    BSCR                IGNORE ERRORS IN STATUS CHANGE               
*                                                                               
BSCR     BAS   RE,NXTREP           FIND NEXT REPORT FOR PRINTING                
         BNE   BSCERR              DISK ERROR                                   
         OC    CIADDR,CIADDR       TEST FOR END OF CURRENT QUEUE                
         BZ    BSCQ                                                             
         TM    RESULT+1,X'80'      TEST/SET REPORT IS LINE UP                   
         BZ    *+8                                                              
         OI    PNEX,PREXRLU                                                     
         XC    STAID,STAID         DONT CHECK USER ID                           
         MVI   STANI,255                                                        
         MVI   STAOI,PQSTPG                                                     
         MVI   STATY,X'02'                                                      
         BAS   RE,STAT             SET INDEX STATUS TO PRINTING                 
         BNZ   BSCERR                                                           
         BAS   RE,SETQUE           SET NEW QUEUE HEADER FOR REPORT              
         OI    TRFLAGS,X'40'       TRACE SET NEW REPORT FOUND                   
*                                                                               
BSCR1    CLI   MODE+3,1            TEST FIRST REPORT FOR AUTO MODE              
         BNE   BSCR2                                                            
         MVI   MODE+3,0            YES - RESET FIRST AUTO FLAG                  
         MVI   MODE+2,0                                                         
         B     BSCL                BACK FOR FIRST REPORT DATA                   
*                                                                               
BSCR2    TM    PRSTAT,PRSSR        TEST STOP AT END OF REPORT                   
         BZ    BSCL                                                             
         B     BSCS1                                                            
*                                                                               
BSCQ     BAS   RE,NXTQUE           FIND NEXT QUEUE ENTRY                        
         BNZ   BSCS                NO MORE ENTRYS                               
         OI    TRFLAGS,X'20'       TRACE SET NEW QUEUE ENTRY                    
         B     BSCR                BACK FOR NEW QUEUE ENTRY                     
*                                                                               
BSCS     NI    PRSTAT,X'F0'        SET PRINTER LOGICALLY STOPPED                
         BAS   RE,STOP             PHYSICALLY STOP DEVICE                       
         B     BSCX                                                             
BSCS1    NI    PRSTAT,X'F0'        SET PRINTER LOGICALLY STOPPED                
         BAS   RE,STOP             PHYSICALLY STOP DEVICE                       
         B     BSCX                                                             
*                                                                               
BSCERFR  MVI   RESULT,X'01'        FILE CI READ ERROR                           
         B     BSCERR                                                           
BSCERFF  MVI   RESULT,X'04'        FILE CI FORMAT ERROR                         
         B     BSCERR                                                           
BSCERR   TM    PATT2,ATTRTRC       TEST IF TRACING PRINTER                      
         BZ    BSCERRX                                                          
         OI    TRFLAGS,X'01'       TRACE SET DEATH                              
         L     RF,=A(LOGPRNT)      OUTPUT TRACE DATA FOR PRINTER                
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
BSCERRX  DC    H'0'                                                             
*                                                                               
BSCX     LR    R1,R3               GET RELATIVE POSN IN BUFFER                  
         S     R1,PBUFF                                                         
         BM    *+12                DIE IF NEGATIVE                              
         CHI   R1,4096                                                          
         JH    *+2                 DIE IF > 4K                                  
*                                                                               
BSCX1    TM    PATT3,ATTRLAN       TEST IF LAN ATTACHED                         
         BZ    BSCX2                                                            
         LR    RE,R3                                                            
         AHI   RE,-3                                                            
         CLC   0(3,RE),=X'150C0D'  TEST IF <NL><FF><CR>                         
         BNE   BSCX2                                                            
         MVC   0(2,RE),=X'150D'    REPLACE BY <NL><CR>                          
         LA    R3,2(RE)                                                         
BSCX2    MVI   0(R3),EM            SET END OF BUFFER CONTROL CHRS               
         MVI   1(R3),ETX                                                        
         LA    R3,2(R3)                                                         
*                                                                               
         LH    RF,PRBUFFS          BUMP NUMBER OF BUFFERS                       
         LA    RF,1(RF)                                                         
         CHI   RF,10000            TEST WRAP AT 10000                           
         BL    *+6                                                              
         SR    RF,RF                                                            
         STH   RF,PRBUFFS                                                       
*                                                                               
         TM    PATT2,ATTRTRC       TEST IF TRACING PRINTER                      
         BZ    EXIT                                                             
         L     RF,=A(LOGPRNT)      OUTPUT TRACE DATA FOR PRINTER                
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TWX PRINTER TERMINAL                                                *         
***********************************************************************         
TWX      NI    PATTR,255-ATTRCHK-ATTRPAG                                        
         ST    R3,PBUFFSOD         SAVE START OF DATA IN BUFFER                 
*                                                                               
TWXI     TM    PSTAT2,X'02'        TEST FIRST TIME                              
         BZ    TWXI1                                                            
         MVI   0(R3),DC2           SET START OF BUFFER CONTROL CHRS             
         MVI   1(R3),ESC                                                        
         MVI   2(R3),C'3'          TRANSPARENT MODE ENABLE                      
         LA    R3,3(R3)                                                         
         MVC   0(L'TWXMSGS,R3),TWXMSGS                                          
         LA    R3,L'TWXMSGS(R3)                                                 
         MVI   0(R3),FF                                                         
         LA    R3,1(R3)                                                         
         OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
         ST    R3,PBUFFSOD         SAVE START OF DATA IN BUFFER                 
*                                                                               
TWXI1    TM    PNEX,PREXEOR        TEST END OF REPORT PENDING                   
         BO    TWXN                                                             
         TM    PRSTAT,PRSSP        TEST IF STOP NOW REQUESTED                   
         BO    TWXS1                                                            
         OC    PRCIADDR,PRCIADDR   TEST IF EMPTY QUEUE                          
         BNZ   TWXI2                                                            
         TM    PRQMODE,X'80'                                                    
         BZ    TWXS                DEACTIVATE FOR TEMP QUEUE ENTRYS             
         OC    PNSRCID,PNSRCID                                                  
         BZ    TWXS                                                             
         CLI   PRSTAT,0                                                         
         BE    TWXS                                                             
         MVI   MODE+3,1            SET AUTO INIT MODE                           
         XC    MODE(2),MODE                                                     
         XC    MODE+4(4),MODE+4                                                 
         B     TWXR                FIND FIRST REPORT ENTRY                      
*                                                                               
TWXI2    TM    PRSTAT,X'70'        TEST IF FLUSH REQUESTED                      
         BZ    TWXL                                                             
         L     R5,ACIREC           READ CURRENT CI REC                          
         MVC   CIADDR,PRADDR                                                    
         BAS   RE,READCI                                                        
         BNE   *+14                                                             
         CLC   PQSRCID,PR1KEY      CHECK REPORT MATCHES                         
         BE    TWXM                                                             
         OI    MODE+2,MODERER      SET ERROR END OF REPORT                      
         B     TWXM                GO TO END OF REPORT                          
*                                                                               
TWXL     BAS   RE,NXTLIN           GET NEXT PRINT LINE INTO P                   
         BZ    *+12                                                             
         OI    MODE+2,MODERER      SET ERROR END OF REPORT                      
         B     TWXM                                                             
         TM    FLINE,X'01'         TEST START OF REPORT                         
         BZ    TWXL1                                                            
         TM    PATTR,ATTRPAG       TEST PAGE NUMBERS PRINTING                   
         BZ    TWXL1                                                            
         TM    PSTAT2,X'02'        IF RESTART ALREADY BEEN DONE                 
         BO    TWXL1                                                            
         LA    R1,1                                                             
         BAS   RE,PAGNUM1          SET PAGE ONE IN BUFFER                       
TWXL1    TM    FLINE,X'80'         TEST END OF REPORT                           
         BO    TWXM                                                             
*                                                                               
TWXL2    LH    R1,NUMNL            SET R1 TO TOTAL CHR COUNT                    
         CHI   R1,255                                                           
         BNE   TWXL2A                                                           
         LA    R1,2                                                             
         TM    PATTR,ATTRPAG       ADJUST FOR PAGE NUMBERING                    
         BZ    *+8                                                              
         LA    R1,6(R1)                                                         
TWXL2A   AH    R1,NUMCHR                                                        
         BAS   RE,TSTFIT                                                        
         BL    TWXX                EXIT IF NO ROOM IN BUFFER                    
*                                                                               
TWXL3    LH    R1,NUMCHR           MOVE DATA CHRS TO BUFFER                     
         LTR   R1,R1                                                            
         BZ    TWXL3A                                                           
         MOVE  ((R3),(R1)),P                                                    
         AH    R3,NUMCHR                                                        
TWXL3A   CLI   NUMNL+1,X'FF'       MOVE CNTL CHRS TO BUFFER                     
         BNE   TWXL3B                                                           
         MVI   0(R3),NL                                                         
         MVI   1(R3),FF                                                         
         LA    R3,2(R3)                                                         
         TM    PATTR,ATTRPAG       TEST IF PAGE NUMBER REQUIRED                 
         BZ    *+8                                                              
         BAS   RE,PAGNUM           SET PAGE NUMBER IN BUFFER                    
         B     TWXL4                                                            
TWXL3B   LH    R1,NUMNL            MOVE NL CTL CHRS TO BUFFER                   
         LTR   R1,R1                                                            
         BNP   TWXL4                                                            
         MVI   0(R3),NL                                                         
         CLI   LCC,X'01'           TEST FOR DATA AND SPACE ZERO                 
         BNE   TWXL3C                                                           
         MVI   0(R3),CR            USE CR INSTEAD OF NL                         
         LA    R3,1(R3)                                                         
         B     TWXL4                                                            
TWXL3C   AHI   R1,-2               REPLICATE REQUIRED NUM OF NL CHRS            
         BL    TWXL3D                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),0(R3)                                                    
TWXL3D   AHI   R1,2                                                             
         AR    R3,R1                                                            
*                                                                               
TWXL4    OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
         BAS   RE,BMPLAP           BUMP LINE AND PAGE PRINTED COUNTER           
         BNE   TWXL5                                                            
         LTR   R0,R0               NEW PAGE - R0 IS END PAGE NUMBER             
         BZ    TWXL5                                                            
         CR    R1,R0               R1 IS NEW PAGE NUMBER                        
         BNH   TWXL5                                                            
         OI    PNEX,PREXRPP        SET END OF PART PRINT FLAG                   
         B     TWXM                                                             
*                                                                               
TWXL5    LH    RE,PRDISP           BUMP TO NEXT PRINT LINE                      
         AH    RE,LLINE                                                         
         STH   RE,PRDISP                                                        
         CLI   NUMNL+1,X'FF'       SAVE HDR1 IF FIRST NEW PAGE                  
         BNE   TWXL                                                             
         OC    PRHDR1P(4),PRHDR1P                                               
         BNZ   TWXL                                                             
         MVC   PRHDR1P,PRHDR1                                                   
         B     TWXL                BACK FOR NEXT PRINT LINE                     
*                                                                               
TWXM     TM    MODE+2,MODERER      END OF REPORT                                
         BZ    TWXM1                                                            
         BAS   RE,SETERR           MOVE ERROR MESSAGE TO BUFFER                 
*                                                                               
TWXM1    OC    PRLNCTR,PRLNCTR     SKIP TO TOP OF PAGE                          
         BZ    TWXM4               ALREADY THERE                                
         CLI   PRLPP,X'80'         REPORT DOES NOT WANT SKIP TO TOP             
         BE    TWXM4                                                            
         LLC   R1,PRLPP                                                         
         TM    PATTR,ATTRNFF       TEST NO FF SUPPORT                           
         BO    *+12                                                             
         LA    R1,2                SET FORMS FEED LENGTH                        
         B     *+8                                                              
         SH    R1,PRLNCTR          R1=NUM OF NL'S TO END PAGE                   
         BAS   RE,TSTFIT                                                        
         BL    TWXX                WONT FIT IN BUFFER                           
*                                                                               
TWXM2    OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
         XC    PRLNCTR,PRLNCTR                                                  
         MVI   0(R3),NL                                                         
         TM    PATTR,ATTRNFF       TEST NO FF SUPPORT                           
         BO    TWXM3                                                            
         MVI   1(R3),FF                                                         
         LA    R3,2(R3)                                                         
         B     TWXM4                                                            
*                                                                               
TWXM3    MVI   0(R3),NL            MOVE NL'S TO BUFFER                          
         AHI   R1,-2                                                            
         BL    TWXM3A                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),0(R3)                                                    
TWXM3A   AHI   R1,2                                                             
         AR    R3,R1                                                            
*                                                                               
TWXM4    OC    PRHDR1P(4),PRHDR1P  SAVE HDR1 IF FIRST NEW PAGE                  
         BNZ   *+10                                                             
         MVC   PRHDR1P,PRHDR1                                                   
         OI    PNEX,PREXEOR        SET END OF REPORT PENDING                    
         TM    MODE+2,MODERER                                                   
         BZ    *+8                                                              
         OI    PNEX,PREXRER        SET END OF REPORT DUE TO ERROR               
         B     TWXX                                                             
*                                                                               
TWXN     MVC   SVPREX,PNEX         RESET END OF REPORT PENDING                  
         NI    PNEX,X'F0'                                                       
         TM    SVPREX,PREXRER      TEST ERROR END OF REPORT                     
         BO    TWXN3                                                            
         BAS   RE,SETCPY           TEST FOR MULTIPLE COPYS                      
         BNZ   TWXL                                                             
         TM    SVPREX,PREXRLU+PREXRPP                                           
         BNZ   TWXN2                                                            
*                                                                               
TWXN1    MVI   STATY,X'01'         FULL PRINT                                   
         MVI   STANI,255-PQSTPG-PQSTAC-PQSTHO                                   
         MVI   STAOI,PQSTPR        SET REPORT STATUS TO PRTD                    
         OI    MODE+2,MODEEOR      SET END OF REPORT MODE                       
         B     TWXN4                                                            
*                                                                               
TWXN2    MVI   STATY,X'02'         PART PRINT OR LINE UP                        
         MVI   STANI,255-PQSTPG                                                 
         MVI   STAOI,0             SET INDEX STATUS TO NOT PNTG                 
         OI    MODE+2,MODERPP      SET PARTIAL END OF REPORT                    
         B     TWXN4                                                            
*                                                                               
TWXN3    MVI   STATY,X'02'         ERROR REPORT                                 
         MVI   STANI,255-PQSTPG-PQSTAC-PQSTHO                                   
         MVI   STAOI,PQSTHO        SET INDEX STATUS TO HOLD                     
         OI    MODE+2,MODERPP      SET PARTIAL END OF REPORT                    
         B     TWXN4                                                            
*                                                                               
TWXN4    MVC   CIADDR,PRCIADDR     CHANGE REPORT STATUS                         
         MVC   STAID,PR1KEY        CHECK USER ID MATCHES                        
         BAS   RE,STAT                                                          
         BZ    TWXR                IGNORE ERRORS IN STATUS CHANGE               
*                                                                               
TWXR     BAS   RE,NXTREP           FIND NEXT REPORT FOR PRINTING                
         BNE   TWXERR              DISK ERROR                                   
         OC    CIADDR,CIADDR       TEST FOR END OF CURRENT QUEUE                
         BZ    TWXQ                                                             
         TM    RESULT+1,X'80'      TEST/SET REPORT IS LINE UP                   
         BZ    *+8                                                              
         OI    PNEX,PREXRLU                                                     
         XC    STAID,STAID         DONT CHECK USER ID                           
         MVI   STANI,255                                                        
         MVI   STAOI,PQSTPG                                                     
         MVI   STATY,X'02'                                                      
         BAS   RE,STAT             SET INDEX STATUS TO PRINTING                 
         BNZ   TWXERR                                                           
         BAS   RE,SETQUE           SET NEW QUEUE HEADER FOR REPORT              
*                                                                               
TWXR1    CLI   MODE+3,1            TEST FIRST REPORT FOR AUTO MODE              
         BNE   TWXR2                                                            
         MVI   MODE+3,0            YES - RESET FIRST FLAG                       
         MVI   MODE+2,0                                                         
         B     TWXL                BACK FOR FIRST REPORT DATA                   
*                                                                               
TWXR2    TM    PRSTAT,PRSSR        TEST STOP AT END OF REPORT                   
         BZ    TWXL                                                             
         B     TWXS1                                                            
*                                                                               
TWXQ     BAS   RE,NXTQUE           FIND NEXT QUEUE ENTRY                        
         BNZ   TWXS                NO MORE ENTRYS                               
         B     TWXR                BACK FOR NEW QUEUE ENTRY                     
*                                                                               
TWXS     EQU   *                                                                
TWXS1    NI    PRSTAT,X'F0'        SET PRINTER LOGICALLY STOPPED                
         L     RE,APRNUTL                                                       
         USING UTLD,RE                                                          
         XC    TPRNT,TPRNT         TURN OFF PRINTER QUEUE                       
         MVC   0(L'TWXMSGX,R3),TWXMSGX                                          
         LA    R3,L'TWXMSGX(R3)                                                 
         MVI   0(R3),ESC                                                        
         MVI   1(R3),C'4'          TRANSPARENT MODE DISABLE                     
         MVI   2(R3),DC4                                                        
         LA    R3,3(R3)                                                         
         OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
         B     TWXX                                                             
*                                                                               
TWXERFR  MVI   RESULT,X'01'        FILE CI READ ERROR                           
         B     TWXERR                                                           
TWXERFF  MVI   RESULT,X'04'        FILE CI FORMAT ERROR                         
         B     TWXERR                                                           
TWXERR   DC    H'0'                                                             
*                                                                               
TWXX     B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* IBM 3780 RJE TERMINAL PRINTER                                     *           
*********************************************************************           
RJE      NI    PATTR,255-ATTRCHK                                                
         MVI   CRCHR,CR            SET CARRAIGE RETURN                          
         MVI   EOBCHR,ETB          SET NORMAL END OF BUFFER CHR                 
         BAS   RE,CHKPNT                                                        
*                                                                               
RJEA     MVI   0(R3),DLE           SET START OF BUFFER CONTROL CHRS             
         MVI   1(R3),STX                                                        
         LA    R3,2(R3)                                                         
         ST    R3,PBUFFSOD         SAVE START OF DATA IN BUFFER                 
*                                                                               
RJEB     TM    PRSVSTAT,X'80'      TEST FIRST ACTIVITY OF DAY                   
         BO    RJEC                NO                                           
         XC    PRSVDATA,PRSVDATA   YES INITIALISE SAVE DATA                     
         OI    PRSVSTAT,X'80'      AND SET INITIALISED FLAG                     
*                                                                               
RJEC     EQU   *                   CANT THINK OF ANYTHING ELSE                  
*                                                                               
RJEH     CLI   PRSTAT1,0           TEST STATUS 1 ACTIONS                        
         BE    RJEJ                                                             
*                                                                               
RJEH1    TM    PRSTAT1,PRS1SOS     TEST START OF SESSION                        
         BZ    RJEH2                                                            
         NI    PRSTAT1,255-PRS1SOS                                              
         LA    RF,PAGNUM                                                        
         ST    RF,SAVRF            SAVE A(PAGE NUMBER ROUTINE)                  
         B     RJEHX                                                            
*                                                                               
RJEH2    TM    PRSTAT1,PRS1ARS+PRS1MRS  TEST MANUAL/AUTO RESTART                
         BZ    RJEJ                                                             
         NI    PRSTAT1,255-PRS1MRS                                              
         LA    RF,PAGNUM0                                                       
         ST    RF,SAVRF            SAVE A(PAGE NUMBER ROUTINE)                  
         TM    PRSTAT1,PRS1ARS     TEST AUTO RESTART                            
         BZ    RJEH3                                                            
         NI    PRSTAT1,255-PRS1ARS                                              
*                                                                               
RJEH3    CLC   PRADDR,PR1CIFST     TEST RESTART AT START OF REPORT              
         BNE   RJEHX               NO                                           
         LA    RE,PQDATA-PQINDEX                                                
         CH    RE,PRDISP                                                        
         BNE   RJEHX                                                            
         XC    PRLNCTR,PRLNCTR     SET COUNTERS FOR START OF REPORT             
         XC    PRPAGES,PRPAGES                                                  
         XC    PRLINES,PRLINES                                                  
*                                                                               
RJEHX    MVI   0(R3),FF            SKIP TO TOP OF PAGE                          
         MVC   1(1,R3),CRCHR                                                    
         LA    R3,2(R3)                                                         
*                                                                               
RJEJ     TM    PNEX,PREXEOR        TEST END OF REPORT PENDING                   
         BO    RJEN                                                             
         TM    PRSTAT,PRSSP        TEST IF STOP NOW REQUESTED                   
         BO    RJES1                                                            
         OC    PRCIADDR,PRCIADDR   TEST IF EMPTY QUEUE                          
         BNZ   RJEK                                                             
         TM    PRQMODE,X'80'                                                    
         BZ    RJES                DEACTIVATE FOR TEMP QUEUE ENTRYS             
         OC    PNSRCID,PNSRCID                                                  
         BZ    RJES                                                             
         CLI   PRSTAT,0                                                         
         BE    RJES                                                             
         MVI   MODE+3,1            SET AUTO INIT MODE                           
         XC    MODE(2),MODE                                                     
         XC    MODE+4(4),MODE+4                                                 
         B     RJER                FIND FIRST REPORT ENTRY                      
*                                                                               
RJEK     TM    PRSTAT,X'70'        TEST IF FLUSH REQUESTED                      
         BZ    RJEK1                                                            
         L     R5,ACIREC           READ CURRENT CI REC                          
         MVC   CIADDR,PRADDR                                                    
         BAS   RE,READCI                                                        
         BNE   *+14                                                             
         CLC   PQSRCID,PR1KEY      CHECK REPORT MATCHES                         
         BE    RJEM                                                             
         OI    MODE+2,MODERER      SET ERROR END OF REPORT                      
         B     RJEM                GO TO END OF REPORT                          
*                                                                               
RJEK1    TM    PRSTAT2,PRS2RTR     TEST IF RETANSMIT REPORT                     
         BZ    RJEL                                                             
         NI    PRSTAT2,255-PRS2RTR                                              
         LH    R1,PRSVDATA+2       SET BACK SEQ NUM                             
         BCTR  R1,0                                                             
         STH   R1,PRSVDATA+2                                                    
         OC    PRHDR1F(4),PRHDR1F  TEST IF REPORT DATA AVAILABLE                
         BZ    RJEL                                                             
         MVC   PRHDR1,PRHDR1F      SET PRQ HDR TO RESTART REPORT                
         MVC   PRHDR1P,PRHDR1                                                   
         MVC   PRHDR1S,PRHDR1                                                   
*                                                                               
RJEL     BAS   RE,NXTLIN           GET NEXT PRINT LINE INTO P                   
         BZ    *+12                                                             
         OI    MODE+2,MODERER      SET ERROR END OF REPORT                      
         B     RJEM                                                             
         TM    FLINE,X'01'         TEST START OF REPORT                         
         BZ    RJEL1                                                            
         L     RF,=A(RJEHDR)                                                    
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     RJEL5                                                            
*                                                                               
RJEL1    TM    FLINE,X'80'         TEST END OF REPORT                           
         BO    RJEM                                                             
*                                                                               
RJEL2    LH    R1,NUMNL            SET R1 TO TOTAL CHR COUNT                    
         CHI   R1,255                                                           
         BNE   RJEL2A                                                           
         LA    R1,3                                                             
RJEL2A   AH    R1,NUMCHR                                                        
         BAS   RE,TSTFIT                                                        
         BL    RJEX                EXIT IF NO ROOM IN BUFFER                    
*                                                                               
RJEL3    LH    R1,NUMCHR           MOVE DATA CHRS TO BUFFER                     
         LTR   R1,R1                                                            
         BZ    RJEL4                                                            
         MOVE  ((R3),(R1)),P                                                    
         AH    R3,NUMCHR                                                        
         MVI   0(R3),CR                                                         
         MVI   1(R3),LF                                                         
         LA    R3,2(R3)                                                         
*                                                                               
RJEL4    OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
         BAS   RE,BMPLAP           BUMP LINE AND PAGE PRINTED COUNTER           
         BNE   RJEL5                                                            
         LTR   R0,R0               NEW PAGE - R0 IS END PAGE NUMBER             
         BZ    RJEL5                                                            
         CR    R1,R0               R1 IS NEW PAGE NUMBER                        
         BNH   RJEL5                                                            
         OI    PNEX,PREXRPP        SET END OF PART PRINT FLAG                   
         B     RJEM                                                             
*                                                                               
RJEL5    LH    RE,PRDISP           BUMP TO NEXT PRINT LINE                      
         AH    RE,LLINE                                                         
         STH   RE,PRDISP                                                        
         CLI   NUMNL+1,X'FF'       SAVE HDR1 IF FIRST NEW PAGE                  
         BNE   RJEL                                                             
         OC    PRHDR1P(4),PRHDR1P                                               
         BNZ   RJEL                                                             
         MVC   PRHDR1P,PRHDR1                                                   
         B     RJEL                BACK FOR NEXT PRINT LINE                     
*                                                                               
RJEM     TM    MODE+2,MODERER      END OF REPORT                                
         BZ    RJEM1                                                            
         BAS   RE,SETERR           MOVE ERROR MESSAGE TO BUFFER                 
*                                                                               
RJEM1    OC    PRLNCTR,PRLNCTR     SKIP TO TOP OF PAGE                          
         BZ    RJEM4               ALREADY THERE                                
         LA    R1,3                SET FORMS FEED LENGTH                        
         BAS   RE,TSTFIT                                                        
         BL    RJEX                WONT FIT IN BUFFER                           
*                                                                               
RJEM2    OI    MODE+2,MODEDIB      DATA HAS BEEN MOVED TO BUFFER                
         XC    PRLNCTR,PRLNCTR                                                  
         MVI   0(R3),NL                                                         
         MVI   1(R3),NL                                                         
         MVC   2(1,R3),CRCHR                                                    
         LA    R3,3(R3)                                                         
*                                                                               
RJEM4    OC    PRHDR1P(4),PRHDR1P  SAVE HDR1 IF FIRST NEW PAGE                  
         BNZ   *+10                                                             
         MVC   PRHDR1P,PRHDR1                                                   
         OI    PNEX,PREXEOR        SET END OF REPORT PENDING                    
         MVI   EOBCHR,ETX          SET END OF BUFF CHR FOR LAST                 
         TM    MODE+2,MODERER                                                   
         BZ    *+8                                                              
         OI    PNEX,PREXRER        SET END OF REPORT DUE TO ERROR               
         B     RJEX                                                             
*                                                                               
RJEN     MVC   SVPREX,PNEX         RESET END OF REPORT PENDING                  
         NI    PNEX,X'F0'                                                       
         TM    SVPREX,PREXRER      TEST ERROR END OF REPORT                     
         BO    RJEN3                                                            
         BAS   RE,SETCPY           TEST FOR MULTIPLE COPYS                      
         BNZ   RJEL                                                             
         TM    SVPREX,PREXRLU+PREXRPP                                           
         BNZ   RJEN2                                                            
*                                                                               
RJEN1    MVI   STATY,X'01'         FULL PRINT                                   
         MVI   STANI,255-PQSTPG-PQSTAC-PQSTHO                                   
         MVI   STAOI,PQSTSE        SET REPORT STATUS TO SENT                    
         OI    MODE+2,MODEEOR      SET END OF REPORT MODE                       
         B     RJEN4                                                            
*                                                                               
RJEN2    MVI   STATY,X'02'         PART PRINT OR LINE UP                        
         MVI   STANI,255-PQSTPG                                                 
         MVI   STAOI,0             SET INDEX STATUS TO NOT PNTG                 
         OI    MODE+2,MODERPP      SET PARTIAL END OF REPORT                    
         B     RJEN4                                                            
*                                                                               
RJEN3    MVI   STATY,X'02'         ERROR REPORT                                 
         MVI   STANI,255-PQSTPG-PQSTAC-PQSTHO                                   
         MVI   STAOI,PQSTHO        SET INDEX STATUS TO HOLD                     
         OI    MODE+2,MODERPP      SET PARTIAL END OF REPORT                    
         B     RJEN4                                                            
*                                                                               
RJEN4    MVC   CIADDR,PRCIADDR     CHANGE REPORT STATUS                         
         MVC   STAID,PR1KEY        CHECK USER ID MATCHES                        
         BAS   RE,STAT                                                          
         BZ    RJER                IGNORE ERRORS IN STATUS CHANGE               
*                                                                               
RJER     BAS   RE,NXTREP           FIND NEXT REPORT FOR PRINTING                
         BNE   RJEERR              DISK ERROR                                   
         OC    CIADDR,CIADDR       TEST FOR END OF CURRENT QUEUE                
         BZ    RJEQ                                                             
         TM    RESULT+1,X'80'      TEST/SET REPORT IS LINE UP                   
         BZ    *+8                                                              
         OI    PNEX,PREXRLU                                                     
         XC    STAID,STAID         DONT CHECK USER ID                           
         MVI   STANI,255                                                        
         MVI   STAOI,PQSTPG                                                     
         MVI   STATY,X'02'                                                      
         BAS   RE,STAT             SET INDEX STATUS TO PRINTING                 
         BNZ   RJEERR                                                           
         BAS   RE,SETQUE           SET NEW QUEUE HEADER FOR REPORT              
*                                                                               
RJER1    CLI   MODE+3,1            TEST FIRST REPORT FOR AUTO MODE              
         BNE   RJER2                                                            
         MVI   MODE+3,0            YES - RESET FIRST AUTO FLAG                  
         MVI   MODE+2,0                                                         
         B     RJEL                BACK FOR FIRST REPORT DATA                   
*                                                                               
RJER2    TM    PRSTAT,PRSSR        TEST STOP AT END OF REPORT                   
         BZ    RJEL                                                             
         B     RJES1                                                            
*                                                                               
RJEQ     BAS   RE,NXTQUE           FIND NEXT QUEUE ENTRY                        
         BNZ   RJES                NO MORE ENTRYS                               
         B     RJER                BACK FOR NEW QUEUE ENTRY                     
*                                                                               
RJES     BAS   RE,STOP             PHYSICALLY STOP DEVICE                       
RJES1    NI    PRSTAT,X'F0'        SET PRINTER LOGICALLY STOPPED                
         B     RJEX                                                             
*                                                                               
RJEERFR  MVI   RESULT,X'01'        FILE CI READ ERROR                           
         B     RJEERR                                                           
RJEERFF  MVI   RESULT,X'04'        FILE CI FORMAT ERROR                         
         B     RJEERR                                                           
RJEERR   DC    H'0'                                                             
*                                                                               
RJEX     MVC   0(1,R3),EOBCHR      SET END OF BUFF CONTROL CHR                  
         B     EXIT                DO NOT INCLUDE IN BUFF LENGTH                
         EJECT                                                                  
***********************************************************************         
* CHECKPOINT/RESTART - READ PRINTER QUEUE CHECKPOINT FROM DISK        *         
***********************************************************************         
CHKPNT   NTR1                                                                   
         MVI   RESULT,0            SET OK RESULT                                
         TM    PATTR,ATTRCHK       EXIT IF NO CHECK POINT DEFINED               
         BZ    CHKPX                                                            
*                                                                               
CHKP1    TM    PRSTAT2,PRS2RTB     RETANSMIT BUFFER REQUESTED                   
         BO    CHKP1A                                                           
         MVI   PRQNAKS,0                                                        
         B     CHKP2                                                            
CHKP1A   LA    R5,CXREC            READ TWA0 TO GET LAST COPY OF QUEUE          
         XC    DMCB+08(2),DMCB+8                                                
         MVC   DMCB+10(2),PNUM                                                  
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,,(R5)                               
         CLI   8(R1),0                                                          
         BNE   CHKP1C                                                           
CHKP1B   LA    R5,64(R5)           POINT TO SAVED VERSION OF QUEUE              
         LA    RE,PRQUTLA-PRHDR(R5)                                             
         CLC   1(2,RE),PNUM                                                     
         BNE   CHKP1C              CHECK AND RESTORE A(UTL)                     
         MVC   0(3,RE),APRNUTL+1                                                
         CLC   PRHDR2(6),PRHDR2-PRHDR(R5)                                       
         BNE   CHKP1C                                                           
         CLC   PRQNE,PRQNE-PRHDR(R5)                                            
         BNE   CHKP1C                                                           
         CLC   PNTRY(8),PNTRY-PRHDR(R5)                                         
         BNE   CHKP1C                                                           
         CLC   PNNEXT,PNNEXT-PRHDR(R5)                                          
         BNE   CHKP1C                                                           
         CLC   PNLAST,PNLAST-PRHDR(R5)                                          
         BNE   CHKP1C                                                           
         MVC   0(PRQDL,R6),0(R5)   RESTORE FIXED PART OF QUEUE                  
CHKP1C   NI    PRSTAT2,255-PRS2RTB                                              
         LLC   RF,PRQNAKS          BUMP RETRANSMIT COUNT                        
         LA    RF,1(RF)                                                         
         STC   RF,PRQNAKS                                                       
         CH    RF,MAXNAKS          CHECK MAX RETRANSMIT COUNT                   
         BNH   CHKP1X                                                           
         MVI   PRQNAKS,0                                                        
         MVI   PRSTAT,PRSERR       SET STOPPED DUE TO ERROR                     
         OI    PRSTAT,PRSSP                                                     
         MVI   RESULT,1            SET STOPPED RESULT                           
CHKP1X   B     CHKP5                                                            
*                                                                               
CHKP2    TM    PRSTAT1,PRS1ARS     TEST AUTO SYSTEM RESTART PENDING             
         BZ    CHKP3                                                            
         LA    R5,CXREC            READ TWA0 TO GET LAST COPY OF QUEUE          
         XC    DMCB+08(2),DMCB+8                                                
         MVC   DMCB+10(2),PNUM                                                  
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,,(R5)                               
         CLI   8(R1),0                                                          
         BNE   CHKP2C                                                           
CHKP2A   LA    R5,64(R5)           POINT TO SAVED VERSION OF QUEUE              
         LA    RE,PRQUTLA-PRHDR(R5)                                             
         CLC   1(2,RE),PNUM                                                     
         BNE   CHKP2C              CHECK AND RESTORE A(UTL)                     
         MVC   0(3,RE),APRNUTL+1                                                
         CLC   PRHDR2(6),PRHDR2-PRHDR(R5)                                       
         BNE   CHKP2C                                                           
         CLC   PRQNE,PRQNE-PRHDR(R5)                                            
         BNE   CHKP2C                                                           
         CLC   PNTRY(8),PNTRY-PRHDR(R5)                                         
         BNE   CHKP2C                                                           
         TM    PATT1,ATTRCHK       TEST IF PRINTER WANTS AUTO RESTART           
         BZ    CHKP2C              NO                                           
*                                                                               
CHKP2B   MVC   HALF,PNLAST         SAVE CURRENT NEXT ENTRY INFO                 
         MVC   HALF1,PNNEXT                                                     
         MVC   0(PRQDL,R6),0(R5)   COPY FIXED PART OF QUEUE FROM DISK           
         MVC   PNLAST,HALF                                                      
         MVC   PNNEXT,HALF1                                                     
         MVI   PRSTAT1,PRS1ARS     SET AUTO RESTART STILL PENDING               
         MVC   PRHDR1,PRHDR1S      COPY SAVED ENTRY TO CURRENT                  
         MVC   PRHDR1P,PRHDR1S                                                  
         B     CHKP4                                                            
*                                                                               
CHKP2C   MVI   PRSTAT1,0           RESET AUTO RESTART FLAG                      
         MVC   SVSTAT(3),PRSTAT                                                 
         B     CHKP4                                                            
*                                                                               
CHKP3    OC    PRHDR1P(4),PRHDR1P  DID PREV BUFF CONTAIN NEW PAGE               
         BZ    CHKP4                                                            
         MVC   PRHDR1S,PRHDR1P     YES - SAVE PREV BUFF DATA                    
*                                                                               
CHKP4    XC    PRHDR1P,PRHDR1P     SET PREV BUFF DID NOT HAVE NEW PAGE          
*                                                                               
CHKP5    L     RF,=A(CHKWRT)       WRITE PRINTER QUEUE TO DISK                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
CHKPX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* READ/WRITE OUTPUT BUFFER TO DISK - FULL (0-9) HAS REL BUFF NUM      *         
* BUFF NUMBER N WILL BE MAPPED TO TEMPSTR PAGE N+1                    *         
***********************************************************************         
BUFWRT   NTR1                                                                   
         LA    R5,CXREC            POINT TO 14K AREA FOR TEMPSTR REC            
         XC    DMCB(24),DMCB                                                    
         LR    RE,R5               CLEAR AREA AND SET 64 BYTE HDR               
         LH    RF,RECLEN                                                        
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   16(8,R5),=C'*PRTBUF*'                                            
         MVC   24(2,R5),PNUM                                                    
         MVC   26(1,R5),FULL                                                    
         MVI   27(R5),05                                                        
         MVC   28(4,R5),MVSDATE                                                 
         MVC   32(4,R5),MVSTIME                                                 
         MVC   36(8,R5),PRNSYM                                                  
*                                                                               
         LA    R5,64(R5)           POINT TO PRT SAVE AREA                       
         MVC   0(PRQDL,R5),PRHDR                                                
*                                                                               
         LA    R5,2048(R5)         POINT TO BUFF SAVE AREA                      
         L     RE,PBUFF                                                         
         LR    R1,R3               COMPUTE BUFFER LENGTH                        
         SR    R1,RE                                                            
         JNP   *+2                                                              
         STH   R1,0(R5)                                                         
         LA    R5,2(R5)                                                         
         LR    R0,R5                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE CORE BUFF TO TWA DISK COPY              
*                                                                               
         LA    R5,CXREC            POINT TO START OF RECORD                     
         LLC   RF,FULL             TEMPSTR PAGE IS RBN+1                        
         CH    RF,MAXRBN                                                        
         BNL   BUFWE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,DMCB+8                                                        
         MVC   DMCB+10(2),PNUM                                                  
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,,(R5)                                
         B     BUFWX                                                            
*                                                                               
BUFWE    MVI   DMCB+8,X'FF'        CANT WRITE BUFFER TO DISK                    
*                                                                               
BUFWX    CLI   DMCB+8,0            EXIT WITH CC=EQL FOR OK                      
         XIT1                                                                   
*                                                                               
BUFREAD  NTR1                                                                   
         LA    R5,CXREC            POINT TO 14K AREA FOR TEMPSTR REC            
         XC    DMCB(24),DMCB                                                    
         LLC   RF,FULL             TEMPSTR PAGE IS RBN+1                        
         CH    RF,MAXRBN                                                        
         BNL   BUFRE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,DMCB+8                                                        
         MVC   DMCB+10(2),PNUM                                                  
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,,(R5)                               
         CLI   8(R1),0                                                          
         BNE   BUFRX                                                            
*                                                                               
         CLC   16(8,R5),=C'*PRTBUF*'                                            
         BNE   BUFRE                                                            
         CLC   24(2,R5),PNUM                                                    
         BNE   BUFRE                                                            
         CLC   26(1,R5),FULL                                                    
         BNE   BUFRE                                                            
*                                                                               
         LA    R5,64(R5)           POINT TO PRQ SAVE AREA                       
         LA    R5,2048(R5)         POINT TO BUFF SAVE AREA                      
         L     RF,PBUFF                                                         
         AHI   RF,-2                                                            
         SR    R1,R1                                                            
         ICM   R1,3,0(R5)          GET BUFFER LENGTH                            
         STH   R1,DMCB+22          SET AT DMCB6+2(2)                            
         BZ    BUFRE                                                            
         LA    R1,2(R1)                                                         
         MOVE  ((RF),(R1)),(R5)    MOVE DISK COPY TO CORE BUFF                  
         B     BUFRX                                                            
*                                                                               
BUFRE    MVI   DMCB+8,X'FF'        CANT READ BUFF FROM TEMPSTR                  
*                                                                               
BUFRX    CLI   DMCB+8,0            EXIT WITH CC=EQL FOR OK                      
         XIT1                                                                   
*                                                                               
BUFBITS  DC    X'80004000200010000800040002000100'                              
         DC    X'00800040000000000000000000000000'                              
         EJECT                                                                  
***********************************************************************         
* READ/WRITE CONTROL INTERVAL RECORD WITH ADDR=CIADDR INTO CIREC      *         
***********************************************************************         
READCI   NTR1                                                                   
         MVI   DMCB+8,0                                                         
         CLC   DCIREC,CIADDR       TEST ALREADY IN CORE                         
         BE    READCI1                                                          
         MVC   DCIREC,CIADDR       SAVE DISK ADDR OF REC IN CIREC               
         L     RF,ACIREC                                                        
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),PRTQID,DCIREC,(RF)                  
READCI1  CLI   DMCB+8,0                                                         
         XIT1                                                                   
*                                                                               
WRITECI  NTR1                                                                   
         CLC   DCIREC,CIADDR       CHECK DISK ADDR HAS NOT CHANGED              
         JNE   *+2                                                              
         CLC   DCIREC,=X'00010100' CHECK DISK ADDR IS NOT FIRST ADDRESS         
         JE    *+2                                                              
         L     RF,ACIREC                                                        
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),PRTQID,DCIREC,(RF)                   
         CLI   DMCB+8,0                                                         
         XIT1                                                                   
                                                                                
***********************************************************************         
* SET STATUS OF REPORT AT CIADDR TO VALUE DEFINED BY STATUS FLAGS     *         
* SET MODE(2) TO THE REPORT NUMBER AND MODE+4(4) TO DATE/TIME         *         
* RESULT=X'00' OK                                                     *         
* RESULT=X'01' DISK READ ERROR ON CI REC                              *         
* RESULT=X'02' DISK WRITE ERROR ON CI REC                             *         
* RESULT=X'04' FORMAT ERROR ON REPORT                                 *         
* RESULT=X'10' INDEX ERROR                                            *         
***********************************************************************         
STAT     NTR1                                                                   
         MVC   SVCIADDR,CIADDR                                                  
         TM    STATY,X'01'         TEST FILE STATUS CHANGE                      
         BZ    STAT0                                                            
         TM    STAOI,PQSTDEAD      TEST IF SETTING TO PRTD/SENT                 
         BZ    STAT0                                                            
         BAS   RE,GETDATE          GET DATE AND TIME                            
         BAS   RE,GETTIME                                                       
*                                                                               
STAT0    L     R5,ACIREC           R5=A(CONTROL INTERVAL RECORD)                
         LA    R4,CIRECS           R4=A(LIST OF CI REC DISK ADDRS)              
         MVI   RESULT,0            SET RESULT OK                                
         XC    SVAGEI,SVAGEI                                                    
         OC    SVCIADDR,SVCIADDR   EXIT IF CALLER GAVE ZERO DISK ADDR           
         BZ    STATX                                                            
*                                                                               
STAT1    BAS   RE,READCI           READ NEXT CI REC                             
         BNE   STATERFR                                                         
*                                                                               
         OC    STAID,STAID         TEST TO CHECK USER ID                        
         BZ    *+14                                                             
         CLC   PQSRCID,STAID                                                    
         BNE   STATERFF                                                         
         CLI   PQSEQ,1             TEST FIRST CI OF REPORT                      
         BH    STAT1A                                                           
         MVC   DATEC,DATEN                                                      
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    *+10                                                             
         MVC   DATEC,DATEO                                                      
         MVC   CISRCID(8),PQKEY    SAVE REPORT KEY                              
         MVC   MODE(2),PQREPNO     SAVE REPORT SEQNUM                           
         MVC   MODE+4(2),PQAGELD   SAVE REPORT CREATE DATE                      
         MVC   MODE+6(2),PQAGELT   SAVE REPORT CREATE TIME                      
         MVC   SVAGEI,PQAGELD      SAVE REPORT AGE INFO                         
         TM    PQTYP1,PQTYNCD                                                   
         BO    STAT1A0                                                          
         GOTO1 ADATCON,DMCB,(2,MODE+4),(30,MODE+4)                              
*                                                                               
STAT1A0  CLC   PQSRCID,PUBMIN      DO NOT CHANGE PUBLIC REPORTS                 
         BL    *+14                                                             
         CLC   PQSRCID,PUBMAX                                                   
         BNH   STATX                                                            
         TM    PNEX,PREXGRP        DO NOT CHANGE GROUP ID REPORTS               
         BO    STATX                                                            
         TM    STATY,X'01'         TEST FILE STATUS CHANGE                      
         BZ    STAT2                                                            
*                                                                               
STAT1A   NC    PQSTAT,STANI        SET STATUS IN CI REC                         
         OC    PQSTAT,STAOI                                                     
         TM    STAOI,PQSTDEAD      TEST IF SETTING TO PRTD/SENT                 
         BZ    STAT1C                                                           
         TM    STAOI,PQSTHO        TEST IF SPECIAL PRTD/DEAD STATUS             
         BZ    *+8                                                              
         NI    PQSTAT,255-PQSTDEAD                                              
         CLI   PQSEQ,1             TEST FIRST CI OF REPORT                      
         BNH   STAT1B                                                           
         MVC   PQAGELD(7),SVAGEI   SET AGE INFO IN HIGH ORDER CIS               
         LA    RF,DATEN                                                         
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,DATEO                                                         
         MVC   PQAGEDD,0(RF)       SET DATE PRINTED                             
         B     STAT1C                                                           
STAT1B   LA    RF,DATEN                                                         
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,DATEO                                                         
         MVC   PQDATED,0(RF)       SET DATE/TIME PRINTED                        
         MVC   PQTIMED,TIMEB                                                    
         MVC   PQPRLOC,PRID        SET LOCATION/PRINTER PRINTED                 
         MVC   PQPRNUM,PRNUM                                                    
         MVC   PQPRSYM,PRNSYM      SET DEVICE SYMBOLIC ID                       
         IC    RE,PQPRCNT          BUMP NUMBER OF TIMES PRINTED                 
         LA    RE,1(RE)                                                         
         STC   RE,PQPRCNT                                                       
         TM    STAOI,PQSTHO        TEST IF SPECIAL PRTD/DEAD STATUS             
         BO    STAT1C                                                           
         MVC   DUB+0(3),DATEC      COMPUTE NEW RETAIN DATE/TIME                 
         MVC   DUB+3(2),PQRETND                                                 
         L     RF,=A(GETRETN)                                                   
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         MVC   PQAGEDD,DATEC                                                    
         MVC   PQAGERD(3),DUB1                                                  
         MVC   SVAGEI,PQAGELD      SAVE REPORT AGE INFO                         
*                                                                               
STAT1C   BAS   RE,WRITECI          WRITE BACK CI REC                            
         BNE   STATERFW                                                         
*                                                                               
STAT2    MVC   0(2,R4),CIADDR      SAVE CI ADDR IN LIST                         
         MVC   2(2,R4),=X'FFFF'                                                 
         LA    R4,2(R4)                                                         
*                                                                               
         TM    STATY,X'01'         TEST FILE STATUS CHANGE                      
         BZ    STAT3                                                            
         SR    RF,RF               EXTRACT LINK TO NEXT CI REC                  
         ICM   RF,3,PQCINEXT                                                    
         STH   RF,CIADDR                                                        
         LTR   RF,RF                                                            
         BNZ   STAT1               BACK FOR NEXT CI                             
*                                                                               
STAT3    LA    R4,CIRECS           SORT CI ADDR'S INTO ASCENDING ORDER          
         MVI   FLAG,X'00'                                                       
STAT3A   CLC   2(2,R4),=X'FFFF'                                                 
         BE    STAT3C                                                           
         CLC   2(2,R4),0(R4)       COMPARE NEXT ENTRY TO THIS                   
         BNL   STAT3B                                                           
         XC    2(2,R4),0(R4)       SWAP IF IN WRONG ORDER                       
         XC    0(2,R4),2(R4)                                                    
         XC    2(2,R4),0(R4)                                                    
         OI    FLAG,X'02'                                                       
STAT3B   LA    R4,2(R4)            BUMP TO NEXT CI ADDR                         
         B     STAT3A                                                           
STAT3C   TM    FLAG,X'02'          DONE IF NO SWAPS THIS PASS                   
         BO    STAT3                                                            
*                                                                               
         SAM31                                                                  
         LA    R4,CIRECS                                                        
STAT5    CLC   0(2,R4),=X'FFFF'    EXIT IF NO CI ADDRESS                        
         BE    STATX                                                            
*                                                                               
         MVC   CIADDR(2),0(R4)     READ INDEX ENTRY FOR CIADDR                  
         MVC   CIADDR+2(2),=X'0100'                                             
*                                                                               
         MVC   FIWCIA,CIADDR                                                    
         BRAS  RE,FIRCN            A(CI) TO A(INDEX) AND REPORT #               
         L     R5,FIWNDA            A(INDEX NODE)                               
         BRAS  RE,FIRC1            IS THIS A PART1                              
         BNE   *+12                                                             
         LA    R5,SI1NDX-SI1PAR(R5) A(PART1 INDEX)                              
         B     *+8                                                              
         LA    R5,SI2NDX-SI2PAR(R5) A(PART2 INDEX)                              
*                                                                               
         NC    PQSTAT,STANI        SET STATUS IN INDEX ENTRY                    
         OC    PQSTAT,STAOI                                                     
         TM    STAOI,PQSTDEAD      TEST IF SETTING TO PRTD/SENT                 
         BZ    STAT6                                                            
         TM    STAOI,PQSTHO        TEST IF SPECIAL PRTD/DEAD STATUS             
         BZ    *+8                                                              
         NI    PQSTAT,255-PQSTDEAD                                              
*                                                                               
STAT6    TM    STATY,X'01'         TEST FOR INDEX CHANGE ONLY                   
         BO    STAT7                                                            
         TM    STAOI,PQSTPG        IF SETTING INDEX TO PRINTING...              
         BZ    STAT6A                                                           
         MVI   PQAGERT,X'FE'       SET VALUE TO SHOW INDEX HAS SYSID            
         MVC   PQAGEDD(2),PNUM                                                  
         OC    PQAGEDD(1),SYSID                                                 
         B     STAT7                                                            
STAT6A   CLI   PQAGERT,X'FE'       IF TURNING OFF PRINTING IN INDEX...          
         BNE   STAT7                                                            
         TM    PQSTAT,PQSTPG                                                    
         BO    STAT7                                                            
         MVC   PQAGELD(7),SVAGEI   RESTORE INDEX AGE TIME FROM FILE             
*                                                                               
STAT7    TM    STATY,X'01'         TEST FILE STATUS CHANGE                      
         BZ    STAT7A                                                           
         TM    STAOI,PQSTDEAD      TEST IF SETTING TO PRTD/SENT                 
         BZ    STAT7A                                                           
         MVC   PQAGELD(7),SVAGEI   RESTORE INDEX AGE TIME FROM FILE             
STAT7A   EQU   *                                                                
*                                                                               
STAT8    LA    R4,2(R4)            BUMP TO NEXT CI ADDR IN LIST                 
         B     STAT5                                                            
*                                                                               
STATERFR OI    RESULT,X'01'        FILE CI READ ERROR                           
         B     STATX                                                            
STATERFW OI    RESULT,X'02'        FILE CI WRITE  ERROR                         
         B     STATX                                                            
STATERFF OI    RESULT,X'04'        FILE CI FORMAT ERROR                         
         B     STATX                                                            
STATERXR MVI   RESULT,X'10'        INDEX ERROR                                  
         B     STATX                                                            
*                                                                               
STATX    CLI   RESULT,0            EXIT WITH CC=EQL IF OK                       
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* FIND NEXT LINE IN REPORT - MOVE TO P AND SET LINEDATA               *         
* RESULT=X'00' OK                                                     *         
* RESULT=X'01' DISK READ ERROR ON CI REC                              *         
* RESULT=X'04' FORMAT ERROR ON REPORT                                 *         
***********************************************************************         
NXTLIN   NTR1                                                                   
NLIN0    MVI   RESULT,0            SET OK RESULT                                
         MVI   FLINE,0             SET LINE FLAG                                
*                                                                               
NLIN1    L     R5,ACIREC           READ CURRENT PRTQUE RECORD                   
         LR    R4,R5               R4=A(NEXT PRTQUE LINE)                       
         AH    R4,PRDISP                                                        
         MVC   CIADDR,PRADDR                                                    
         BAS   RE,READCI                                                        
         BNE   NLINERFR                                                         
         CLC   PQSRCID,PR1KEY      CHECK REPORT MATCHES                         
         BNE   NLINERFF                                                         
         CLI   PRLPP,0             TEST FOR FIRST RECORD IN CI                  
         BNE   NLIN3                                                            
         CLI   PQSEQ,1             TEST FOR FIRST RECORD IN REPORT              
         BH    NLIN2                                                            
         OI    FLINE,X'01'         SET FIRST LINE IN REPORT FLAG                
*                                                                               
NLIN2    MVC   PRLPP,PQLPP         BUILD CI DATA IN QUEUE HEADER                
         CLI   PRLPP,X'80'                                                      
         BNE   *+8                                                              
         NI    PATTR,255-ATTRPAG   NO PAGE NUMBERS IF NO PAGES                  
         MVC   PRSEQ,PQSEQ                                                      
         SR    RE,RE                                                            
         ICM   RE,3,PRADDR                                                      
         LH    R0,CITRKS                                                        
         CLM   RE,3,CJSTTRK                                                     
         BL    *+8                                                              
         LH    R0,CJTRKS                                                        
         AR    RE,R0                                                            
         AHI   RE,-1                                                            
         STH   RE,PRCIHIGH                                                      
         MVC   PRCINEXT,PQCINEXT                                                
         LA    RE,PQDATA-PQINDEX                                                
         CLC   PRADDR,PRCIADDR     TEST IF FIRST REC IN FIRST CI                
         BNE   NLIN2A                                                           
         LA    RE,PQDATA1-PQINDEX                                               
         CLC   PQDATA1(8),ARCLINE  SKIP ROUND <ARC=CARD>                        
         BNE   NLIN2A                                                           
         LA    RE,ARCLINEL(RE)                                                  
NLIN2A   STH   RE,PRDISP                                                        
         LA    R4,0(R5,RE)         R4=A(FIRST LINE IN NEW CI)                   
*                                                                               
NLIN3    ST    R4,ALINE            SAVE ADR AND LEN OF LINE REC                 
         SR    RF,RF                                                            
         ICM   RF,3,0(R4)          RF=REC LEN (FIRST TWO BYTES)                 
         STH   RF,LLINE                                                         
         LA    RE,0(R4,RF)         CHECK LINE VALID FOR BUFFER                  
         C     RE,ACIRECX                                                       
         BH    NLINERFF                                                         
         MVI   PCC,X'09'           SET DEFAULT CC CHR                           
         TM    PQLINET,PQLTCC                                                   
         BZ    *+10                                                             
         MVC   PCC,2(R4)           CC CHR FIRST DATA BYTE                       
         CHI   RF,1                TEST REC LEN                                 
         BL    NLINB               0000 END OF BLOCK                            
         BE    NLINR               0001 END OF REPORT                           
*                                                                               
NLIN4    LA    RE,CCTAB            NORMAL PRINT LINE                            
         LA    R0,L'CCTAB                                                       
NLIN4A   CLC   0(1,RE),PCC         SEARCH CONTROL CHR TABLE                     
         BE    NLIN4B                                                           
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   NLIN4A                                                           
         LA    RE,CCTS2            DEFAULT IS WRITE/SPACE2                      
         CHI   RF,3                                                             
         BH    *+6                                                              
         AR    RE,R0                                                            
NLIN4B   MVC   LCC,0(RE)           SAVE CC TAB ENTRY                            
         IC    R0,LCC+2            SET NUM OF NL'S REQUIRED                     
         STH   R0,NUMNL                                                         
         CLI   LCC+2,X'FF'         SKIP TO TOP OF PAGE                          
         BNE   NLIN6               NO                                           
*                                                                               
NLIN5    OC    PRLNCTR,PRLNCTR     TEST IF ALREADY AT TOP OF PAGE               
         BNZ   NLIN5B                                                           
         CHI   RF,3                TEST BLANKS AND/OR SKIP CHAN 1               
         BNE   NLIN5A              NO                                           
         BAS   RE,BMPLAP           YES BUMP COUNTERS AND IGNORE                 
         AR    R4,RF                                                            
         AH    RF,PRDISP                                                        
         STH   RF,PRDISP                                                        
         OC    PRHDR1P(4),PRHDR1P  SAVE HDR1 IF FIRST NEW PAGE                  
         BNZ   *+10                                                             
         MVC   PRHDR1P,PRHDR1                                                   
         B     NLIN3                                                            
NLIN5A   XC    NUMNL,NUMNL         SET TO ZERO IF AT TOP OF PAGE                
         B     NLIN6                                                            
NLIN5B   TM    PATTR,ATTRNFF       TEST IF NO FF SUPPORT FOR DEVICE             
         BZ    NLIN6                                                            
         LLC   R1,PRLPP                                                         
         SH    R1,PRLNCTR                                                       
         BNM   *+8                                                              
         LA    R1,1                                                             
         STH   R1,NUMNL            SET TO NUM OF NL'S TO END PAGE               
*                                                                               
NLIN6    SR    R1,R1               CALCULATE NUM OF CHRS OF DATA                
         TM    LCC+1,X'01'                                                      
         BZ    NLIN6A              NO CHRS IF IMMEADIATE CTL CHR                
         LH    R1,LLINE                                                         
         LA    R0,2                DATA STARTS AFTER CC CHR                     
         TM    PQLINET,PQLTCC                                                   
         BZ    *+8                                                              
         LA    R0,3                                                             
         SR    R1,R0               SET DATA LENGTH                              
         BNM   *+8                                                              
         LA    R1,0                                                             
NLIN6A   STH   R1,NUMCHR           SET DATA LEN AT NUMCHR                       
*                                                                               
NLIN7    LTR   R1,R1               MOVE CHRS TO P                               
         BNP   NLIN7X                                                           
         LA    RE,P                RE=A(NEXT CHR IN P)                          
         LA    R4,2(R4)            R4=A(NEXT CHR IN BUFFER)                     
         TM    PQLINET,PQLTCC                                                   
         BZ    *+8                                                              
         LA    R4,1(R4)            BUMP PAST CC CHR IF PRESENT                  
*                                                                               
NLIN7A   CLI   0(R4),C'<'          TEST IF LINE MIGHT BE XML COMMAND            
         BNE   NLIN7B                                                           
         LA    RF,0(R4,R1)                                                      
         BCTR  RF,0                POINT TO LAST CHR                            
         CLI   0(RF),C'>'                                                       
         BNE   NLIN7B                                                           
         LH    RE,PRDISP           BUMP PAST THIS PRINT LINE                    
         AH    RE,LLINE                                                         
         STH   RE,PRDISP                                                        
         B     NLIN0                                                            
*                                                                               
NLIN7B   LR    R0,R1               R0 IS RESIDUAL CHR COUNT                     
         CHI   R1,256                                                           
         BNH   *+8                                                              
         LHI   R1,256                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)       MOVE DATA CHRS FROM BUFFER                   
*                                                                               
NLIN7C   TM    XLAT,X'01'          TEST IF USING ALL CHRS VALID TABLE           
         BZ    NLIN7D                                                           
         CLI   PCTRY,2             ONLY FOR UK/US                               
         BH    NLIN7D                                                           
         L     RF,=A(OUTALLC)                                                   
         TM    PATTR,ATTRLAS       TEST IF LASER CHRS SUPPORTED                 
         BZ    *+8                                                              
         L     RF,=A(OUTALLCL)                                                  
         A     RF,RELO                                                          
         B     NLIN7E                                                           
*                                                                               
NLIN7D   L     RF,=A(VALOCHRS)                                                  
         TM    PATTR,ATTRLAS       TEST IF LASER CHRS SUPPORTED                 
         BZ    *+8                                                              
         L     RF,=A(VALLCHRS)                                                  
         A     RF,RELO                                                          
*                                                                               
NLIN7E   EX    R1,*+8                                                           
         B     *+10                                                             
         TR    0(0,RE),0(RF)       REMOVE INVALID DATA CHRS                     
         CHI   R0,256                                                           
         BNH   NLIN7X                                                           
         AHI   R0,-256                                                          
         LR    R1,R0                                                            
         LA    RE,256(RE)                                                       
         LA    R4,256(R4)                                                       
         B     NLIN7B                                                           
NLIN7X   B     NLINX                                                            
*                                                                               
NLINB    LA    RF,PQDATA-PQINDEX   END OF BLOCK - SET FOR NEXT BLOCK            
         STH   RF,PRDISP                                                        
         IC    RF,PRADDR+2                                                      
         LA    RF,1(RF)                                                         
         STC   RF,PRADDR+2                                                      
         CLC   PRADDR(2),CJSTTRK                                                
         JL    *+12                                                             
         CH    RF,CJHIREC          TEST END OF TRACK PART 2                     
         BNH   NLIN1                                                            
         CH    RF,CIHIREC          TEST END OF TRACK PART 1                     
         BNH   NLIN1                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PRADDR                                                      
         AHI   RF,1                                                             
         STH   RF,PRADDR                                                        
         MVI   PRADDR+2,1                                                       
         CLM   RF,3,PRCIHIGH       TEST END OF CI                               
         BNH   NLIN1                                                            
*                                                                               
NLINC    SR    RF,RF               END OF CI - SET FOR NEXT CI                  
         ICM   RF,3,PRCINEXT                                                    
         LTR   RF,RF                                                            
         BZ    NLINERFF            ERROR NO LINK FORWARD                        
         STH   RF,PRADDR                                                        
         MVI   PRADDR+2,1          SET DISK ADDR OF FIRST RECORD                
         MVI   PRLPP,0             SET FIRST RECORD IN CI FLAG                  
         B     NLIN1                                                            
*                                                                               
NLINR    OI    FLINE,X'80'         RETURN END OF REPORT                         
         B     NLINX                                                            
*                                                                               
NLINERFR OI    RESULT,X'01'        FILE CI READ ERROR                           
         B     NLINX                                                            
NLINERFF OI    RESULT,X'04'        FILE CI FORMAT ERROR                         
         B     NLINX                                                            
*                                                                               
NLINX    CLI   RESULT,0            EXIT WITH CC=EQL IF OK                       
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* TEST IF DATA OF LENGTH R1 WILL FIT IN BUFFER                        *         
***********************************************************************         
TSTFIT   L     R0,PBUFF            POINT TO END OF BUFFER                       
         AH    R0,PBUFFLEN                                                      
         AHI   R0,-3               ADJUST FOR END OF BUFFER CTL CHRS            
         SR    R0,R3                                                            
         CR    R0,R1                                                            
         BR    RE                  EXIT WITH CC=LOW IF WONT FIT                 
         EJECT                                                                  
***********************************************************************         
* SET PAGE NUMBER IN R1 MOVE TO PAGN AND TO BUFFER                    *         
***********************************************************************         
PAGNUMNO MVI   PAGN,C' '           SET NO PAGE NUMBER                           
         MVI   PAGN+1,NL                                                        
         LA    R1,2                                                             
         B     PAGNUM3                                                          
*                                                                               
PAGNUM   SR    R1,R1               GET CURRENT PAGE NUMBER                      
         ICM   R1,3,PRPAGES                                                     
         LA    R1,1(R1)            CURRENT+1 IS REQUIRED FOR BUFFER             
         B     PAGNUM1                                                          
*                                                                               
PAGNUM0  SR    R1,R1               GET CURRENT (RESTART) PAGE NUMBER            
         ICM   R1,3,PRPAGES                                                     
         B     PAGNUM1                                                          
*                                                                               
PAGNUM1  CVD   R1,DUB              R1=PAGE NUM TO BE MOVED TO BUFFER            
         UNPK  PAGN(5),DUB         MAX PAGE NUM IS 99999                        
         OI    PAGN+4,C'0'                                                      
         MVI   PAGN+5,NL           TERMINATE WITH NL CHR                        
         LA    R1,6                                                             
         TM    PTYPE,TTYPERMC                                                   
         BZ    PAGNUM2                                                          
         TM    PATT2,ATTRAFP                                                    
         BO    PAGNUM4                                                          
PAGNUM2  CLI   PAGN,C'0'           REMOVE LEADING ZEROS                         
         BNE   PAGNUM3                                                          
         MVC   PAGN(5),PAGN+1                                                   
         BCT   R1,PAGNUM2                                                       
PAGNUM3  TM    PATT2,ATTRAFP       TEST IF PRINTER SUPPORTS AFP FORMS           
         BZ    PAGNUM5                                                          
         MVI   0(R3),C'<'          SET FORMS CODE IN <.....>                    
         MVC   1(5,R3),PNFAFP                                                   
         MVI   6(R3),C'>'                                                       
         AHI   R1,7                ADJUST LEN                                   
         STH   R1,PAGNLEN                                                       
         AHI   R1,-7                                                            
         ST    R3,PAGNADR                                                       
         AHI   R3,7                                                             
         B     PAGNUM6                                                          
PAGNUM4  MVI   PAGN+5,C'<'         NNNNN<SPPXY> FOR SHUTTLES                    
         MVC   PAGN+6(5),PNFAFP                                                 
         MVI   PAGN+11,C'>'                                                     
         MVI   PAGN+12,NL                                                       
         LA    R1,13                                                            
PAGNUM5  ST    R1,PAGNLEN          SAVE LENGTH AND LOCATION IN BUFFER           
         ST    R3,PAGNADR                                                       
PAGNUM6  BCTR  R1,0                MOVE PAGE NUM TO BUFFER                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PAGN                                                     
         LA    R1,1(R1)            R1=LEN OF DATA MOVED TO BUFFER               
         AR    R3,R1                                                            
         BR    RE                  EXIT WITH UPDATED BUFFER POINTER             
         EJECT                                                                  
***********************************************************************         
* BUMP LINES AND PAGES PRINTED COUNTERS                               *         
* IF NEW PAGE EXIT WITH R1=PAGE NUMBER AND R0=END PAGE NUMBER         *         
***********************************************************************         
BMPLAP   OC    NUMCHR,NUMCHR       TEST IF EMPTY PRINT LINE                     
         BZ    BLAP1                                                            
         L     R1,PRLINES          BUMP NON BLANK LINES PRINTED COUNT           
         LA    R1,1(R1)                                                         
         ST    R1,PRLINES                                                       
*                                                                               
BLAP1    CLI   LCC+2,X'FF'         NEW PAGE                                     
         BNE   BLAP2                                                            
         XC    PRLNCTR,PRLNCTR     CLEAR LINE COUNTER                           
         SR    R0,R0               R0=REQUESTED END PAGE                        
         SR    R1,R1                                                            
         TM    PNCOPYS,PNCENDP     TEST IF END PAGE SPECIFIED                   
         BZ    BLAP1A              NO                                           
         ICM   R0,3,PNSUBID        YES GET START PAGE                           
         IC    R1,PNSUBID+2        GET NUMPAGES-1                               
         AR    R0,R1                                                            
BLAP1A   ICM   R1,3,PRPAGES        BUMP PAGES PRINTED COUNT                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,PRPAGES        R1=CURRENT NEW PAGE NUMBER                   
         B     BLAP3                                                            
*                                                                               
BLAP2    LH    R1,PRLNCTR          NEW LINE                                     
         CLI   LCC,X'01'                                                        
         BNE   *+10                                                             
         XC    NUMNL,NUMNL         ADJUST FOR WRITE AND SPACE ZERO              
         AH    R1,NUMNL                                                         
         STH   R1,PRLNCTR                                                       
*                                                                               
BLAP3    CLI   LCC+2,X'FF'         EXIT WITH CC=EQL IF NEW PAGE                 
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET ERROR MESSAGE IN BUFFER                                         *         
***********************************************************************         
SETERR   NTR1                                                                   
         LA    RF,20               SET MAXIMUM REPEAT COUNT                     
SETE1    LA    R1,L'ERRMSG                                                      
         BAS   RE,TSTFIT                                                        
         BL    SETE2               EXIT IF THIS MESSAGE WONT FIT                
         MVC   0(L'ERRMSG,R3),ERRMSG                                            
         LA    R3,L'ERRMSG(R3)                                                  
         LH    R1,PRLNCTR                                                       
         LA    R1,2(R1)            ERROR MESSAGE CONTAINS 2 NL'S                
         STH   R1,PRLNCTR                                                       
         BCT   RF,SETE1                                                         
SETE2    OI    MODE+2,MODEDIB                                                   
SETEX    XIT1  REGS=(R3)           EXIT WITH R3 AT END OF ERROR MSG             
         EJECT                                                                  
***********************************************************************         
* LET LINE CONTROL KNOW THAT WE HAVE STOPPED THE PRINTER              *         
***********************************************************************         
STOP     NTR1                                                                   
         L     RE,APRNUTL          SET STATUS TO CLOSE DEST PENDING             
         OI    TSTATU-UTLD(RE),TSTATDNE                                         
*NOP*    LA    RF,VTPRSTOP         CALL LCM TO DO CLOSE DEST                    
*NOP*    ST    RF,DMCB                                                          
*NOP*    MVC   DMCB+4(4),APRNUTL                                                
*NOP*    GOTO1 VLCM,DMCB                                                        
STOPX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* FIND NEXT REPORT IN CURRENT QUEUE ENTRY FOR PRINTING                *         
* MODE CONTAINS REPORT SEQUENCE NUMBER OR LOW TIME VALUE              *         
*                                                                     *         
* RESULT=X'00' SET CIADDR TO ADDR OF NEXT REPORT IN CURRENT QUEUE     *         
*              SET RESULT+1=X'80' IF REPORT IS LINEUP                 *         
* RESULT=X'01' DISK ERROR FILE READ                                   *         
* RESULT=X'10' DISK ERROR INDEX READ                                  *         
***********************************************************************         
NXTREP   NTR1                                                                   
         BAS   RE,GETDATE          GET DATE AND TIME                            
         BAS   RE,GETTIME                                                       
         XC    RESULT(2),RESULT    CLEAR RETURN VALUES                          
         XC    CIADDR,CIADDR                                                    
         XC    CIRID,CIRID                                                      
         XC    SVCIRID,SVCIRID                                                  
         XC    SVCILK1,SVCILK1                                                  
         XC    SVCILK2,SVCILK2                                                  
         XC    SVCILK3,SVCILK3                                                  
         NI    PRSTAT,255-PRSFR    TURN OFF FLUSH REPORT                        
         TM    PRSTAT,PRSFQ+PRSFP                                               
         BNZ   NREPX               FLUSH QUEUE/ALL REQUESTED                    
         TM    PNEX,PREXACTV                                                    
         BZ    NREPX               QUEUE ENTRY IS INACTIVE                      
*                                                                               
NREP1    MVC   CIID,PNTRY          SAVE PRINTER QUEUE REPORT ID                 
         TM    PNCOPYS,PNCENDP                                                  
         BZ    *+10                                                             
         MVC   CISUBID,PR1KEY+2                                                 
         XC    CIRLD(4),CIRLD      SET LOW DATE/TIME                            
         MVC   CIRHD(4),=4X'FF'    SET HIGH DATE/TIME                           
         OC    MODE(2),MODE        TEST IF LAST REPORT PRINTED SET              
         BZ    NREP1A              NO                                           
         MVC   CIRNO,MODE          SET REPORT NUMBER                            
         MVC   CIRLD(4),MODE+4     SET LOW DATE/TIME                            
NREP1A   TM    CICOPYS,PNCTIME     TEST IF QUEUE ENTRY HAS END TIME             
         BZ    NREP1B              NO                                           
         MVC   CIRHD,DATEC         SET HIGH DATE TO TODAY                       
         MVC   CIRHT,CISEQN        SET HIGH TIME FROM QUEUE ENTRY               
         B     NREP1C                                                           
NREP1B   OC    CISEQN,CISEQN       TEST IF SINGLE REPORT ENTRY                  
         BNZ   NREP2                                                            
NREP1C   MVC   SVCIRID,CIRID       SAVE REPORT ID DATA                          
         B     NREP4                                                            
*                                                                               
NREP2    MVC   SVCIRID,CIRID       QUEUE ENTRY CONTAINS SINGLE REPORT           
         TM    MODE+2,MODERPP      IGNORE PARTIAL PRINTED REPORTS               
         BO    NREPX                                                            
         CLC   CISRCID(7),PR1KEY   TEST IF JUST PRINTED THIS SUCKER             
         BE    NREPX                                                            
         MVC   NDX(2),CISRCID      GET PRTQ ID FOR SINGLE REPORT                
         GOTO1 VDATAMGR,DMCB,(0,GFILE),PRTQUE,NDX,,CXREC                        
         MVC   PRTQID,NDX+32                                                    
         GOTO1 VDATAMGR,DMCB,(0,BUFFER),PRTQID,,,CXREC                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   PRTQADTF,CXREC+4    FILE EXT NUM AND A(DTF)                      
         MVC   CIDATA,CXREC+12     SET CI DATA FOR THIS PRTQ FILE               
*                                                                               
         SAM31                                                                  
         MVC   FIWRES,SPACES       SET RESOURCE                                 
         MVC   FIWRES(L'PRTQID),PRTQID                                          
         BRAS  RE,FIRSET           SET ADDRESSES FOR THIS RESOURCE              
         JNE   *+2                 INVALID PRTQ                                 
         BRAS  RE,FIRSLOCK         PUT A SHARED LOCK ON THE PRINT QUEUE         
*                                                                               
         XC    FIWREF,FIWREF                                                    
         MVC   FIWREF+2(2),CISEQN                                               
         BRAS  RE,FIRRC                                                         
         BRAS  RE,FIRCN                                                         
         BRAS  RE,FIRC1                                                         
         BNE   NREPERXR                                                         
*                                                                               
         L     R1,FIWNDA                                                        
         MVC   FIWNDX,SI1NDX-SI1PAR(R1)                                         
         LA    R5,FIWNDX                                                        
         SAM24                                                                  
*                                                                               
         MVC   SVKEY,CISRCID       SAVE REPORT KEY                              
         TM    CICOPYS,PNCENDP     TEST IF SUBID CONTAINS PAGE NUMS             
         BZ    *+10                                                             
         MVC   SVKEY+2(3),PQSUBID                                               
         CLC   SVKEY(7),PQKEY      TEST IF STILL THERE                          
         BNE   NREP2C                                                           
         TM    PQSTAT,PQSTIN       CANT PRINT INVISIBLE REPORTS                 
         BO    NREP2C                                                           
         TM    PQSTAT,PQSTAC       MUST BE ACTIVE                               
         BZ    NREP2B1                                                          
         TM    PQSTAT,PQSTPG       AND NOT PRINTING                             
         BO    NREP2C                                                           
         B     NREP3                                                            
NREP2B1  TM    PATT2,ATTRASS       TEST IS ALLOW SPECIAL STATUS REPORTS         
         BZ    NREP2C                                                           
         TM    PQSTAT,PQSTHO       MUST BE HOLD                                 
         BZ    NREP2C                                                           
         TM    PQSTAT,PQSTPG       AND NOT PRINTING                             
         BO    NREP2C                                                           
         B     NREP3                                                            
*                                                                               
NREP2C   B     NREPX               REPORT NO LONGER VALID FOR ENTRY             
*                                                                               
NREP3    MVI   CIRFLAG,1           SET FLAG TO SHOW SINGLE REPORT               
         MVC   CIRNO,CISEQN        SAVE REPORT SEQNUM                           
         MVC   CIRXPE,FIWCIA       SAVE INDEX PAGE/ENTRY                        
         MVC   CIADDR,CIRXPE       SAVE REPORT CIADDR                           
         B     NREPD                                                            
                                                                                
***********************************************************************         
* READ ALL PRTQS                                                      *         
***********************************************************************         
NREP4    XC    NDX,NDX             SET TO READ ALL PRTQS IF GENERIC             
         XC    APRTQLST,APRTQLST                                                
         TM    PNSRCID,X'80'       TEST IF GENERIC USER ID                      
         BO    NREP4A              YES                                          
         MVC   NDX(2),PNSRCID      GET PRTQ ID FOR SPECIFIC USER ID             
         GOTO1 VDATAMGR,DMCB,(0,GFILE),PRTQUE,NDX,,CXREC                        
         MVC   PRTQID,NDX+32                                                    
         B     NREP5                                                            
NREP4A   GOTO1 VDATAMGR,DMCB,(0,GLIST),PRTQUE,NDX,,CXREC                        
         L     RE,NDX+32                                                        
         LA    RE,8(RE)                                                         
         ST    RE,APRTQLST         SAVE ADR OF FIRST PRTQ FILE ENTRY            
         MVC   PRTQID+4(1),1(RE)   SET PRTQ ID FROM LIST ENTRY                  
*                                                                               
NREP5    GOTO1 VDATAMGR,DMCB,(0,BUFFER),PRTQID,,,CXREC                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   PRTQADTF,CXREC+4    FILE EXT NUM AND A(DTF)                      
         MVC   CIDATA,CXREC+12     SET CI DATA FOR THIS PRTQ FILE               
         XC    CIRXPE,CIRXPE       SET NO REPORT FOUND                          
         XC    CIRNO,CIRNO                                                      
         XC    SVKEY,SVKEY                                                      
         XC    DUB,DUB                                                          
         LA    R5,FIWNDX                                                        
*                                                                               
         SAM31                                                                  
         MVC   FIWRES,PRTQID       SET RESOURCE                                 
         BRAS  RE,FIRSET           SET ADDRESSES FOR THIS RESOURCE              
         JNE   *+2                 INVALID PRTQ                                 
         BRAS  RE,FIRSLOCK         PUT A SHARED LOCK ON THE PRINT QUEUE         
         MVC   FIWNDA,FIWP1A       A(START OF PART1 INDEXES)                    
         B     NREP6A                                                           
NREP6    SAM31                                                                  
         BRAS  RE,FIRNSN                                                        
         BNE   NREPC                                                            
NREP6A   L     R1,FIWNDA                                                        
         MVC   FIWNDX(L'PQINDEX),SI1NDX-SI1PARD(R1)                             
         BRAS  RE,FIRNC            A(NODE) TO A(CI)                             
         SAM24                                                                  
*                                                                               
NREP7    TM    CISRCID,X'80'       TEST GENERIC ID                              
         BZ    NREP7C                                                           
         OC    VGENIDS,VGENIDS     TEST A(GENIDS) SET                           
         BNZ   NREP7B              YES                                          
         GOTO1 VCALLOV,DMCB,0,X'D9000AFC',0                                     
         MVC   VGENIDS+0(1),4(R1)                                               
         MVC   VGENIDS+1(3),1(R1)                                               
*                                                                               
NREP7B   CLI   VGENIDS,X'FF'       TEST VGENIDS FOUND                           
         BE    NREPB               NO                                           
         GOTO1 VGENIDS,DMCB,CISRCID,VDATAMGR                                    
         BNE   NREPB                                                            
         LM    RE,RF,0(R1)         RE=N'ENTRIES, RF=A(ENTRIES)                  
         CLC   PQSRCID,0(RF)       MATCH SOURCE ID                              
         BE    NREP7D                                                           
         LA    RF,2(RF)            BUMP TO NEXT                                 
         BCT   RE,*-14                                                          
         B     NREPB                                                            
*                                                                               
NREP7C   CLC   PQSRCID,CISRCID     SAME SOURCE ID                               
         BNE   NREPB                                                            
*&&US                                                                           
         CLC   CISRCID,=X'0406'    CLASS G REPORTS ARE FOR GRAFNET              
         BNE   *+16                                                             
         CLI   PQCLASS,C'G'                                                     
         BNE   NREPB                                                            
         B     NREP8                                                            
         CLI   PQCLASS,C'G'                                                     
         BE    NREPB                                                            
         CLI   PQCLASS,C'N'        NEVER PRINT CLASS N                          
         BE    NREPB                                                            
*&&                                                                             
NREP7D   MVC   FLAG1,CICLASS       SAME CLASS (POSITIVE OR NEGATIVE)            
         CLI   FLAG1,0                                                          
         BE    NREP8                                                            
         TM    FLAG1,X'40'                                                      
         BZ    NREP7E                                                           
         CLC   PQCLASS,FLAG1                                                    
         BE    NREP8                                                            
         B     NREPB                                                            
NREP7E   OI    FLAG1,X'40'                                                      
         CLC   PQCLASS,FLAG1                                                    
         BE    NREPB                                                            
*                                                                               
NREP8    TM    CICOPYS,PNCENDP     TEST IF SUBID CONTAINS SUBID                 
         BO    NREP9                                                            
         CLC   CISUBID,=C'ALL'     SAME SUB ID (OR ALL)                         
         BNE   NREP8A                                                           
         TM    PQSTAT,PQSTKE       IGNORE LINEUP REPORTS                        
         BZ    *+14                                                             
         CLC   PQSUBID,=C'LU1'                                                  
         BE    NREPB                                                            
         B     NREP9                                                            
NREP8A   CLI   CISUBID+1,C'*'      TEST GENERIC SUBID X*                        
         BNE   NREP8B                                                           
         CLC   CISUBID(1),PQSUBID                                               
         BNE   NREPB                                                            
         TM    PQSTAT,PQSTKE       IGNORE LINEUP REPORTS                        
         BZ    *+14                                                             
         CLC   PQSUBID,=C'LU1'                                                  
         BE    NREPB                                                            
         B     NREP9                                                            
NREP8B   CLI   CISUBID+2,C'*'      TEST GENERIC SUBID XX*                       
         BNE   NREP8C                                                           
         CLC   CISUBID(2),PQSUBID                                               
         BNE   NREPB                                                            
         TM    PQSTAT,PQSTKE       IGNORE LINEUP REPORTS                        
         BZ    *+14                                                             
         CLC   PQSUBID,=C'LU1'                                                  
         BE    NREPB                                                            
         B     NREP9                                                            
NREP8C   CLC   CISUBID,PQSUBID                                                  
         BNE   NREPB                                                            
*                                                                               
NREP9    MVC   HALF2,PQAGELD                                                    
         TM    PQTYP1,PQTYNCD      SET HALF2 TO NEW CMPRSD DATE                 
         BO    NREP9A                                                           
         GOTO1 ADATCON,DMCB,(2,HALF2),(30,HALF2)                                
NREP9A   TM    CICOPYS,PNCTIME     TEST IF SEQNUM CONTAINS SEQ NUM              
         BZ    NREP9B                                                           
NREP9AA  CLC   HALF2,CIRLD         TEST LOW DATE                                
         BL    NREPB                                                            
         BH    *+14                                                             
         CLC   PQAGELT,CIRLT       TEST LOW TIME IF LOW DATES MATCH             
         BL    NREPB                                                            
         CLC   HALF2,CIRHD         TEST HIGH DATE                               
         BH    NREPB                                                            
         BL    *+14                                                             
         CLC   PQAGELT,CIRHT       TEST HIGH TIME IF HIGH DATES MATCH           
         BH    NREPB                                                            
         B     NREPA                                                            
NREP9B   OC    CISEQN,CISEQN       TEST ALL SEQUENCE NUMBERS                    
         BZ    NREP9AA                                                          
         CLC   PQREPNO,CISEQN      MATCH REPORT SEQUENCE NUMBER                 
         BNE   NREPB                                                            
         MVI   CIRFLAG,2           SET MATCHED ON SINGLE REPORT                 
*                                                                               
NREPA    CLI   PQSEQ,1             IGNORE HIGH ORDER CI'S                       
         BH    NREPB                                                            
         TM    PQSTAT,PQSTIN       CANT PRINT INVISIBLE REPORTS                 
         BO    NREPB                                                            
         TM    PQSTAT,PQSTAC       MUST BE ACTIVE                               
         BZ    NREPA0                                                           
         TM    PQSTAT,PQSTPG       AND NOT PRINTING                             
         BO    NREPB                                                            
         B     NREPA1                                                           
NREPA0   TM    PATT2,ATTRASS       TEST IF ALLOW SPECIAL STATUS                 
         BZ    NREPB                                                            
         TM    PQSTAT,PQSTHO       MUST BE HOLD                                 
         BZ    NREPB                                                            
         TM    PQSTAT,PQSTPG       AND NOT PRINTING                             
         BO    NREPB                                                            
NREPA1   TM    MODE+2,MODERPP      IGNORE PARTIAL PRINTED REPORTS               
         BO    NREPB                                                            
         CLC   PQKEY,PR1KEY        IGNORE ACTIVE REPORT JUST PRINTED            
         BE    NREPB                                                            
         CLC   PQKEY,SVCILK1       IGNORE IF SAME AS A PREV PASS                
         BE    NREPB                                                            
         CLC   PQKEY,SVCILK2       IGNORE IF SAME AS A PREV PASS                
         BE    NREPB                                                            
         CLC   PQKEY,SVCILK3       IGNORE IF SAME AS A PREV PASS                
         BE    NREPB                                                            
*                                                                               
NREPA2   TM    PQATTB,PQATJOBI     IGNORE REPORTS CONTAINING JCL                
         BO    NREPB                                                            
         TM    PQATTB,PQATNP       TEST NON PRINTABLE REPORT                    
         BZ    NREPA3              NO                                           
         TM    PTYPE,TTYPERMC      YES SHUTTLE CAN COPE                         
         BO    NREPA3                                                           
         B     NREPB                                                            
*                                                                               
NREPA3   TM    PQATTB,PQATPW       TEST SECURE REPORT                           
         B     NREPA4              *NOP* BZ - IGNORE SECURE FLAG                
         TM    PTYPE,TTYPERMC      YES DO NOT SEND TO SHUTTLE                   
         BO    NREPB                                                            
         TM    CICOPYS,PNCTIME     TEST IF QUEUE ENTRY HAS END TIME             
         BO    NREPB                                                            
         OC    CISEQN,CISEQN       TEST IF SINGLE REPORT ENTRY                  
         BZ    NREPB                                                            
         CLC   PQREPNO,CISEQN      MUST BE EXACT MATCH FOR SECURITY             
         BNE   NREPB                                                            
*                                                                               
NREPA4   EQU   *                   REPORT IS A CANDIDATE                        
*                                                                               
NREPA5   TM    CISRCID,X'80'       TEST IF MATCHED ON GENERIC                   
         BZ    NREPA6              NO                                           
         OC    DUB(2),DUB          TEST IF FIRST MATCH                          
         BNZ   *+14                NO                                           
         MVC   DUB(2),PQSRCID      YES SAVE SPECIFIC USER ID                    
         B     NREPA6                                                           
         CLC   DUB(2),PQSRCID      TEST IF SAME USER ID AS PREV                 
         BNE   NREPB                                                            
*                                                                               
NREPA6   MVC   CIRNO,PQREPNO       SAVE REPORT NUM                              
         MVC   CIRXPE,FIWCIA       SAVE REPORT CI ADDRESS                       
         MVC   SVKEY,PQKEY         SAVE REPORT KEY                              
         MVC   CIRHD,HALF2         SAVE HIGH DATE/TIME                          
         MVC   CIRHT,PQAGELT                                                    
*                                                                               
NREPB    B     NREP6               BUMP TO NEXT INDEX ENTRY                     
*                                                                               
NREPC    MVC   CIADDR,CIRXPE       END OF PTRQ INDEX                            
         OC    CIADDR,CIADDR       WAS A REPORT FOUND                           
         BNZ   NREPC1              YES                                          
         ICM   RE,15,APRTQLST                                                   
         BZ    NREPX               EXIT WITH ZERO CIADDR IF NO REPORT           
         LA    RE,8(RE)                                                         
         ST    RE,APRTQLST         BUMP TO NEXT PRTQ IF GENERIC ID              
         CLI   0(RE),0                                                          
         BE    NREPX               EXIT IF END OF PRTQ FILE LIST                
         MVC   PRTQID+4(1),1(RE)   SET NEXT PRTQ FILE ID                        
         XC    RESULT,RESULT                                                    
         B     NREP5               BACK TO PROCESS NEXT PRTQ FILE               
*                                                                               
NREPC1   MVC   FIWCIA,CIADDR       SAVE CIADDR                                  
         BRAS  RE,FIRCN                                                         
         L     R1,FIWNDA                                                        
         MVC   FIWNDX,SI1NDX-SI1PAR(R1)                                         
         SAM24                                                                  
*                                                                               
         CLC   PQKEY,SVKEY         MUST BE STILL THE SAME REPORT                
         BNE   NREPE                                                            
         TM    PQSTAT,PQSTPG       MUST NOT BE ALREADY PRINTING                 
         BO    NREPE                                                            
         TM    PQSTAT,PQSTAC       MUST BE ACTIVE                               
         BO    NREPD                                                            
         TM    PATT2,ATTRASS       TEST IF ALLOW SPECIAL STATUS                 
         BZ    NREPE                                                            
         TM    PQSTAT,PQSTHO       MUST BE HOLD                                 
         BZ    NREPE                                                            
*                                                                               
NREPD    MVC   CIRXPE,CIADDR       SAVE CIADDR OF REPORT                        
         L     R5,ACIREC                                                        
         BAS   RE,READCI           READ FIRST CI REC OF REPORT                  
         BNE   NREPE                                                            
         CLC   PQKEY,SVKEY         CHECK INDEX SAME AS FILE                     
         BNE   NREPE                                                            
*&&UK*&& CLI   PQCLASS,C'0'        TEST IF APS FORM/TRIGGER                     
*&&UK*&& BE    NREPD1                                                           
         TM    PQSTAT,PQSTKE       TEST IF LINEUP REPORT                        
         BZ    NREPD2                                                           
         CLC   PQDESC(7),=C'*LINEUP='                                           
         BNE   NREPD2                                                           
NREPD1   OI    RESULT+1,X'80'                                                   
         TM    PRQMODE,X'80'       IGNORE LINE UPS FOR AUTO MODE                
         BO    NREPE                                                            
NREPD2   B     NREPX                                                            
*                                                                               
NREPE    EQU   *                   BYPASS INVALID/UNWANTED REPORTS              
*                                                                               
NREPF    XC    RESULT,RESULT       SET UP EXTRA PASS                            
         XC    CIADDR,CIADDR                                                    
         CLI   CIRFLAG,0           EXIT IF SINGLE REPORT ENTRY                  
         BNE   NREPX                                                            
         OC    SVCILK1,SVCILK1     TEST PASS 1                                  
         BNZ   *+14                                                             
         MVC   SVCILK1,SVKEY       SAVE KEY OF UNWANTED REPORT 1                
         B     NREPF1                                                           
         OC    SVCILK2,SVCILK2     TEST PASS 2                                  
         BNZ   *+14                                                             
         MVC   SVCILK2,SVKEY       SAVE KEY OF UNWANTED REPORT 2                
         B     NREPF1                                                           
         OC    SVCILK3,SVCILK3     TEST PASS 3                                  
         BNZ   *+14                                                             
         MVC   SVCILK3,SVKEY       SAVE KEY OF UNWANTED REPORT 3                
         B     NREPF1                                                           
         B     NREPX                                                            
NREPF1   MVC   CIRID,SVCIRID       RESTORE REPORT DEFINITION                    
         B     NREP5                                                            
*                                                                               
NREPERFR MVI   RESULT,X'01'        DISK ERROR ON FILE READ                      
         B     *+8                                                              
NREPERXR MVI   RESULT,X'10'        INDEX ERROR                                  
         B     NREPX                                                            
*                                                                               
NREPX    CLI   RESULT,X'00'        EXIT WITH CC=EQL IF REPORT FOUND             
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SET QUEUE HEADER FOR NEW CURRENT REPORT WITH ADDR AT CIADDR         *         
* CIREC CONTAINS THE FIRST REPORT CI RECORD                           *         
***********************************************************************         
SETQUE   NTR1                                                                   
         MVC   PRCIADDR,CIADDR     SET NEW CURRENT REPORT                       
         OC    PRID,PRID                                                        
         BNZ   *+14                                                             
         MVC   PRID,SVKEY          SET PRINTER ID FROM FIRST REPORT ID          
         MVI   PRNUM,1                                                          
*                                                                               
         XC    NDX,NDX             GET PRTQ FILE ID FOR THIS REPORT             
         MVC   NDX(2),SVKEY                                                     
         LA    R5,CXREC                                                         
         GOTO1 VDATAMGR,DMCB,(X'00',GFILE),PRTQUE,NDX,P,(R5)                    
         MVC   PRTQINUM(2),NDX+24                                               
         MVC   PRTQID,NDX+32                                                    
         GOTO1 VDATAMGR,DMCB,(X'00',BUFFER),PRTQID                              
         MVC   PRTQADTF,4(R5)      FILE EXT NUM AND A(DTF)                      
         MVC   DISPBUSV,8(R5)                                                   
         MVC   CIDATA,12(R5)                                                    
*                                                                               
         SAM31                                                                  
         MVC   FIWRES,SPACES       SET RESOURCE                                 
         MVC   FIWRES(L'PRTQID),PRTQID                                          
         BRAS  RE,FIRSET           SET ADDRESSES FOR THIS RESOURCE              
         JNE   *+2                 INVALID PRTQ                                 
         BRAS  RE,FIRSLOCK         PUT A SHARED LOCK ON THE PRINT QUEUE         
         SAM24                                                                  
*                                                                               
         L     R5,ACIREC           R5=A(FIRST CI RECORD)                        
         XC    PRHDR1,PRHDR1       CLEAR COUNTERS ETC                           
         MVC   PRPRTQA,PRTQID+4    SET PRTQ FILE ID                             
         MVC   PR1CIFST,CIADDR     SET FIRST CI DISK ADDR                       
         MVC   PR1KEY,SVKEY        SET REPORT KEY                               
         MVC   PR1COPYS,PNCOPYS    SET NUMBER OF COPYS REQUESTED                
         NI    PR1COPYS,X'0F'                                                   
         CLI   PR1COPYS,0          TEST IF COPYS REQUESTED                      
         BNE   SETQ1               YES                                          
         MVC   PR1COPYS,PQCOPIES   NO SET COPYS DEFINED FOR REPORT              
         NI    PR1COPYS,X'0F'                                                   
         TM    PNCOPYS,PNCTIME     TEST IF SINGLE REPORT ENTRY                  
         BO    SETQ1               NO                                           
         OC    PNSEQN,PNSEQN                                                    
         BZ    SETQ1                                                            
         OC    PNCOPYS,PR1COPYS    SET COPIES SO IT WILL SHOW IN $PQ            
SETQ1    CLI   PR1COPYS,0                                                       
         BNE   *+8                                                              
         MVI   PR1COPYS,1                                                       
         MVC   PRADDR,CIADDR       SET ADDR/DISP OF FIRST PRINT LINE            
         LA    RE,PQDATA1-PQINDEX                                               
         STH   RE,PRDISP                                                        
         MVC   PNFAFP,PQMAKER      SET AFP FORM CODE                            
         OC    PNFAFP,SPACES                                                    
         MVC   PNCAFP,PQMAXCPL     SET AFP MAX CHRS PER LINE                    
*                                                                               
SETQ2    TM    PNCOPYS,PNCENDP     TEST IF START PAGE SPECIFIED                 
         BZ    SETQ5               NO                                           
         OI    PR1FLAG,PR1FPP      SET PARTIAL PRINT REPORT                     
         CLC   PNSUBID(2),=H'1'    SUBID HAS START PAGE                         
         BNH   SETQ4                                                            
         XC    PCC(12),PCC         FIND START PAGE GIVEN BY SPAGE               
         MVC   PCC+2(2),PNSUBID                                                 
         MVC   PCC+4(4),=C'PAGE'                                                
         L     RE,DISPBUSV         POINT TO BUFFER SAVE STORAGE                 
         AR    RE,R5                                                            
         USING SKBUFFD,RE                                                       
         MVC   SKINTNO,PRTQINUM    SET REQUIRED FIELDS FOR RANDOM               
         MVC   SKEXTNO,PRTQXNUM                                                 
         XC    SKFCTRL,SKFCTRL                                                  
         XC    SKXCTRL,SKXCTRL                                                  
         MVC   SKFSTCI,CIADDR                                                   
         GOTO1 VDATAMGR,DMCB,(X'00',RANDOM),PRTQID,NDX,PCC,(R5)                 
         CLI   8(R1),0                                                          
         BE    SETQ3                                                            
         XC    DCIREC,DCIREC       READ FIRST CI ON ERROR                       
         MVC   CIADDR,PRCIADDR                                                  
         BAS   RE,READCI                                                        
         MVC   PNSUBID(2),=H'1'    SET START PAGE TO FIRST                      
         MVI   PNSUBID+2,0         SET NUMPAGES-1 TO PRINT ONE PAGE             
         B     SETQ4                                                            
*                                                                               
SETQ3    L     RE,DISPBUSV         POINT TO BUFFER SAVE STORAGE                 
         AR    RE,R5                                                            
         MVC   PRLPP,PQLPP         RESET PRINTER QUEUE HEADER                   
         MVC   PRSEQ,PQSEQ                                                      
         MVC   PRCIHIGH,SKENDCI                                                 
         MVC   PRCINEXT,SKNXTCI                                                 
         MVC   PRDISP,SKDISP                                                    
         MVC   PRADDR,SKADDR                                                    
         XC    PRLNCTR,PRLNCTR                                                  
         MVC   PRLINES,SKLINES                                                  
         MVC   PRPAGES,SKPAGES+2                                                
         CLI   PRSEQ,0                                                          
         BNE   SETQ4                                                            
         OC    PRCINEXT,PRCINEXT                                                
         BNE   SETQ4                                                            
         MVI   PRSEQ,1                                                          
SETQ4    EQU   *                                                                
         DROP  RE                                                               
*                                                                               
SETQ5    MVC   PRHDR1F,PRHDR1      SET CHECKPOINT DATA FROM NEW HDR             
         MVC   PRHDR1P,PRHDR1                                                   
         MVC   PRHDR1S,PRHDR1                                                   
*                                                                               
         L     RF,=A(CHKWRT)       WRITE NEW QUEUE TO DISK                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
SQUEX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* TEST AND SET QUEUE HEADER FOR MULTIPLE COPIES OF REPORT             *         
***********************************************************************         
SETCPY   NTR1                                                                   
         LLC   R1,PR1COPYS         DECR NUMBER OF COPYS                         
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         SR    R1,R1                                                            
         STC   R1,PR1COPYS                                                      
         LTR   R1,R1                                                            
         BZ    SCPYX                                                            
*                                                                               
SCPY2    MVC   PRHDR1,PRHDR1F      SET CURRENT REPORT BACK TO FIRST             
         STC   R1,PR1COPYS                                                      
         MVC   PRHDR1P,PRHDR1                                                   
         MVC   PRHDR1S,PRHDR1                                                   
*                                                                               
SCPYX    LTR   R1,R1               EXIT WITH CC=EQL IF NO MORE COPYS            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CLEAR CURRENT QUEUE AND FIND NEXT QUEUE ENTRY                       *         
* RESULT=X'00' NEXT QUEUE ENTRY FOUND                                 *         
* RESULT=X'01' NO MORE ACTIVE QUEUE ENTRYS                            *         
***********************************************************************         
NXTQUE   NTR1                                                                   
         GOTO1 VTICTOC,DUB,C'SSET' DISABLE TIMER                                
         MVI   RESULT,X'01'        SET NO NEXT QUEUE ENTRY                      
*                                                                               
NQUE1    IC    RF,NSHIFTS          BUMP NUMBER OF TIMES HAVE DONE THIS          
         LA    RF,1(RF)                                                         
         STC   RF,NSHIFTS                                                       
         NI    PRSTAT,255-PRSFQ    RESET FLUSH QUEUE REQUESTED                  
         XC    PRCIADDR,PRCIADDR   CLEAR QUEUE HDR FIELDS                       
         XC    PRHDR1,PRHDR1                                                    
         XC    PRHDR1F,PRHDR1F                                                  
         XC    PRHDR1P,PRHDR1P                                                  
         XC    PRHDR1S,PRHDR1S                                                  
         NI    PNEX,255-PREXACTV   SET FIRST QUEUE ENTRY INACTIVE               
         MVC   CIID(L'PNTRY),PNTRY SAVE FIRST QUEUE ENTRY                       
         SR    R0,R0                                                            
         ICM   R0,1,PRQNE          R0=NUMBER OF QUEUE ENTRIES                   
         BNZ   NQUE3                                                            
*                                                                               
NQUE2    LA    R0,1                SET TO SINGLE ENTRY IF INVALID               
         STC   R0,PRQNE                                                         
         TM    PNEX,PREXPERM                                                    
         BO    *+8                                                              
         NI    PRQMODE,X'7F'                                                    
*                                                                               
NQUE3    CHI   R0,1                SINGLE QUEUE ENTRY                           
         BH    NQUE4                                                            
         XC    PNNEXT(2),PNNEXT                                                 
         TM    PNEX,PREXPERM       LEAVE SINGLE PERM ENTRY ALONE                
         BO    NQUE6                                                            
         XC    PNTRY,PNTRY         CLEAR SINGLE TEMP ENTRY                      
         SR    R0,R0                                                            
         MVI   PRQNE,0                                                          
         B     NQUE6                                                            
*                                                                               
NQUE4    SR    R4,R4               GET FIRST POOL ENTRY                         
         ICM   R4,3,PNNEXT                                                      
         BZ    NQUE2               ERROR IF NO FIRST                            
         OC    PNLAST,PNLAST                                                    
         BZ    NQUE2               ERROR IF NO LAST                             
         ST    R4,DUB+4            DUB+4(4)=FIRST POOL ENTRY NUMBER             
         BCTR  R4,0                                                             
         MH    R4,LPRQES                                                        
         A     R4,APRQES           R4=A(FIRST POOL ENTRY)                       
         MVC   PNTRY,0(R4)         MOVE FIRST POOL ENTRY TO PRQ                 
         TM    CIEX,PREXPERM                                                    
         BO    NQUE5               OLD FIRST ENTRY IS PERMANENT                 
         LA    RF,VTDELPRQ                                                      
         ST    RF,DUB                                                           
         GOTO1 VLCM,DUB            DELETE FIRST POOL ENTRY                      
         BCTR  R0,0                                                             
         STC   R0,PRQNE                                                         
         CHI   R0,1                TEST ONLY ONE ENTRY LEFT                     
         BH    *+10                NO                                           
         XC    PNLAST,PNLAST       YES CLEAR LAST POOL ENTRY                    
         B     NQUE6                                                            
*                                                                               
NQUE5    SR    RE,RE               GET LAST QUEUE ENTRY                         
         ICM   RE,3,PNLAST                                                      
         BZ    NQUE2                                                            
         ST    RE,DUB+0            DUB+0(4)=LAST POOL ENTRY NUMBER              
         BCTR  RE,0                                                             
         MH    RE,LPRQES                                                        
         A     RE,APRQES           RE=A(LAST POOL ENTRY)                        
         XC    CINEXT,CINEXT                                                    
         MVC   0(L'PNTRY,R4),CIID                                               
         CLC   DUB(4),DUB+4        TEST IF FIRST=LAST                           
         BNE   NQUE5A                                                           
         MVC   PNNEXT,DUB+6                                                     
         B     NQUE6                                                            
NQUE5A   MVC   PNNEXT-PNTRY(2,RE),DUB+6                                         
         MVC   PNLAST,DUB+6        NEW LAST ENTRY IS FIRST POOL ENTRY           
*                                                                               
NQUE6    CLI   PRQNE,0             TEST EMPTY QUEUE                             
         BE    NQUE7                                                            
         CLC   NSHIFTS,MSHIFTS     TEST CYCLED WHOLE QUEUE                      
         BL    NQUE8                                                            
         MVI   PRSTAT,0            SET PRINTER INACTIVE                         
         B     NQUEX                                                            
*                                                                               
NQUE7    XC    PRID,PRID           CLEAR QUEUE HDR FIELDS IF EMPTY              
         MVI   PRNUM,0                                                          
         MVI   PRSTAT,0                                                         
         MVI   PRQMODE,0                                                        
         B     NQUEX                                                            
*                                                                               
NQUE8    TM    PRSTAT,PRSFP        TEST IF FLUSH ALL REQUESTED                  
         BO    NQUE1                                                            
         XC    MODE(2),MODE                                                     
         XC    MODE+4(4),MODE+4                                                 
         MVI   MODE+2,MODEEOQ      SET END OF QUEUE MODE                        
*                                                                               
NQUE9    TM    PRSTAT,PRSSQ        TEST STOP AT END OF QUEUE REQUESTED          
         BZ    NQUE9A                                                           
         NI    PRSTAT,255-PRSSQ                                                 
         OI    PRSTAT,PRSSR        SET TO STOP AT END OF REPORT                 
NQUE9A   MVI   RESULT,X'00'                                                     
*                                                                               
NQUEX    GOTO1 VTICTOC,DUB,C'RSET' ENABLE TIMER                                 
         L     RF,=A(CHKWRT)       WRITE NEW QUEUE TO DISK                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         CLI   RESULT,0            EXIT WITH CC=EQL IF FOUND                    
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GET DATE AND TIME                                                   *         
***********************************************************************         
GETDATE  ST    RE,SAVRE                                                         
         DATE  DATEE               DATEE=C'YYMMDDCC'                            
         SR    RE,RE                                                            
         CLI   DATEE,C'9'                                                       
         BNH   GETDATE1                                                         
         IC    RE,DATEE                                                         
         AHI   RE,-10                                                           
         STC   RE,DATEE                                                         
         LA    RE,100              SET ADJUSTMENT FOR 2000+ DATES               
GETDATE1 PACK  DUB,DATEE+0(2)                                                   
         CVB   R0,DUB                                                           
         AR    R0,RE               ADJUST FOR 2000+ DATES                       
         STC   R0,DATEB+0                                                       
         PACK  DUB,DATEE+2(2)                                                   
         CVB   R0,DUB                                                           
         STC   R0,DATEB+1                                                       
         PACK  DUB,DATEE+4(2)                                                   
         CVB   R0,DUB                                                           
         STC   R0,DATEB+2          DATEB=X'YYMMDD'                              
*                                                                               
         GOTO1 ADATCON,DUB2,(0,DATEE),(2,DATEO)                                 
         GOTO1 ADATCON,DUB2,(0,DATEE),(30,DATEN)                                
*                                                                               
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
GETTIME  ST    RE,SAVRE                                                         
         TBIN  SECS                                                             
         SR    R0,R0                                                            
         D     R0,=F'60'           R1=BINARY MINUTES                            
         SR    RE,RE                                                            
         LR    RF,R1                                                            
         D     RE,=F'10'                                                        
         STC   RF,TIMEI            TIMEI=SINGLE BYTE 10MIN INCREMENT            
         SR    R0,R0                                                            
         D     R0,=F'60'           R0=MINS,R1=HOURS                             
         STC   R1,TIMEB                                                         
         STC   R0,TIMEB+1          TIMEB=B'HHHHHHHHMMMMMMMM'                    
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
* DDSHFI                                                                        
       ++INCLUDE DDSHFIR                                                        
         EJECT                                                                  
SOH      EQU   X'01'                                                            
STX      EQU   X'02'                                                            
ETX      EQU   X'03'                                                            
FF       EQU   X'0C'                                                            
CR       EQU   X'0D'                                                            
DLE      EQU   X'10'                                                            
NL       EQU   X'15'                                                            
ETB      EQU   X'17'                                                            
EM       EQU   X'19'                                                            
LF       EQU   X'25'                                                            
ESC      EQU   X'27'                                                            
DC2      EQU   X'12'                                                            
DC4      EQU   X'3C'                                                            
*                                                                               
MAXERRP  DS    0F                  MAX PAGES TO PRINT FOR ERROR REPORT          
*&&UK*&& DC    F'9999999'                                                       
*&&US*&& DC    F'1'                                                             
*                                                                               
PUBMIN   DC    AL2(32000)          PUBLIC USER IDS IN THIS RANGE                
PUBMAX   DC    AL2(32100)                                                       
*                                                                               
QPOSMAX  DC    H'8'                                                             
MAXRBN   DC    H'10'                                                            
         DC    H'0'                                                             
LPRNBUFF DC    H'1900'                                                          
PQBLKLN  DC    H'14336'                                                         
*                                                                               
SHMUSS   DC    CL8'SHMUSS'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
ATTACH   DC    CL8'ATTACH'                                                      
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
GLIST    DC    CL8'GLIST'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
PRTQUE   DC    CL8'PRTQU'                                                       
CTFILE   DC    CL8'CTFILE'                                                      
TEMPSTR  DC    CL8'TEMPSTR'                                                     
MEMORY   DC    CL8'PRTQ'                                                        
SPACES   DC    CL16' '                                                          
*                                                                               
BSCSOB   DC    XL04'0227F5C8'      STX/ESC/WOP/WCC                              
MINIH    DC    CL10'$$P200    '    $$P2BINNNN                                   
MINILOGI DC    CL04'$PQ '                                                       
         EJECT                                                                  
***********************************************************************         
* TABLE OF VALID IBM CARRIAGE CONTROL CHARACTERS                      *         
* XL1    IBM CC CHR                                                   *         
* XL1    01=CC CHR HAS DATA                                           *         
* XL1    NUMBER OF LINES TO SKIP (X'FF'=CHANNEL 1)                    *         
***********************************************************************         
CCTAB    DS    0XL3                                                             
         DC    X'010101'           DATA THEN SPACE 0                            
CCTS1    DC    X'090101'           DATA THEN SPACE 1                            
         DC    X'0B0001'           IMEADIATE SPACE 1                            
CCTS2    DC    X'110102'           DATA THEN SPACE 2                            
         DC    X'130002'           IMEADIATE SPACE 2                            
CCTS3    DC    X'190103'           DATA THEN SPACE 3                            
         DC    X'1B0003'           IMEADIATE SPACE 3                            
CCTC1    DC    X'8901FF'           DATA THEN CNANL 1                            
         DC    X'8B00FF'           IMEADIATE CNANL 1                            
         DC    X'000000'                                                        
*                                                                               
TWXMSGS  DC    C' *** START OF PRINT *** '                                      
TWXMSGX  DS    0CL34                                                            
         DC    C' 4 3'                                                          
         DC    C' ***  END OF PRINT  *** '                                      
         DC    X'151515151515'                                                  
*                                                                               
ERRMSG   DS    0CL44                                                            
         DC    X'15'                                                            
         DC    C' *** ERROR *** *** ERROR *** *** ERROR ***'                    
         DC    X'15'                                                            
*                                                                               
ARCLINE  DC    X'004D09'           <ARC=AA,SPP,FFFF  ,UUUUU,UUUUU,...>          
         DC    C'<'                                                             
ARCCARD  DC    CL72'ARC=AA,SPP,FFFF  ,NNNNN,NNNNN, ,A'                          
         DC    C'>'                                                             
ARCLINEL EQU   *-ARCLINE                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TRANSLATE TABLE OF VALID OUTPUT CHARACTERS                          *         
* BOX CHRS 8F,AB,AC,BB,BC,BF,CB,CC,EB,EC,FA ARE TRANSLATED TO + - :   *         
* SHADING CHR 42 IS TRANSLATED TO SPACE                               *         
***********************************************************************         
VALOCHRS DC    XL16'40404040404040404040404040404040'  00-0F                    
         DC    XL16'40404040404040404040404040404040'  10-1F                    
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404040404040404040404A4B4C4D4E4F'  40-4F                    
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60-6F                    
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'4081828384858687888940404040404E'  80-8F                    
         DC    XL16'4091929394959697989940404040409F'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A9404E4E404040'  A0-AF                    
         DC    XL16'40404040404040404040404E4E404060'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404E4E404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-D1                    
         DC    XL16'E040E2E3E4E5E6E7E8E9404E4E404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F97A4040404040'  F0-FF                    
         EJECT                                                                  
***********************************************************************         
* TRANSLATE TABLE OF VALID OUTPUT CHARACTERS FOR LASER PRINTERS       *         
* OCRA CHRS ALSO APPEAR IN LASER CHR SET                              *         
* 6D     SQUARE Y SHAPED CHR                                          *         
* 79     SQUARE BACKWARD LITTLE H CHR                                 *         
* A1     SQUARE S SHAPED CHR                                          *         
***********************************************************************         
VALLCHRS DC    XL16'40404040404040404040404040404040'  00-0F                    
         DC    XL16'40404040404040404040404040404040'  10-1F                    
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404040404040404040404A4B4C4D4E4F'  40-4F                    
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60-6F                    
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'4081828384858687888940404040408F'  80-8F                    
         DC    XL16'4091929394959697989940404040409F'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A940ABAC404040'  A0-AF                    
         DC    XL16'4040404040404040404040BBBC4040BF'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C940CBCC404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-D1                    
         DC    XL16'E040E2E3E4E5E6E7E8E940EBEC404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FA4040404040'  F0-FF                    
         EJECT                                                                  
***********************************************************************         
* ALL CHARACTERS ABOVE X'40' ARE CONSIDERED VALID                     *         
* BOX CHRS 8F,AB,AC,BB,BC,BF,CB,CC,EB,EC,FA ARE TRANSLATED TO + - :   *         
* SHADING CHR 42 IS TRANSLATED TO SPACE                               *         
***********************************************************************         
OUTALLC  DC    XL16'40404040404040404040404040404040'  00-0F                    
         DC    XL16'40404040404040404040404040404040'  10-1F                    
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404140434445464748494A4B4C4D4E4F'  40-4F 42                 
         DC    XL16'505152535455565758595A5B5C5D5E5F'  50-5F                    
         DC    XL16'606162636465666768696A6B6C6D6E6F'  60-6F                    
         DC    XL16'707172737475767778797A7B7C7D7E7F'  70-7F                    
         DC    XL16'808182838485868788898A8B8C8D8E4E'  80-8F 8F                 
         DC    XL16'909192939495969798999A9B9C9D9E9F'  90-9F                    
         DC    XL16'A0A1A2A3A4A5A6A7A8A9AA4E4EADAEAF'  A0-AF AB/AC              
         DC    XL16'B0B1B2B3B4B5B6B7B8B9BA4E4EBDBE60'  B0-BF BB/BC/BF           
         DC    XL16'C0C1C2C3C4C5C6C7C8C9CA4E4ECDCECF'  C0-CF CB/CC              
         DC    XL16'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'  D0-D1                    
         DC    XL16'E0E1E2E3E4E5E6E7E8E9EA4E4EEDEEEF'  E0-EF EB/EC              
         DC    XL16'F0F1F2F3F4F5F6F7F8F97AFBFCFDFEFF'  F0-FF FA                 
         EJECT                                                                  
***********************************************************************         
* TRANSLATE TABLE OF VALID OUTPUT CHARACTERS FOR LASER PRINTERS       *         
* ALL CHARACTERS ABOVE X'40' ARE DEFINED AS VALID                     *         
* BOX CHARACTERS ARE 8F,AB,AC,AD,BB,BC,BD,BF,CB,CC,EB,EC,FA AND 42    *         
***********************************************************************         
OUTALLCL DC    XL16'00404040404040404040404040404040'  00-0F                    
         DC    XL16'40404040404040404040404040404040'  10-1F                    
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404142434445464748494A4B4C4D4E4F'  40-4F                    
         DC    XL16'505152535455565758595A5B5C5D5E5F'  50-5F                    
         DC    XL16'606162636465666768696A6B6C6D6E6F'  60-6F                    
         DC    XL16'707172737475767778797A7B7C7D7E7F'  70-7F                    
         DC    XL16'808182838485868788898A8B8C8D8E8F'  80-8F                    
         DC    XL16'909192939495969798999A9B9C9D9E9F'  90-9F                    
         DC    XL16'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'  A0-AF                    
         DC    XL16'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'  D0-DF                    
         DC    XL16'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'  F0-FF                    
         EJECT                                                                  
***********************************************************************         
* WRITE PRINTER CHECKPOINT RECORD TO DISK                             *         
***********************************************************************         
CHKWRT   NTR1  BASE=*                                                           
         TM    PATTR,ATTRCHK       EXIT IF NO CHECK POINT DEFINED               
         BZ    CHKWX                                                            
         L     R5,SAVR1                                                         
         L     R5,20(R5)           POINT TO TWA                                 
         LR    R0,R5                                                            
         LH    R1,RECLEN           CLEAR TWA                                    
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
CHKW1    MVC   16(8,R5),=C'*PRTCHK*' SET HEADER TO SHOW T105 CHKPNT             
         MVC   24(2,R5),PNUM                                                    
         MVI   27(R5),05                                                        
         MVC   28(4,R5),MVSDATE                                                 
         MVC   32(4,R5),MVSTIME                                                 
         MVC   36(8,R5),PRNSYM                                                  
*                                                                               
CHKW3    XC    HALF,HALF           HALF=LAST ENTRY NUMBER                       
         LLC   R0,PRQNE                                                         
         LR    R1,R0               R1=NUMBER OF ENTRYS                          
         CHI   R1,1                                                             
         BH    CHKW6                                                            
         BE    CHKW5                                                            
*                                                                               
CHKW4    OC    PNTRY,PNTRY         CHECK ZERO ENTRY QUEUE                       
         BZ    CHKW8                                                            
         OI    PNQOK,X'01'         INVALID FIRST ENTRY                          
         XC    PNTRY,PNTRY                                                      
         B     CHKW8                                                            
*                                                                               
CHKW5    OC    PNNEXT,PNNEXT       CHECK SINGLE ENTRY QUEUE                     
         BZ    CHKW5A                                                           
         OI    PNQOK,X'02'         INVALID LAST ENTRY (NON ZERO LINK)           
         XC    PNNEXT,PNNEXT                                                    
CHKW5A   OC    PNTRY(8),PNTRY                                                   
         BNZ   CHKW8                                                            
         OI    PNQOK,X'01'         INVALID FIRST ENTRY                          
         XC    PNTRY,PNTRY                                                      
         SR    R1,R1                                                            
         B     CHKW8                                                            
*                                                                               
CHKW6    OC    PNTRY(8),PNTRY      CHECK MULTIPLE ENTRY QUEUE                   
         BNZ   CHKW7                                                            
         OI    PNQOK,X'01'         INVALID FIRST ENTRY                          
         XC    PNTRY,PNTRY                                                      
         SR    R1,R1                                                            
         B     CHKW8                                                            
*                                                                               
CHKW7    LA    R5,64+PRQDL(R5)     R5=A(NEXT PRQ ENTRY SLOT IN TWA)             
         BCTR  R0,0                R0=NUM OF POOL ENTRIES                       
         LA    RE,PNTRY            RE=A(PREV PRQ ENTRY)                         
         LA    R1,1                R1=ACTUAL NUMBER OF QUEUE ENTRIES            
*                                                                               
CHKW7A   SR    RF,RF               GET NEXT ENTRY NUMBER                        
         ICM   RF,3,PNNEXT-PNTRY(RE)                                            
         BZ    CHKW8                                                            
         STH   RF,HALF1            HALF1=NEXT ENTRY NUMBER                      
         BCTR  RF,0                INDEX INTO ENTRY POOL                        
         MH    RF,LPRQES                                                        
         A     RF,APRQES           RF=A(CURR PRQ ENTRY)                         
         OC    0(8,RF),0(RF)       CHECK VALID ENTRY                            
         BNZ   CHKW7B                                                           
         OI    PNQOK,X'04'         FORWARD LINK POINTS TO ZERO ENTRY            
         XC    PNNEXT-PNTRY(2,RE),PNNEXT-PNTRY(RE)                              
         LA    R0,L'PNTRY                                                       
         SR    R5,R0                                                            
         XC    PNNEXT-PNTRY(2,R5),PNNEXT-PNTRY(R5)                              
         AR    R5,R0                                                            
         B     CHKW8                                                            
CHKW7B   MVC   0(L'PNTRY,R5),0(RF) MOVE POOL ENTRY TO TWA SLOT                  
         LA    R1,1(R1)                                                         
         LA    R5,L'PNTRY(R5)      BUMP TO NEXT SLOT IN TWA                     
         MVC   HALF,HALF1          HALF=LAST ENTRY NUMBER                       
         LR    RE,RF                                                            
         BCT   R0,CHKW7A                                                        
*                                                                               
CHKW7C   OC    PNNEXT-PNTRY(2,RE),PNNEXT-PNTRY(RE)                              
         BZ    CHKW8                                                            
         OI    PNQOK,X'02'         INVALID LAST ENTRY (NON ZERO LINK)           
         XC    PNNEXT-PNTRY(2,RE),PNNEXT-PNTRY(RE)                              
         LA    R0,L'PNTRY                                                       
         SR    R5,R0                                                            
         XC    PNNEXT-PNTRY(2,R5),PNNEXT-PNTRY(R5)                              
         AR    R5,R0                                                            
*                                                                               
CHKW8    CLM   R1,1,PRQNE          TEST VALID NUM OF QUEUE ENTRIES              
         BE    CHKW8A                                                           
         OI    PNQOK,X'80'         SET INVALID COUNT                            
         STC   R1,PRQNE                                                         
CHKW8A   CLC   PNLAST,HALF         TEST VALID LAST QUEUE ENTRY NUMBER           
         BE    CHKW8B                                                           
         OI    PNQOK,X'40'         SET INVALID LAST ENTRY NUMBER                
         MVC   PNLAST,HALF                                                      
CHKW8B   L     R5,SAVR1                                                         
         L     R5,20(R5)                                                        
         LA    R5,64(R5)           MOVE FIXED PART OF PRQ TO TWA                
         MVC   0(PRQDL,R5),PRHDR                                                
         LA    RE,PRQUTLA-PRHDR(R5)                                             
         MVI   0(RE),0                                                          
         MVC   1(2,RE),PNUM        OVERWRITE A(UTL) WITH PNUM                   
*                                                                               
CHKWW    L     R1,SAVR1                                                         
         MVI   0(R1),X'FF'         SET WRITE TWA RETURN FLAG                    
CHKWX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PASS   DUB+0(3)  DATEC/TIMEI (DATE COMPRESSED AND TIME INCR)        *         
* PASS   DUB+3(2)  RETENTION PERIOD IN HOURS                          *         
* RETURN DUB1+0(3) DATEC/TIMEI (OLD DATE/TIME PLUS RETENTION)         *         
***********************************************************************         
GETRETN  NTR1  BASE=*                                                           
*                                                                               
         XC    DUB1,DUB1           INITIALISE RETURN VALUES                     
         XC    GRCB,GRCB                                                        
         MVC   GRCTRY,PCTRY        SET COUNTRY CODE OF DEVICE                   
         MVC   GRHOURS,DUB+3       SET BINARY HOURS RETAIN PERIOD               
*                                                                               
         CLC   DATEC,DATEO         TEST NEW/OLD CMPRSD DATES                    
         BE    GRETN0                                                           
         GOTO1 ADATCON,DUB2,(14,DUB),(3,GRIYEAR)                                
         B     GRETN01                                                          
GRETN0   GOTO1 ADATCON,DUB2,(2,DUB),(3,GRIYEAR)                                 
*                                                                               
GRETN01  MVC   GRIHOUR(2),TIMEB    SET BINARY HOURS/MINUTES                     
*                                                                               
*                                                                               
*                                                                               
         OI    GRFLAG,GRDF2028     ALLOW YEARS GREATER THAN 2027                
         LA    R1,GRCB                                                          
         L     RF,AGETRET                                                       
         BASR  RE,RF               GO TO V(GETRET)                              
*                                                                               
         MVC   DUB2+4(3),GROYEAR   RETURN YEAR/MONTH/DAY                        
         CLC   DATEC,DATEO         TEST NEW/OLD CMPRSD DATES                    
         BE    GRETN11                                                          
         GOTO1 ADATCON,DUB2,(3,GROYEAR),(30,DUB1)                               
         B     GRETN12                                                          
GRETN11  GOTO1 ADATCON,DUB2,(3,GROYEAR),(2,DUB1)                                
*                                                                               
GRETN12  LLC   R0,GROMIN                                                        
         LLC   R1,GROHOUR                                                       
         MHI   R1,60                                                            
         AR    R1,R0               R1=BINARY MINUTES                            
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,DUB1+2           RETURN NEW TIME IN 10MIN INCREMENTS          
         B     GRETNX                                                           
*                                                                               
GRETNX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET ESCAPE SEQUENCE IN BUFFER AT START OF NEW REPORT                *         
***********************************************************************         
ESCDATA  NTR1  BASE=*                                                           
         TM    PATT2,ATTRESC       EXIT IF NO ESCAPE SEQUENCE DEFINED           
         BZ    ESCDX                                                            
         CLI   PESC,0                                                           
         BE    ESCDX                                                            
*                                                                               
         LA    RF,P                                                             
         USING ESCKEYD,RF                                                       
         XC    ESCKEY,ESCKEY       BUILD ESCAPE SEQUENCE RECORD KEY             
         MVI   ESCKSYS,ESCKSYSQ                                                 
         MVI   ESCTYPE,ESCTYPEQ                                                 
         MVC   ESCNBR,PESC                                                      
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),CTFILE,P,P                          
         CLI   8(R1),0                                                          
         BNE   ESCDX               ESCAPE SEQUENCE RECORD NOT FOUND             
*                                                                               
         LA    RF,P+28             SEARCH FOR ESCAPE ELEMENT                    
         USING ESCVALD,RF                                                       
         SR    R1,R1                                                            
ESCD1    CLI   0(RF),0                                                          
         BE    ESCDX               ESCAPE ELEMENT NOT FOUND                     
         CLI   0(RF),ESCVALEQ                                                   
         BE    ESCD3                                                            
ESCD2    IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     ESCD1                                                            
*                                                                               
ESCD3    IC    R1,ESCVALLN         R1=LEN OF ESCAPE SEQUENCE                    
         SH    R1,=Y(ESCVALOV)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ESCVALTX    PUT ESCAPE SEQUENCE IN BUFFER                
         LA    R3,1(R3,R1)                                                      
         B     ESCD2               BACK TO SEE IF THERE IS ANOTHER              
*                                                                               
ESCDX    XIT1  REGS=(R3)                                                        
         DROP  RF                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET POSTSCRIPT START OF REPORT DATA                                 *         
***********************************************************************         
PSSRDATA NTR1  BASE=*                                                           
         OC    PAGNADR,PAGNADR     REMOVE PAGE NUM IF IN BUFFER                 
         BZ    *+8                                                              
         L     R3,PAGNADR                                                       
         MVC   DUB(5),PNFAFP       GET FORMS CODE                               
         OC    DUB(5),SPACES                                                    
         SR    R1,R1                                                            
         LA    RE,DUB                                                           
PSSR0    CLI   0(RE),C' '          FIND LENGTH OF FORMS CODE                    
         BE    PSSR0A                                                           
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         CHI   R1,5                                                             
         BL    PSSR0                                                            
PSSR0A   LTR   R1,R1               TEST NO FORMS CODE                           
         BNZ   PSSR0B                                                           
         MVC   DUB(5),=C'STD  '                                                 
         LA    R1,3                                                             
PSSR0B   BCTR  R1,0                R1=FORMS LEN-1                               
         TM    PATT3,ATTRPFLM      ATT3=B FOR FLASH MEMORY LOAD                 
         BO    PSSR2                                                            
         TM    PATT3,ATTRPOSA      ATT3=D FOR OSA CONNECT                       
         BO    PSSR3                                                            
*                                                                               
PSSR1    MVC   0(PSST1B-PSST1A,R3),PSST1A TRIGGER FOR HARD DISK FORM            
         LA    R3,PSST1B-PSST1A(R3)                                             
         MVI   0(R3),C'('                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),DUB         FORMS CODE                                   
         LA    R3,2(R1,R3)                                                      
         MVC   0(6,R3),PSST1B+6                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),DUB         FORMS CODE                                   
         LA    R3,7(R1,R3)                                                      
         MVC   0(1,R3),PSST1B+17                                                
         LA    R3,1(R3)                                                         
         MVC   0(PSST1X-PSST1C,R3),PSST1C                                       
         LLC   R0,PNCAFP                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB+6(2)    MAX CHRS PER LINE                            
         LA    R3,PSST1X-PSST1C(R3)                                             
         B     PSSRP                                                            
*                                                                               
PSSR2    MVC   0(PSST2B-PSST2A,R3),PSST2A TRIGGER FOR FLASH MEM FORM            
         LA    R3,PSST2B-PSST2A(R3)                                             
         MVC   0(8,R3),PSST2B                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),DUB         FORMS CODE                                   
         LA    R3,9(R1,R3)                                                      
         MVC   0(6,R3),PSST2B+13                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),DUB         FORMS CODE                                   
         LA    R3,7(R1,R3)                                                      
         MVC   0(1,R3),PSST2B+24                                                
         LA    R3,1(R3)                                                         
         MVC   0(PSST2X-PSST2C,R3),PSST2C                                       
         LLC   R0,PNCAFP                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB+6(2)    MAX CHRS PER LINE                            
         LA    R3,PSST2X-PSST2C(R3)                                             
         B     PSSRP                                                            
*                                                                               
PSSR3    MVC   0(PSST3B-PSST3A,R3),PSST3A TRIGGER PLANET PRESS OSA              
         LA    R3,PSST3B-PSST3A(R3)                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),DUB         FORMS CODE                                   
         LA    R3,1(R1,R3)                                                      
         MVC   0(1,R3),PSST3B+5                                                 
         LA    R3,1(R3)                                                         
         MVC   0(PSST3X-PSST3C,R3),PSST3C                                       
         LLC   R0,PNCAFP                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB+6(2)    MAX CHRS PER LINE                            
         LA    R3,PSST3X-PSST3C(R3)                                             
         B     PSSRP                                                            
*                                                                               
PSSRP    OC    PAGNADR,PAGNADR     TEST IF THERE WAS PAGE NUM IN BUFFER         
         BZ    PSSRX               NO                                           
         L     R1,PAGNLEN          YES GET LENGTH OF PAGE NUM DATA              
         AHI   R1,-1                                                            
         BM    PSSRX                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PAGN        RESTORE PAGE NUMBER                          
         ST    R3,PAGNADR                                                       
         LA    R3,1(R1,R3)                                                      
*                                                                               
PSSRX    XIT1  REGS=(R3)           LEAVE R3 AT END OF POSTSCRIPT HEADER         
*                                                                               
         LTORG                                                                  
*                                                                               
PSST1    EQU   *                   FORM LOADED FROM DISK                        
PSST1A   DC    C'%!PS-Adobe',X'15'                                              
PSST1B   DC    C'(REPID) run REPID',X'15'                                       
PSST1C   DC    C'999',X'15'                                                     
PSST1X   EQU   *                                                                
*                                                                               
PSST2    EQU   *                   FORM LOADED FROM FLASH MEMORY                
PSST2A   DC    C'%!PS-Adobe',X'15'                                              
PSST2B   DC    C'(%flash%REPID) run REPID',X'15'                                
PSST2C   DC    C'999',X'15'                                                     
PSST2X   EQU   *                                                                
*                                                                               
PSST3    EQU   *                   PRINTER SOURCE FOR OSA                       
PSST3A   DC    C'%!Adobe',X'15'                                                 
PSST3B   DC    C'REPID',X'15'                                                   
PSST3C   DC    C'999',X'15'                                                     
PSST3X   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* COMPRESS PRINT LINE AT P AND RETURN NEW LENGTH IN NUMCHR            *         
***********************************************************************         
COMPRESS NTR1  BASE=*                                                           
         LA    R6,P                R6=A(START OF STRING)                        
         LH    R1,NUMCHR           R1=LENGTH OF STRING                          
         LA    RE,0(R1,R6)                                                      
         MVI   0(RE),X'FF'         SET END OF STRING                            
*                                                                               
COMP1    CLI   0(R6),X'FF'         SEARCH FOR STRING OF MIN LEN                 
         BE    COMPX                                                            
         CLC   0(L'COMPMIN-1,R6),1(R6)                                          
         BE    COMP3                                                            
COMP2    LA    R6,1(R6)                                                         
         B     COMP1                                                            
*                                                                               
COMP3    CLC   0(1,R6),COMPMAX     IGNORE STRING IF CHR CANT COMPRESS           
         BL    *+14                                                             
         CLC   0(1,R6),COMPMAX+L'COMPMAX-1                                      
         BNH   COMP2                                                            
         LA    R4,L'COMPMIN(R6)    R4=A(END OF STRING + 1)                      
         LA    R0,L'COMPMAX-1      R0=MAX LEN OF STRING                         
         CLC   0(1,R4),0(R6)                                                    
         BNE   COMP4                                                            
         LA    R4,1(R4)                                                         
         BCT   R0,*-14                                                          
*                                                                               
COMP4    LR    RF,R4               RF=LEN OF STRING                             
         SR    RF,R6                                                            
         IC    R0,COMPMIN(RF)      R0=LEN OF STRING CODE CHR                    
         MVI   0(R6),ESC                                                        
         CLI   1(R6),C' '          ESC/LEN FOR BLANK STRING                     
         BE    *+8                                                              
         LA    R6,1(R6)            ESC/CHR/LEN FOR NON BLANK CHR STRING         
         STC   R0,1(R6)                                                         
         LA    R6,2(R6)                                                         
*                                                                               
COMP5    LR    RF,R4               RF=NUM OF CHRS TO DELETE                     
         SR    RF,R6                                                            
         LA    RE,P(R1)            RE=NUM OF CHRS TO SHIFT LEFT                 
         SR    RE,R4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R4)                                                    
         SR    R1,RF               DECR TOTAL DATA LENGTH                       
         B     COMP1                                                            
*                                                                               
COMPX    STH   R1,NUMCHR           SET COMPRESSED LEN OF DATA                   
         XIT1                                                                   
*                                                                               
COMPMIN  DC    CL04' '                                                          
COMPMAX  DC    CL36'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                       
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET REPORT DATA FOR REMOTE MINI TERMINAL - FULL HAS VALUE FLAG      *         
***********************************************************************         
MINIDATA NTR1  BASE=*                                                           
         ST    R3,SAVR0            SAVE CURRENT BUFFER POINTER                  
         L     R3,PBUFF            SET FIRST BUFFER FLAG                        
         LA    R3,4(R3)            POINT TO FIRST MINI HEADER BYTE              
         OC    5(1,R3),FULL        SET HEADER TYPE FROM FULL                    
         LA    R3,L'MINIH(R3)      BUMP PAST STANDARD MINI HEADER               
         USING MHD,R3                                                           
         TM    MODE+2,MODEHDR      TEST IF HAVE MINI HEADER ALREADY             
         BZ    MIN0                NO                                           
         L     R3,SAVR0            YES RESTORE BUFFER POINTER                   
         B     MINX                                                             
*                                                                               
MIN0     XC    SAVR0,SAVR0         INITIALISE MINI HEADER                       
         OI    MODE+2,MODEHDR                                                   
         XC    FULL,FULL           CLEAR FULL FOR ARITHMETIC                    
         MVI   MHD,C' '                                                         
         MVC   MHD+1(MHEND-MHD),MHD                                             
*                                                                               
MIN1     MVC   MHSUBID,PQSUBID     SET REPORT ID AND CLASS                      
         MVC   FULL+2(2),PQREPNO                                                
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB1,DUB                                                         
         MVC   MHREPNO(4),DUB1+4   SET LOW ORDER 4 DIGITS                       
         MVC   MHREPNX(1),DUB1+3   SET HIGH ORDER DIGIT                         
         MVC   MHCLASS,PQCLASS                                                  
*                                                                               
MIN2     CLC   PQMAKER(3),SPACES   SET DESC SPP,XXXXXXX                         
         BNH   MIN2B                                                            
         MVC   MHDESC(3),PQMAKER                                                
         CLC   PQDESC(7),SPACES                                                 
         BNH   MIN2C               NO DESCRIPTION                               
         CLC   PQDESC(3),PQMAKER                                                
         BNE   MIN2A                                                            
         CLI   PQDESC+3,C' '                                                    
         BNH   MIN2C               DESCRIPTION SAME AS MAKER                    
MIN2A    MVI   MHDESC+3,C','                                                    
         MVC   MHDESC+4(7),PQDESC                                               
         MVC   MHMAKERX,PQMAKER+3                                               
         MVC   MHDESCX,PQDESC+7                                                 
         B     MIN2C                                                            
MIN2B    MVC   MHDESC,PQDESC       SET FULL DESCRIPTION                         
         MVI   MHDESCTY,C'F'                                                    
MIN2C    OC    MHDESC,SPACES                                                    
         OC    MHDESCX,SPACES                                                   
         OC    MHMAKERX,SPACES                                                  
         L     RF,=A(VALOCHRS)                                                  
         A     RF,RELO                                                          
         TR    MHDESC,0(RF)        REMOVE INVALID DATA CHRS                     
         TR    MHDESCX,0(RF)                                                    
         TR    MHMAKERX,0(RF)                                                   
*                                                                               
         SR    R0,R0               SET LINES AND PAGES                          
         ICM   R0,7,PQLINES                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MHLINES,DUB                                                      
         SR    R0,R0                                                            
         ICM   R0,3,PQPAGES                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MHPAGES,DUB                                                      
*                                                                               
MIN2D    TM    PQATTB,PQATERR      ERROR REPORTS                                
         BZ    *+12                                                             
         MVI   MHTYPE,C'E'         TYPE E                                       
         B     MIN2E                                                            
*                                                                               
         MVI   MHTYPE,C'O'         DEFAULT TO OVERNIGHT                         
         TM    PQTYPE,PQTYUPDT                                                  
         BNO   *+12                                                             
         MVI   MHTYPE,C'U'         SET TO UPDATIVE                              
         B     MIN2E                                                            
*                                                                               
         TM    PQATTB,PQATJOBO     IF JOBO                                      
         BNO   *+12                                                             
         MVI   MHTYPE,C'S'         SET TO SOON                                  
         B     MIN2E                                                            
*                                                                               
         TM    PQTYPE,PQTYONL      IF ONLINE                                    
         BNO   *+8                                                              
         MVI   MHTYPE,C'N'         SET TO NOW                                   
                                                                                
MIN2E    TM    PQTYPE,PQTYDL       SET UC IF DOWNLOAD                           
         BO    *+8                                                              
         NI    MHTYPE,255-X'40'                                                 
*                                                                               
MIN3     SR    R0,R0               SET CPL AND LPP                              
         ICM   R0,3,PQAVCPL                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MHCPL,DUB                                                        
         LLC   R0,PQLPP                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MHLPP,DUB                                                        
*                                                                               
         MVC   HALF2,PQDATEL       SET DATE FROM CMPRSD FORMAT                  
         TM    PQTYP1,PQTYNCD                                                   
         BO    MIN3A                                                            
         GOTO1 ADATCON,DMCB,(2,HALF2),(30,HALF2)                                
MIN3A    SR    R0,R0                                                            
         ICM   R0,3,HALF2                                                       
         SRDL  R0,5                                                             
         SRL   R1,27                                                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MHDATEC+4(2),DUB+6(2)                                            
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MHDATEC+2(2),DUB+6(2)                                            
         AHI   R0,64               BASE YEAR IS 1964 FOR NEW CMPRSD             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MHDATEC+0(2),DUB+6(2)                                            
*                                                                               
         SR    R0,R0               SET TIME                                     
         ICM   R0,3,PQTIMEL                                                     
         SRDL  R0,8                                                             
         SRL   R1,24                                                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MHTIMEC+2(2),DUB+6(2)                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MHTIMEC+0(2),DUB+6(2)                                            
*                                                                               
MIN4     XC    CXREC(25),CXREC     SET SOURCE ID NAME                           
         MVI   CXREC,C'I'                                                       
         MVC   CXREC+23(2),PQSRCID                                              
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),CTFILE,CXREC,CXREC                  
         CLI   8(R1),0                                                          
         BE    MIN4B                                                            
MIN4A    MVC   FULL+2(2),PQSRCID   SET SOURCE ID NUM IF NO NAME AVAIL           
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MHIDA,DUB                                                        
         B     MIN4X                                                            
MIN4B    LA    RE,CXREC+28         SEARCH FOR ID NAME ELEMENT                   
         SR    RF,RF                                                            
MIN4C    CLI   0(RE),0                                                          
         BE    MIN4A                                                            
         CLI   0(RE),2                                                          
         BE    MIN4D                                                            
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     MIN4C                                                            
MIN4D    MVC   MHIDA(8),2(RE)                                                   
MIN4X    DS    0H                                                               
*                                                                               
MIN5     TM    PQATTB,PQATPW       TEST SECURE REPORT                           
         BZ    MIN6                                                             
         TM    PQSECF1,PQSINONO    TEST VALID SECURITY INFO BYTE                
         BO    MIN6                                                             
         TM    PQSECF1,PQSIPIN     TEST IF PIN ATTACHED TO REPORT               
         BZ    MIN6                                                             
         MVC   MHPSWD,PQPSWD       SET PIN                                      
         OC    MHPSWD,SPACES                                                    
*                                                                               
MIN6     MVC   MHFORMS,PQFORMS     SET FORMS AND CHARS                          
         OC    MHFORMS,SPACES                                                   
         MVC   MHCHARS,PQCHARS                                                  
         OC    MHCHARS,SPACES                                                   
         MVC   DUB(1),PQCOPIES     SET NUMBER OF COPIES                         
         CLI   DUB,C' '                                                         
         BNE   *+8                                                              
         MVI   DUB,0                                                            
         CLI   DUB,C'0'                                                         
         BL    *+8                                                              
         NI    DUB,X'0F'           ALLOW C'N' VALUE                             
         CLI   DUB,0                                                            
         BNE   *+8                                                              
         MVI   DUB,1               DEFAULT IS 1                                 
         LLC   R0,DUB                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MHCOPIES,DUB                                                     
*                                                                               
MINW     MVI   MHEND,NL            SET END OF MINI HEADER                       
         LA    R3,MHEND+1                                                       
MINX     OC    SAVR0,SAVR0         EXIT WITH CC=EQL IF BUILT                    
         XIT1  REGS=(R3)           AND WITH R3 AT END OF NEW HDR                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TEST REMOTE MINI TERMINAL BUFFER AND SET LOG MESSAGE IF NECESSARY   *         
***********************************************************************         
LOGMINI  NTR1  BASE=*                                                           
         L     R3,PBUFF            POINT TO OUTPUT MESSAGE BUFFER               
         LA    R3,4(R3)            POINT TO FIRST MINI HEADER BYTE              
         CLC   0(2,R3),MINIH       EXIT IF NOT VALID MINI HEADER                
         BNE   LOGMX                                                            
         XC    LOGREC,LOGREC       INITIALISE LOG DATA REC                      
*                                                                               
LOGM1    CLC   2(1,R3),MINIH+2     TEST DATA BUFFER                             
         BE    LOGM2                                                            
         CLI   2(R3),C'A'          TEST CONTROL MESSAGE BUFFER                  
         BL    LOGMX                                                            
         CLI   2(R3),C'E'                                                       
         BH    *+14                                                             
         MVC   LOGID(1),2(R3)      ID IS CONTROL MESSAGE CHR A-E                
         B     LOGMA                                                            
         CLI   2(R3),C'M'                                                       
         BE    LOGM5                                                            
         B     LOGMX                                                            
*                                                                               
LOGM2    TM    5(R3),X'01'         TEST FIRST DATA BUFFER                       
         BZ    LOGM2A                                                           
         MVI   LOGID,C'F'          SET FIRST DATA BUFFER ID                     
         B     LOGM3                                                            
LOGM2A   TM    5(R3),X'02'         TEST LAST DATA BUFFER                        
         BZ    LOGM2B                                                           
         MVI   LOGID,C'L'          SET LAST DATA BUFFER ID                      
         MVC   LOGLINES,PRLINES                                                 
         MVC   LOGPAGES,PRPAGES                                                 
         B     LOGM3                                                            
LOGM2B   MVI   LOGID,C'I'          SET INTERMEDIATE DATA BUFFER ID              
         CLC   8(2,R3),=C'00'                                                   
         BE    LOGM3               LOG EVERY 100TH BUFFER                       
         B     LOGMX                                                            
*                                                                               
LOGM3    MVC   DUB1(8),PR1KEY      EXTRACT REPORT KEY FROM PRINTER HDR          
         OC    DUB1(2),DUB1                                                     
         BZ    LOGMX                                                            
         MVC   LOGNUM,6(R3)        SET BUFFER NUMBER                            
         MVC   LOGREPI,DUB1+2      SET REPORT ID                                
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),DUB1+5    SET REPORT SEQUENCE NUMBER                   
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  LOGRENO,DUB                                                      
         OI    LOGRENO+4,X'F0'                                                  
*                                                                               
LOGM4    MVC   LOGREPU,SPACES      SET SOURCE ID NAME                           
         XC    CXREC(25),CXREC                                                  
         MVI   CXREC,C'I'                                                       
         MVC   CXREC+23(2),DUB1                                                 
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),CTFILE,CXREC,CXREC                  
         CLI   8(R1),0                                                          
         BE    LOGM4B                                                           
LOGM4A   MVC   FULL+2(2),DUB1      SET SOURCE ID NUM IF NO NAME AVAIL           
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  LOGREPU,DUB                                                      
         OI    LOGREPU+3,X'F0'                                                  
         B     LOGM4X                                                           
LOGM4B   LA    RE,CXREC+28         SEARCH FOR ID NAME ELEMENT                   
         SR    RF,RF                                                            
LOGM4C   CLI   0(RE),0                                                          
         BE    LOGM4A                                                           
         CLI   0(RE),2                                                          
         BE    LOGM4D                                                           
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     LOGM4C                                                           
LOGM4D   MVC   LOGREPU,2(RE)       SET USER ID NAME IN LOG DATA RECORD          
LOGM4X   B     LOGMA                                                            
*                                                                               
LOGM5    MVI   LOGID,C'M'          ID IS MESSAGE TEXT                           
         MVC   2(1,R3),MINIH+2                                                  
         MVC   LOGTEXT,=CL16'RESHIP REQ BUF#N'                                  
         MVC   LOGTEXT+15(1),PRSVXAC1                                           
         OI    LOGTEXT+15,C'0'                                                  
         B     LOGMA                                                            
*                                                                               
LOGMA    MVC   FULL(1),LOGID       BUILD LOG DATA RECORD                        
         MVC   LOGID(3),MINILOGI                                                
         MVC   LOGID+3(1),FULL                                                  
         L     RE,APRNUTL                                                       
         MVC   LOGLUID,TSYM-UTLD(RE)                                            
         MVC   LOGTIME,TIMEHMS                                                  
*                                                                               
LOGMB    GOTO1 VLOGGER,LOGREC      OUTPUT LOGREC TO ADRFILE                     
         TM    5(R3),X'03'         TEST IF FIRST DATA BUFF IS LAST              
         BNO   LOGMX                                                            
         MVI   LOGID+3,C'L'                                                     
         MVC   LOGLINES,PRLINES                                                 
         MVC   LOGPAGES,PRPAGES                                                 
         BASR  RE,RF               OUTPUT EXTRA LAST BUFF LOGREC                
*                                                                               
LOGMX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOG REMOTE PRINTER ACTIVITY TO ADRFILE                              *         
***********************************************************************         
LOGPRNT  NTR1  BASE=*                                                           
         TM    PATT2,ATTRTRC       EXIT IF NOT TRACING PRINTER                  
         BZ    LOGPX                                                            
         XC    LOGREC,LOGREC       INITIALISE LOG DATA REC                      
         MVC   LOGID(4),=C'$PQQ'   SET ID TO SHOW TRACE                         
         L     RE,APRNUTL                                                       
         MVC   LOGLUID,TSYM-UTLD(RE)                                            
         MVC   LOGTIME,TIMEHMS                                                  
*                                                                               
LOGP1    LH    R0,PRBUFFS          OUTPUT REPORT BUFFER NUMBER                  
         CVD   R0,DUB                                                           
         UNPK  LOGNUM,DUB                                                       
         OI    LOGNUM+3,X'F0'                                                   
*                                                                               
LOGP2    MVC   LOGFLAGS,TRFLAGS    SET MISCELLANEOUS TRACE DATA                 
         MVC   LOGSST(5),TRSTAT    START STATUS                                 
         MVC   LOGSST3,TRSTAT3                                                  
         MVC   LOGSNEX,TRNEX                                                    
         MVC   LOGXST(5),PRSTAT    END STATUS                                   
         MVC   LOGXST3,PRSTAT3                                                  
         MVC   LOGXNEX,PNEX                                                     
         MVC   LOGXLNS,PRLINES+1   LINES AND PAGES PRINTED                      
         MVC   LOGXPGS,PRPAGES                                                  
*                                                                               
LOGP3    MVC   DUB1(8),PR1KEY      EXTRACT REPORT KEY FROM PRINTER HDR          
         OC    DUB1(2),DUB1                                                     
         BNZ   LOGP3A                                                           
         MVC   DUB1(7),TRKEY                                                    
         OC    DUB1(2),DUB1                                                     
         BZ    LOGP4X                                                           
LOGP3A   MVC   LOGREPI,DUB1+2      SET REPORT ID                                
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),DUB1+5    SET REPORT SEQUENCE NUMBER                   
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  LOGRENO,DUB                                                      
         OI    LOGRENO+4,X'F0'                                                  
*                                                                               
LOGP4    MVC   LOGREPU,SPACES      CLEAR SOURCE ID NAME                         
         SR    R0,R0                                                            
         ICM   R0,3,DUB1           GET SOURCE ID NUMBER                         
         BZ    LOGP4X                                                           
         CVD   R0,DUB                                                           
         UNPK  LOGREPU,DUB                                                      
         OI    LOGREPU+5,X'F0'                                                  
         CLC   PRBUFFS,=H'1'       TEST FIRST BUFFER                            
         BH    LOGP4X                                                           
         CLC   PRLINES,=F'100'                                                  
         BH    LOGP4X                                                           
         XC    CXREC(25),CXREC     GET SOURCE ID NAME FOR 1ST BUFFER            
         MVI   CXREC,C'I'                                                       
         MVC   CXREC+23(2),DUB1                                                 
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),CTFILE,CXREC,CXREC                  
         CLI   8(R1),0                                                          
         BNE   LOGP4X                                                           
LOGP4B   LA    RE,CXREC+28         SEARCH FOR ID NAME ELEMENT                   
         SR    RF,RF                                                            
LOGP4C   CLI   0(RE),0                                                          
         BE    LOGP4X                                                           
         CLI   0(RE),2                                                          
         BE    LOGP4D                                                           
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     LOGP4C                                                           
LOGP4D   MVC   LOGREPU,2(RE)       SET USER ID NAME IN LOG DATA RECORD          
LOGP4X   EQU   *                                                                
*                                                                               
LOGPB    GOTO1 VLOGGER,LOGREC      OUTPUT LOGREC TO ADRFILE                     
*                                                                               
LOGPX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT SPECIAL HEADER RECORD FOR RJE TYPE PRINTER        *         
***********************************************************************         
RJEHDR   NTR1  BASE=*                                                           
         ST    R3,SAVR0            SAVE CURRENT BUFFER POINTER                  
         L     R3,PBUFF            POINT TO START OF BUFFER                     
         TM    MODE+2,MODEHDR      TEST IF ALREADY HAVE REPORT HEADER           
         BZ    *+12                NO                                           
         L     R3,SAVR0            YES RESTORE BUFFER POINTER                   
         B     RJEHDRX                                                          
         OI    MODE+2,MODEHDR      SET REPORT HEADER IN BUFFER                  
*                                                                               
RJEHDR1  MVI   0(R3),DLE           LINE=(DLE)(STX)(SOH)DDS0001                  
         MVI   1(R3),STX                                                        
         MVI   2(R3),SOH                                                        
         LA    R3,3(R3)                                                         
         MVC   0(3,R3),=C'DDS'                                                  
         LLC   R2,SYSID            IDENTIFY SYSTEM                              
         SRL   R2,4                                                             
         LA    R2,RJEHDRT-1(R2)                                                 
         MVC   2(1,R3),0(R2)                                                    
         LA    R3,3(R3)                                                         
         LH    RE,PRSVDATA+2       BUMP REPORT SEQUENCE NUMBER                  
         LA    RE,1(RE)                                                         
         CHI   RE,10000            WRAP AT MAXIMUM VALUE                        
         BL    *+8                                                              
         LA    RE,1                                                             
         STH   RE,PRSVDATA+2                                                    
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R3),DUB         MOVE TO REPORT HEADLINE                      
         LA    R3,4(R3)                                                         
         MVI   0(R3),CR                                                         
         MVI   1(R3),LF                                                         
         LA    R3,2(R3)                                                         
*                                                                               
         MVC   0(3,R3),=C'GNT'     LINE=GNT                                     
         LA    R3,3(R3)                                                         
         MVI   0(R3),CR                                                         
         MVI   1(R3),LF                                                         
         LA    R3,2(R3)                                                         
*                                                                               
         MVI   0(R3),C'.'          LINE=.ORIG.                                  
         LA    R3,1(R3)                                                         
         MVC   0(4,R3),P           MOVE ORIGIN                                  
         LA    R3,4(R3)                                                         
         MVI   0(R3),C'('          SET DELIM                                    
         LA    R3,1(R3)                                                         
*                                                                               
         MVC   0(10,R3),P+54       MOVE BILLING                                 
         LA    R3,10(R3)                                                        
         MVI   0(R3),C')'          SET DELIM                                    
         LA    R3,1(R3)                                                         
*                                                                               
         MVC   0(3,R3),=C'ID-'     SET RETURN ID 'ID-UUUUUUIIINNNN'             
         L     R5,ACIREC                                                        
         LH    R0,PQSRCID                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(6,R3),DUB                                                      
         MVC   9(3,R3),PQSUBID                                                  
         MVC   DUB(2),PQREPNO                                                   
         LH    R0,DUB                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  12(4,R3),DUB                                                     
         LA    R3,16(R3)                                                        
         MVI   0(R3),CR                                                         
         MVI   1(R3),LF                                                         
         LA    R3,2(R3)                                                         
*                                                                               
         MVC   0(9,R3),P+4         MOVE '/9FB GFDM'                             
         LA    R3,9(R3)                                                         
         MVI   0(R3),CR                                                         
         MVI   1(R3),LF                                                         
         LA    R3,2(R3)                                                         
*                                                                               
         CLC   P+9(4),=C'GFDM'     IF NOT GFDM ITS A GRAPHNET MNEMONIC          
         BNE   RJEHDR5             AND THEY PROVIDE CALL/ANSWER CODES           
         MVC   0(40,R3),P+13       ELSE WE PROVIDE THEM                         
         LA    R3,40(R3)                                                        
         MVI   0(R3),CR                                                         
         MVI   1(R3),LF                                                         
         LA    R3,2(R3)                                                         
*                                                                               
RJEHDR5  DS    0H                                                               
         MVI   0(R3),C'.'          LINE=.                                       
         LA    R3,1(R3)                                                         
         MVI   0(R3),CR                                                         
         MVI   1(R3),LF                                                         
         LA    R3,2(R3)                                                         
*                                                                               
RJEHDRX  XIT1  REGS=(R3)           EXIT WITH R3 AT END OF HEADER                
*                                                                               
RJEHDRT  DC    C'TASR'             SYSTEM=TEST,ADV,SHUTTLE,REP                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
WRKD     DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
HALF2    DS    H                                                                
HALF3    DS    H                                                                
*                                                                               
SAVRE    DS    F                                                                
SAVRF    DS    F                                                                
SAVR0    DS    F                                                                
SAVR1    DS    F                                                                
*                                                                               
PRTQADTF DS    A                   FILE NUMBER AND AL3(DTF)                     
PRTQID   DS    CL8                                                              
APRTQLST DS    A                                                                
DMCB     DS    6F                                                               
*                                                                               
MODE     DS    XL8                                                              
MODEDIB  EQU   X'80'               MODE+2 DATA IN BUFFER                        
MODECTL  EQU   X'40'               MODE+2 CONTROL MSG IN BUFFER                 
MODEHDR  EQU   X'20'               MODE+2 EXTENDED FILE HEADER IN BUFF          
MODERER  EQU   X'08'               MODE+2 ERROR IN REPORT                       
MODERPP  EQU   X'04'               MODE+2 PARTIAL END OF REPORT                 
MODEEOQ  EQU   X'02'               MODE+2 END OF QUEUE                          
MODEEOR  EQU   X'01'               MODE+2 END OF REPORT                         
*                                                                               
         DS    H                                                                
LPRQES   DS    H                                                                
APRQES   DS    A                                                                
RELO     DS    A                                                                
AGETRET  DS    A                                                                
ADATCON  DS    A                                                                
DISPBUSV DS    F                                                                
ACIREC   DS    A                                                                
ACIRECX  DS    A                                                                
APRNUTL  DS    A                                                                
APRNQ    DS    A                                                                
PBUFF    DS    A                                                                
PBUFFSOD DS    A                                                                
VGENIDS  DS    A                                                                
*                                                                               
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
PRTQINUM DS    X                   PRTQ FILE INTERNAL FILE NUMBER               
PRTQXNUM DS    X                   PRTQ FILE EXTERNAL FILE NUMBER               
TIMEHMS  DS    F                                                                
MVSTIME  DS    F                                                                
MVSDATE  DS    F                                                                
PRNSYM   DS    CL8                                                              
PNUM     DS    H                                                                
PBUFFLEN DS    H                                                                
PSTAT1   DS    XL1                                                              
PSTAT2   DS    XL1                                                              
PSTAT3   DS    XL1                                                              
PSTAT4   DS    XL1                                                              
PSTAT5   DS    XL1                                                              
PTYPE    DS    XL1                                                              
*                                                                               
PATT1    DS    XL1                                                              
ATTRPAG  EQU   X'80'               A=PAGE NUMBERS REQUIRED                      
ATTRLAS  EQU   X'40'               B=LASER SPECIAL CHRS SUPPORTED               
ATTRCHK  EQU   X'20'               C=CHECK POINT DEFINED                        
ATTRNSK  EQU   X'10'               D=NO SKIP AT START OF SESSION                
ATTRNCR  EQU   X'08'               E=NO CR CHR FOR CARRIAGE RETURN              
ATTRNFF  EQU   X'04'               F=NO FF CHR FOR FORM FEED                    
ATTRSAM  EQU   X'02'               G=SELECT AUTO PQ MODE                        
*                                                                               
PATT2    DS    XL1                                                              
ATTRESC  EQU   X'80'               A=ESCAPE SEQUENCE REQUIRED                   
ATTR132  EQU   X'40'               B=SEND NL IF 132 CHR PRINT LINE              
ATTRAFP  EQU   X'20'               C=AFP FORMS CODE SUPPORT                     
ATTRSSS  EQU   X'10'               D=SET SPECIAL STATUS ON PRNT/SEND            
ATTRASS  EQU   X'08'               E=ALLOW SPECIAL STATUS AS ACTV               
ATTRIAT  EQU   X'04'               F=IBM ADVANTIS TERMINAL                      
ATTRTRC  EQU   X'02'               G=TRACE PRINTER ACTIVITY                     
ATTRATOP EQU   X'01'               H=USE ALTERNATIVE SRTOP (T102)               
*                                                                               
PATT3    DS    XL1                                                              
ATTRPSSR EQU   X'80'               A=POSTSCRIPT START OF REPORT                 
ATTRPFLM EQU   X'40'               B=FORM FROM FLASH MEMORY                     
ATTRLAN  EQU   X'20'               C=LAN ATTACHED PRINTER                       
ATTRPOSA EQU   X'10'               D=PLANET PRESS FOR OSA                       
*                                                                               
PESC     DS    XL1                 ESCAPE SEQUENCE CHR                          
PATTR    DS    XL1                 COPY OF PRINTER ATTRIBUTE 1                  
         DS    XL2                                                              
*                                                                               
SVSTAT   DS    XL1                                                              
SVSTAT1  DS    XL1                                                              
SVSTAT2  DS    XL1                                                              
*                                                                               
DATEE    DS    CL8                 EBCDIC DATE C'YYMMDD  '                      
DATEB    DS    XL3                 BINARY DATE X'YMD'                           
DATEC    DS    XL2                 CMPRSD DATE B'YYYYYYYMMMMDDDDD'              
TIMEI    DS    XL1                 BINARY TIME X'I' I=10MIN INTERVAL            
TIMEB    DS    XL2                 BINARY TIME X'HM'                            
*                                                                               
DATEN    DS    XL2                 CMPRSD DATE NEW FORMAT                       
DATEO    DS    XL2                 CMPRSD DATE OLD FORMAT                       
*                                                                               
GRCB     DS    0XL16               GET RETAIN CONTROL BLOCK                     
GRRETC   DS    X                   RETURN CODE                                  
GRCTRY   DS    X                   COUNTRY CODE                                 
GRHOURS  DS    XL2                 NUMBER OF HOURS                              
GRFLAG   DS    XL1                 FLAGS 80=BACK,40=NOHOLS,20=RETCTRY           
GRINFO   DS    XL1                 RETURN CODE                                  
GRIYEAR  DS    X                   INPUT YEAR                                   
GRIMONTH DS    X                   INPUT MONTH                                  
GRIDAY   DS    X                   INPUT DAY                                    
GRIHOUR  DS    X                   INPUT HOUR                                   
GRIMIN   DS    X                   INPUT MIN                                    
GROYEAR  DS    X                   OUTPUT YEAR                                  
GROMONTH DS    X                   OUTPUT MONTH                                 
GRODAY   DS    X                   OUTPUT DAY                                   
GROHOUR  DS    X                   OUTPUT HOUR                                  
GROMIN   DS    X                   OUTPUT MIN                                   
*                                                                               
LINEDATA DS    0XL16                                                            
ALINE    DS    A                   ADR OF LINE REC IN BLOCK                     
LLINE    DS    H                   LEN OF LINE REC IN BLOCK                     
NUMNL    DS    H                   NUM OF NL CHRS REQUIRED                      
NUMCHR   DS    H                   NUM OF DATA CHRS IN P                        
FLINE    DS    X                   X'01'=FIRST LINE,X'80'=END OF REP            
         DS    X                   N/D                                          
*                                                                               
PAGNLEN  DS    F                   LENGTH OF PAGE NUMBER FIELD                  
PAGNADR  DS    A                   ADDRESS IN BUFFER                            
PAGN     DS    CL16                PAGE NUMBER                                  
*                                                                               
RESULT   DS    H                                                                
MAXNAKS  DS    H                                                                
         DS    H                                                                
NSHIFTS  DS    X                                                                
MSHIFTS  DS    X                                                                
EOBCHR   DS    X                                                                
FIXETX   DS    X                                                                
CRCHR    DS    X                                                                
SVPREX   DS    X                                                                
SVCIADDR DS    XL4                                                              
SVKEY    DS    XL8                                                              
SVAGEI   DS    XL7                                                              
         DS    XL5                                                              
*                                                                               
STAID    DS    XL2                                                              
STATY    DS    X                                                                
STANI    DS    X                                                                
STAOI    DS    X                                                                
FLAG     DS    X                                                                
FLAG1    DS    X                                                                
FLAG2    DS    X                                                                
ENQ      DS    X                                                                
SYSID    DS    X                                                                
XLAT     DS    X                                                                
PCTRY    DS    X                                                                
*                                                                               
         DS    0F                                                               
       ++INCLUDE DMPRTQW                                                        
*                                                                               
       ++INCLUDE DDSHFIW                                                        
*                                                                               
CIID     DS    0XL12               COPY OF QUEUE ENTRY                          
CISRCID  DS    XL2                                                              
CISUBID  DS    CL3                                                              
CISEQN   DS    XL2                                                              
CICLASS  DS    XL1                                                              
CIEX     DS    XL1                                                              
CICOPYS  DS    XL1                                                              
CINEXT   DS    XL2                                                              
*                                                                               
CIRID    DS    0XL16               REPORT ID INFO                               
CIRLD    DS    XL2                 LOW DATE                                     
CIRLT    DS    XL2                 LOW TIME                                     
CIRHD    DS    XL2                 HIGH DATE                                    
CIRHT    DS    XL2                 HIGH TIME                                    
CIRXPE   DS    XL4                 REPORT INDEX PAGE/ENTRY                      
CIRNO    DS    XL2                 REPORT NUMBER                                
CIRFLAG  DS    XL1                 REPORT FLAG                                  
         DS    XL1                 N/D                                          
SVCIRID  DS    CL16                                                             
SVCILK1  DS    CL8                                                              
SVCILK2  DS    CL8                                                              
SVCILK3  DS    CL8                                                              
*                                                                               
NDX      DS    CL40                                                             
         DS    XL16                                                             
*                                                                               
LOGREC   DS    0CL80               CURRENT ADRFILE RECORD SIZE                  
*                                                                               
LOGHDR   DS    0CL40                                                            
LOGID    DS    CL4                 $PQ. INTENTIFIES A SRTOP LOG REC             
LOGLUID  DS    CL8                                                              
LOGTIME  DS    PL4                                                              
LOGTEXT  DS    0CL16               FREE FORM TEXT STARTS HERE                   
LOGNUM   DS    CL4                                                              
         DS    CL4                                                              
LOGREPU  DS    CL6                                                              
LOGREPI  DS    CL3                                                              
LOGRENO  DS    CL5                                                              
         DS    CL2                                                              
*                                                                               
LOGMISC  DS    0CL24               MISC INFO - DEPENDS ON REC TYPE              
LOGFLAGS DS    XL1                                                              
*                                                                               
LOGSST   DS    XL1                                                              
LOGSST1  DS    XL1                                                              
LOGSST2  DS    XL1                                                              
LOGSSMOD DS    XL1                                                              
LOGSQNE  DS    XL1                                                              
LOGSST3  DS    XL1                                                              
LOGSNEX  DS    XL1                                                              
*                                                                               
LOGXST   DS    XL1                                                              
LOGXST1  DS    XL1                                                              
LOGXST2  DS    XL1                                                              
LOGXSMOD DS    XL1                                                              
LOGXQNE  DS    XL1                                                              
LOGXST3  DS    XL1                                                              
LOGXNEX  DS    XL1                                                              
*                                                                               
LOGXLNS  DS    XL3                                                              
LOGXPGS  DS    XL2                                                              
         DS    XL4                                                              
*                                                                               
         ORG   LOGMISC                                                          
LOGLDATA DS    0CL24               ENTRY FOR LAST BUFFER LOG RECORD             
LOGLINES DS    XL4                                                              
LOGPAGES DS    XL2                                                              
         DS    CL18                                                             
*                                                                               
TRDATA   DS    0XL16               TRACE DATA FOR LOG RECORD                    
TRFLAGS  DS    XL1                 TRACE FLAGS                                  
TRSTAT   DS    XL1                 START STATUS                                 
TRSTAT1  DS    XL1                 START STATUS 1                               
TRSTAT2  DS    XL1                 START STATUS 2                               
TRQMODE  DS    XL1                 START MODE                                   
TRQNE    DS    XL1                 START QUEUE NUMBER OF ENTRIES                
TRSTAT3  DS    XL1                 START STATUS 3                               
TRNEX    DS    XL1                 START STATUS 3                               
         DS    XL1                                                              
TRKEY    DS    XL7                 START REPORT KEY                             
*                                                                               
LOGRECX  EQU   *                                                                
LOGRECL  EQU   *-LOGREC                                                         
*                                                                               
DCIREC   DS    F                                                                
LCC      DS    XL3                 CCTAB ENTRY FOR CC CHR IN BLOCK              
PCC      DS    X                   PRINT LINE CC CHR                            
P        DS    1024X               PRINT LINE                                   
         ORG   P                                                                
CIRECS   DS    258XL2                                                           
         ORG                                                                    
CXREC    DS    18432C                                                           
CIREC    DS    18432C                                                           
*                                                                               
WRKX     DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
* REPORT HEADER DSECT                                                 *         
***********************************************************************         
MHD      DSECT                     REPORT HEADER DSECT                          
*                                                                               
MHSUBID  DS    CL3                 REPORT SUB ID                                
MHREPNO  DS    CL4                 REPORT NUMBER (LOW ORDER 4 DIGITS)           
MHCLASS  DS    CL1                 REPORT CLASS                                 
MHREPNX  DS    CL1                 REPORT NUMBER (HIGH ORDER DIGIT)             
MHTYPE   DS    CL1                 REPORT TYPE                                  
MHDESC   DS    CL11                MAKER(3),DESC(7) OR DESC(11)                 
MHLINES  DS    CL6                 NUMBER OF LINES                              
MHPAGES  DS    CL6                 NUMBER OF PAGES                              
MHCPL    DS    CL3                 AVERAGE CHRS PER LINE                        
MHLPP    DS    CL3                 LINES PER PAGE                               
MHDATEC  DS    CL6                 DATE CREATED C'YYMMDD'                       
MHTIMEC  DS    CL4                 TIME CREATED C'HHMM'                         
MHIDA    DS    CL6                 USER ID ALPHA                                
MHIDAX   DS    CL2                 USER ID ALPHA EXTENSION                      
MHPSWD   DS    CL4                 REPORT PASSWORD (NOW 4 USED TO BE 6)         
         DS    CL4                 N/D                                          
MHFORMS  DS    CL4                 REPORT FORMS CODE                            
MHCHARS  DS    CL4                 REPORT CHARACTER SET                         
MHCOPIES DS    CL3                 NUMBER OF COPIES                             
MHDESCTY DS    CL1                 SET TO 'F' IF FULL DESC IN MHDESC            
MHMAKERX DS    CL2                 REPORT MAKER EXTENSION                       
MHDESCX  DS    CL4                 REPORT DESCRIPTION EXTENSION                 
*                                                                               
MHEND    DS    CL1                 TERMINATED WITH NL                           
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
* DMPRTQD                                                                       
       ++INCLUDE DMPRTQD                                                        
* DDGETRETD                                                                     
       ++INCLUDE DDGETRETD                                                      
                                                                                
* DMPRTQS                                                                       
       ++INCLUDE DMPRTQS                                                        
                                                                                
* FAPRQ                                                                         
       ++INCLUDE FAPRQ                                                          
                                                                                
* DDSHFID                                                                       
       ++INCLUDE DDSHFID                                                        
                                                                                
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
                                                                                
* FASYSFAC                                                                      
       ++INCLUDE FASYSFAC                                                       
                                                                                
* FASSB                                                                         
       ++INCLUDE FASSB                                                          
                                                                                
* FAUTL                                                                         
       ++INCLUDE FAUTL                                                          
                                                                                
* FADECB                                                                        
       ++INCLUDE FADECB                                                         
                                                                                
* CTGENESC                                                                      
       ++INCLUDE CTGENESC                                                       
                                                                                
* ASCB                                                                          
         IHAASCB LIST=YES                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SRTOP00   11/23/20'                                      
         END                                                                    
