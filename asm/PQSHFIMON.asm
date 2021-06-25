*          DATA SET PQSHFIMON  AT LEVEL 013 AS OF 11/12/20                      
*PHASE PQMONA                                                                   
*INCLUDE FATABOFF                                                               
*INCLUDE DMDMGRL                                                                
*INCLUDE SHFIMON                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE QSORT                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE PERVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DDWTO                                                                  
*INCLUDE TREE                                                                   
*                                                                               
***********************************************************************         
* *NOTES                                                                        
* R8 = A(SHARED MEMORY AREA) AND IS SET IN SHFIMON                              
* R9 = A(SPECIFIC FILE'S INDEX TABLE) AND IS SET IN SHFIMON                     
* RA IS USED BY SHFIMON AND CANNOT BE USED IN THIS MODULE                       
* RC = A(WORKING STORAGE) AND IS PASSED TO SHFIMON                              
***********************************************************************         
         TITLE 'PQMON - PRINT QUEUE INDEX MONITOR'                              
         PRINT NOGEN                                                            
PQMON    CSECT                                                                  
         ENTRY SETFINF             ROUTINES NEEDED BY DDSHFIMON                 
         ENTRY ADDFINF                                                          
         ENTRY FILSCAN                                                          
*                                                                               
         NBASE WORKX-WORKD,**PQMO**,AWORK,CLEAR=YES                             
         USING WORKD,RC            WORKING STORAGE                              
         USING SIHDRD,R8           SHARED MEMORY HEADER                         
         USING SITABD,R9           SHARED MEMORY INDEX TABLE HEADER             
         USING PLINED,PLINE        PRINT LINE                                   
         USING UKRECD,APINDEX      PRINT QUEUE INDEX                            
*                                                                               
         GOTO1 =V(SHFIMON),DMCB,(RC) SHFIMON GETS RC=(WORKING STORAGE)          
*                                                                               
         XBASE                                                                  
*                                                                               
AWORK    DC    A(WORKAREA)                                                      
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET PRINT QUEUE FILE INFORMATION                                              
***********************************************************************         
SETFINF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   FIWCIA,=X'00010100'                                              
         GOTO1 =V(READCI)          READ THE FIRST CI                            
*                                                                               
         L     R4,ACIREC                                                        
         USING CIFDATA,R4                                                       
*                                                                               
         MVC   RIFTRK,FIWCIA                                                    
         MVC   RIFXID,CFFPQXID     FILE EXTERNAL ID (TST/ADV/REP)               
*                                                                               
         MVI   RIDNDX,SI1NDX-SI1PARD    DISPL  TO INDEX    IN NODE              
*        MVI   RIDPREV,                 DISPL  TO PREV CI  IN RECORD            
         MVI   RIDNEXT,PQCINEXT-PQRECD  DISPL  TO NEXT CI  IN RECORD            
         MVI   RIDXKEY,PQKEY-PQINDEX    DISPL  TO KEY      IN INDEX             
         MVI   RIDSEQ,PQSEQ-PQINDEX     DISPL  TO SEQ#     IN INDEX             
         MVI   RIDREF,PQREPNO-PQINDEX   DISPL  TO REPORT # IN INDEX             
         MVI   RIDSTAT,PQSTAT-PQINDEX   DISPL  TO STATUS   IN INDEX             
         MVI   RILNDX,L'PQINDEX         LENGTH OF INDEX                         
*        MVI   RILPREV,                 LENGTH OF PREV CI                       
         MVI   RILNEXT,L'PQCINEXT       LENGTH OF NEXT CI                       
         MVI   RILKEY,L'PQKEY           LENGTH OF KEY = 7 BYTES                 
         MVI   RILREF,L'PQREPNO         LENGTH OF REPORT #                      
         OI    RIVULN,PQSTDEAD          VULNERABLE BITS                         
*                                                                               
         LLH   R1,CIFBLKLN         BLOCK LENGTH                                 
         ST    R1,RIBLKLN          DATA SET BLOCK LENGTH                        
*                                                                               
         MVC   HALF,CIFINDX        INDEX TOTAL NUMBER OF CIS                    
         NI    HALF,X'0F'          TURN OFF FLAGS                               
         LLH   R1,HALF                                                          
         ST    R1,RIXCIC                                                        
         SLL   R1,16                                                            
         ST    R1,RIXCIT                                                        
*                                                                               
         LLH   R1,CIFCITOT         PART1 TOTAL NUMBER OF CI'S                   
         S     R1,RIXCIC           SUBTRACT # OF CIS USED FOR INDEX             
         ST    R1,RI1CIC                                                        
         SLL   R1,16                                                            
         ST    R1,RI1CIT                                                        
*                                                                               
         LLH   R1,CJFCITOT         PART2 TOTAL NUMBER OF CI'S                   
         ST    R1,RI2CIC                                                        
         SLL   R1,16                                                            
         ST    R1,RI2CIT                                                        
*                                                                               
         LLH   R1,CIFTRKS          NUMBER OF TRACKS PER PART1 CI                
         ST    R1,RI1TPC                                                        
         SLL   R1,16                                                            
         ST    R1,RI1TPT                                                        
*                                                                               
         LLH   R1,CJFTRKS          NUMBER OF TRACKS PER PART2 CI                
         ST    R1,RI2TPC                                                        
         SLL   R1,16                                                            
         ST    R1,RI2TPT                                                        
*                                                                               
         LLH   R1,CIFFDTRK         PART1 STARTING TRACK                         
         SLL   R1,16                                                            
         OILL  GR1,X'0100'         R1                                           
         ST    R1,RIFTRK1                                                       
*                                                                               
         LLH   R1,CJFSTTRK         PART2 STARTING TRACK                         
         SLL   R1,16                                                            
         OILL  GR1,X'0100'         R1                                           
         ST    R1,RIFTRK2                                                       
*                                                                               
         LLH   R1,RIFTRK+2                                                      
         NILL  GR1,X'0FFF'         R1 ISOLATE THE BLOCK/RECORD                  
         MH    R1,CIFHIREC                                                      
         ST    R1,RIHIREC          HIGH BLOCK/RECORD NUMBER                     
*                                                                               
         MVC   RIHIREC2,CJFHIREC   COPY BLOCKS PER PART2 20 BIT                 
*                                                                               
         J     EXITOK                                                           
         DROP  R4                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* ADD PRINT QUEUE FILE INFORMATION TO SHARED QUEUE                              
***********************************************************************         
ADDFINF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,FIWNDA           R4=A(PART1 NODE)                             
         USING SI1PARD,R4                                                       
*                                                                               
         L     R2,ACIREC                                                        
         USING PQRECD,R2                                                        
         MVC   SVINDEX(L'PQINDEX),PQINDEX                                       
*                                                                               
         CLI   PQSTAT,0            IGNORE EMPTY/PURGED ENTRIES                  
         JE    EXITH               NO NEED TO ADD TO TREE                       
*                                                                               
         MVC   SI1NDX,PQINDEX      SET PART1 INDEX IN MEMORY                    
*                                                                               
         OC    PQCINEXT,PQCINEXT   IS THERE A NEXT CI                           
         JZ    EXITOK              NO: FINISH UP                                
*                                                                               
         LHI   R1,X'0100'                                                       
         ICM   R1,B'1100',PQCINEXT PICK UP NEXT CI IN CHAIN                     
         CL    R1,SIT2STT          CHECK PART2 HAS VALID DISK ADDRESS           
         JL    BADNXT              NO: CHOP IT OFF                              
         ST    R1,FIWCIA                                                        
*                                                                               
         GOTO1 =V(READCI)                                                       
         MVC   THISCI,FIWCIA       THIS CI UNDER INSPECTION                     
*                                                                               
         CLC   PQREPNO,SVINDEX+(PQREPNO-PQINDEX) FILE # MISMTCH, CHOP           
         JNE   BADFNO              INDEX KEY MISMATCH, CHOP IT                  
         CLC   PQKEY,SVINDEX       CHECK PART2 KEY IS SAME AS PART1 KEY         
         JNE   BADNDX              INDEX KEY MISMATCH, CHOP IT                  
*                                                                               
         BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE)                     
         L     R1,FIWNDA                                                        
         S     R1,FIWSHA           DISPLACEMENT FROM BEGINNING OF TABLE         
         ST    R1,SI1NXT           NEXT PART2                                   
*                                                                               
         L     R4,FIWNDA           R4=A(PART2 NODE)                             
         ST    R4,ACURPAR2         SAVE A(CURRENT PART2 NODE)                   
         USING SI2PARD,R4                                                       
         MVC   SI2NDX,PQINDEX      SET PART2 INDEX IN MEMORY                    
*                                                                               
         L     R1,ACURPAR1         CURRENT PART1                                
         S     R1,FIWSHA                                                        
         ST    R1,SI2PRV           FIRST PREVIOUS IS THE PART1                  
         B     AFPQ060                                                          
*                                                                               
AFPQ040  GOTO1 =V(READCI)                                                       
         MVC   THISCI,FIWCIA       THIS CI UNDER INSPECTION                     
*                                                                               
         L     R4,FIWNDA                                                        
         ST    R4,ACURPAR2         SAVE CURRENT PART2                           
*                                                                               
         CLC   PQREPNO,SVINDEX+(PQREPNO-PQINDEX)                                
         JNE   BADFNO              FILE # MISMTCH, CHOP IT                      
         CLC   PQKEY,SVINDEX       CHECK PART2 KEY IS SAME AS PART1 KEY         
         JNE   BADNDX              INDEX KEY MISMATCH, CHOP IT                  
*                                                                               
         MVC   SI2NDX,PQINDEX      SET PART2 INDEX IN MEMORY                    
*                                                                               
         MVC   FIWCIA,GOODCI                                                    
         BRAS  RE,FIRCN                                                         
         L     R1,FIWNDA                                                        
         S     R1,FIWSHA           DISPLACEMENT FROM BEGINNING OF TABLE         
         ST    R1,SI2PRV           DISPLACEMENT TO PREVIOUS PART2               
*                                                                               
AFPQ060  OC    PQCINEXT,PQCINEXT   IS THERE A NEXT CI?                          
         JZ    EXITOK              NO: FINISH UP                                
*                                                                               
         LHI   R1,X'0100'                                                       
         ICM   R1,B'1100',PQCINEXT PICK UP NEXT CI IN CHAIN                     
*                                                                               
         CL    R1,SIT2STT          VALID PART2 WITHIN RANGE?                    
         BL    BADNXT              NO: CHOP IT OFF                              
         ST    R1,FIWCIA                                                        
*                                                                               
         BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE)                     
         L     R1,FIWNDA                                                        
         S     R1,FIWSHA           DISPLACEMENT FROM BEGINNING OF TABLE         
         ST    R1,SI2NXT           DIPLACEMENT TO NEXT PART2                    
*                                                                               
         MVC   GOODCI,THISCI       CI IS ALL GOOD                               
         B     AFPQ040                                                          
         DROP  R2,R4                                                            
                                                                                
*----------------------------------------------------------------------         
* BAD FILE INFORMATION - ATTEMPT TO REPAIR                                      
*----------------------------------------------------------------------         
BADNDX   MVC   PLMSG,=CL20'*INDEX MISMATCH*'                                    
         J     BAD010                                                           
BADFNO   MVC   PLMSG,=CL20'*FILE # MISMATCH*'                                   
         J     BAD010                                                           
BADPRE   MVC   PLMSG,=CL20'*BAD PREV CI*'                                       
         J     BAD010                                                           
BADNXT   MVC   PLMSG,=CL20'*BAD NEXT CI*'                                       
         J     BAD020                                                           
*                                                                               
BAD010   MVC   APINDEX,0(R2)       BAD INDEX                                    
         BRAS  RE,PRINTNDX         PRINT BAD INDEX                              
*                                                                               
         MVC   FIWCIA,GOODCI       READ LAST GOOD CI                            
         GOTO1 =V(READCI)                                                       
*                                                                               
BAD020   MVC   APINDEX,0(R2)       INDEX OF LAST GOOD CI                        
                                                                                
         LLC   R1,RIDNEXT          DISPLACEMENT TO CI NEXT                      
         LA    R1,0(R1,R2)                                                      
         LLC   RF,RILNEXT          LENGTH OF CI NEXT                            
         AHI   RF,-1                                                            
         EXRL  RF,BAD022                                                        
         J     BAD024                                                           
BAD022   XC    0(0,R1),0(R1)       CHOP IT OFF                                  
*                                                                               
BAD024   BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE)                     
         L     R4,FIWNDA                                                        
*                                                                               
         BRAS  RE,FIRC1            IS THIS A PART1                              
         JNE   BAD030              NO                                           
                                                                                
         LLC   R1,RIDSEQ           DISPLACEMENT TO REPORT SEQUENCE #            
         LA    R1,0(R1,R2)                                                      
         MVI   0(R1),0             SET SEQUENCE TO INDICATE NO PART2S           
*                                                                               
         USING SI1PARD,R4                                                       
         XC    SI1NXT,SI1NXT       REMOVE LINK IN INDEX                         
         LLC   R1,RIDSEQ                                                        
         LA    R1,SI1NDX(R1)                                                    
         MVI   0(R1),0             SET SEQUENCE # IN INDEX                      
         J     BAD040                                                           
*                                                                               
         USING SI2PARD,R4                                                       
BAD030   XC    SI2NXT,SI2NXT       REMOVE LINK IN INDEX                         
*                                                                               
BAD040   GOTO1 =V(WRITECI)         WRITE BACK THE FIX                           
*                                                                               
         BRAS  RE,PRINTNDX         PRINT LAST GOOD INDEX                        
*                                                                               
         LA    R3,MSGW                                                          
         MVC   MSGWMSG,=CL40'*ERROR* REPORT FORMAT ERROR'                       
         MVC   MSGWDET(5),=C'FILE='                                             
         MVC   MSGWDET+5(L'FIWRES),FIWRES                                       
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     EXITOK                                                           
         DROP  R4                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* PRINT QUEUE - SCAN THROUGH THE INDEX                                          
***********************************************************************         
         USING SI1PARD,R2                                                       
         USING PQRECD,R3                                                        
         USING RVTABD,R5                                                        
FILSCAN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DAREWMSG,0                                                       
         XC    DATEM18H,DATEM18H                                                
         XC    TIMEM18H,TIMEM18H                                                
*                                                                               
GSPQ020  ST    R2,FIWNDA           CURRENT PART1                                
         LA    R3,SI1NDX           R3=A(INDEX)                                  
         BRAS  RE,FIRNC            CONVERT A(INDEX NODE) TO A(CI) REF#          
*                                                                               
         LR    R1,R2                                                            
         S     R1,FIWSHA           DISPL TO CURRENT PART1 NODE                  
         C     R1,SITP1HD          IS THIS NODE NEXT AVAILABLE?                 
         BE    GSPQ500             YES: LEAVE IT                                
         OC    SI1NAV,SI1NAV       IS IT ALREADY AVAILABLE?                     
         BNZ   GSPQ500             YES: LEAVE IT                                
         CLI   PQSTAT,PQSTPU       ALREADY PURGED?                              
         BE    GSPQ500             YES: SKIP IT                                 
*                                                                               
         TM    PQSTAT,PQSTTE       TEMP - CREATION / PRINTING                   
         BZ    GSPQ022             NO: MOVE ON                                  
         LA    RF,YESTRDAY                                                      
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,YESTRO                                                        
         CLC   PQAGELD,0(RF)       CREATED BEFORE YESTERDAY?                    
         BH    GSPQ500             NO: AFTER, THEN LEAVE IT                     
         BL    GSPQ022             YES: CONTINUE                                
         CLC   PQAGELT,TIMENOWC    CREATED BEFORE THIS TIME YESTERDAY?          
         BNL   GSPQ500             NO: THEN LEAVE IT                            
*                                                                               
GSPQ022  LA    RF,PERMNEW          DEC31/2059 NEW COMPRSD DATE/TIME             
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,PERMOLD          DEC31/2027 OLD COMPRSD DATE/TIME             
         CLC   PQAGERD(2),0(RF)    IS THIS A PERMANENT REPORT?                  
         BE    GSPQ500             YES: LEAVE IT                                
*                                                                               
*        TEMP UNTIL 2027                                                        
*                                                                               
CHKDATE  XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RE,3,TODAYO         TODAY IN NCD DATE                            
         SRL   RE,9                SET RE TO YEAR ONLY                          
         CHI   RE,127              2027                                         
         JNL   CHK020              IF 2027+ DON'T CHECK                         
*                                                                               
         TM    PQSTAT,PQSTTE       IGNORE TEMP ENTRIES                          
         JO    CHK020                                                           
         TM    PQTYP1,PQTYNCD      NEW COMP DATE                                
         JZ    CHK010                                                           
*                                                                               
         ICM   RE,3,TODAY          TODAY IN NCD DATE                            
         SRL   RE,9                SET RE TO YEAR ONLY                          
         ICM   RF,3,PQAGELD                                                     
         SRL   RF,9                                                             
         SR    RE,RF               HOW MANY YEARS OLD                           
         JM    CHKERR              NEGATIVE MUST BE WRONG                       
         CHI   RE,40               >40 MUST BE WRONG                            
         JH    CHKERR                                                           
*                                                                               
         ICM   RE,3,TODAY          TODAY IN NCD DATE                            
         SRL   RE,9                SET RE TO YEAR ONLY                          
         ICM   RF,3,PQAGERD                                                     
         CLM   RF,3,=X'FF9F'       PERMANENT RETAIN IS OK                       
         JNL   CHK020                                                           
         SRL   RF,9                                                             
         SR    RF,RE               HOW MANY YEARS TO EXPIRY                     
         CHI   RF,-40                                                           
         JL    CHKERR              NEGATIVE MUST BE WRONG                       
         CHI   RF,40               >40 MUST BE WRONG                            
         JH    CHKERR                                                           
         J     CHK020                                                           
*                                                                               
CHK010   ICM   RE,3,TODAYO         TODAY IN OLD DATE                            
         SRL   RE,9                SET RE TO YEAR ONLY                          
         ICM   RF,3,PQAGELD                                                     
         SRL   RF,9                                                             
         SR    RE,RF               HOW MANY YEARS OLD                           
         JM    CHKERR              NEGATIVE MUST BE WRONG                       
         CHI   RE,40               >40 MUST BE WRONG                            
         JH    CHKERR                                                           
*                                                                               
         ICM   RE,3,TODAYO         TODAY IN NCD DATE                            
         SRL   RE,9                SET RE TO YEAR ONLY                          
         ICM   RF,3,PQAGERD                                                     
         CLM   RF,3,=X'FF9F'       PERMANENT RETAIN IS OK                       
         JNL   CHK020                                                           
         SRL   RF,9                                                             
         SR    RF,RE               HOW MANY YEARS TO EXPIRY                     
         CHI   RF,-40                                                           
         JL    CHKERR              NEGATIVE MUST BE WRONG                       
         CHI   RF,40               >40 MUST BE WRONG                            
         JH    CHKERR                                                           
         J     CHK020                                                           
*                                                                               
         OC    PQAGEDD,PQAGEDD     ANY PRTD/SENT DATE                           
         JZ    CHK020                                                           
         ICM   RE,3,TODAYO         TODAY IN NCD DATE                            
         SRL   RE,9                SET RE TO YEAR ONLY                          
         ICM   RF,3,PQAGEDD                                                     
         SRL   RF,9                                                             
         SR    RF,RE               HOW MANY YEARS OLD                           
         JM    CHKERR              NEGATIVE MUST BE WRONG                       
         CHI   RF,40               >40 MUST BE WRONG                            
         JH    CHKERR                                                           
         J     CHK020                                                           
*                                                                               
CHKERR   MVC   PLMSG,=CL20'*NCD DATE ERROR '                                    
         MVC   APINDEX,0(R3)       BAD INDEX                                    
         BRAS  RE,PRINTNDX         PRINT BAD INDEX                              
*                                                                               
CHK020   CLI   SETCDATW,C'N'       DO WE WANT ALL NCD                           
         JNE   CHK030                                                           
         OI    SIHFIND,SIHFNCD     USING NEW CMPRSD DATE                        
         TM    PQTYP1,PQTYNCD      OK IF ALREADY NCD                            
         JO    CHK030                                                           
         J     GSPQ200             DO STATUS CHANGE                             
*                                                                               
CHK030   CLI   SETCDATW,C'O'       DO WE WANT ALL OCD                           
         JNE   CHK040                                                           
         NI    SIHFIND,X'FF'-SIHFNCD RESOURCE USING OLD CMPRSD DATE             
         TM    PQTYP1,PQTYNCD      OK IF ALREADY OCD                            
         JNO   CHK040                                                           
         J     GSPQ200             DO STATUS CHANGE                             
*                                                                               
CHK040   EQU   *                                                                
*                                                                               
*        ENDTEMP UNTIL 2027                                                     
*                                                                               
*&&US                                                                           
*----------------------------------                                             
* LOOK FOR UNPROCESSED DARE REPORTS                                             
*----------------------------------                                             
         OC    WARNDARE,WARNDARE   ANY DARE WARNING HOUR VALUE?                 
         BZ    GSPQ025             NO: DON'T CHECK FOR DARE                     
         BRAS  RE,DARECHK          CALL SPECIAL DARE ROUTINE                    
         BE    GSPQ025             NO WARNING NEEDED, CONTINUE                  
         BL    GSPQ100             CAN'T STAY FOREVER, MAKE VULNERABLE          
         MVC   PLMSG,=CL20'*DARE WARNING*'                                      
         MVC   APINDEX,0(R3)       THE INDEX TO PRINT                           
         BRAS  RE,PRINTNDX                                                      
         B     GSPQ500             IF WARNING ISSUED, KEEP REPORT               
*&&                                                                             
*----------------------------------                                             
* HAS THE REPORT EXPIRED?                                                       
*----------------------------------                                             
GSPQ025  LA    RF,TODAY                                                         
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,TODAYO                                                        
         CLC   PQAGERD,0(RF)       TEST RETAIN DATE WITH TODAY                  
         BH    GSPQ040             NOT EXPIRED                                  
         BL    GSPQ030             EXPIRED                                      
         CLC   PQAGERT,TIMENOWM    EXPIRING TODAY, TEST TIME                    
         BNL   GSPQ040             NOT EXPIRED                                  
         B     GSPQ030             EXPIRED                                      
                                                                                
*----------------------------------------------------------------------         
* EXPIRED REPORTS                                                               
*----------------------------------------------------------------------         
GSPQ030  DS    0H                                                               
*                                                                               
*&&UK*&& TM    PQSTAT,PQSTAC+PQSTKE  ACTIVE AND ON KEEP                         
*&&UK*&& BNO   *+14                                                             
*&&UK*&& CLC   PQSUBID,=C'LU1'     IGNORE AVAILABLE LINE UP REPORTS             
*&&UK*&& BE    GSPQ500             LEAVE THIS ONE                               
*                                                                               
         TM    PQSTAT,PQSTTE       TEMP - CREATION / PRINTING                   
         BZ    GSPQ031             NO: MOVE ON                                  
         CLC   PQAGERD,=XL2'0000'  IS THERE AN EXPIRATION DATE?                 
         BE    GSPQ500             NO: LEAVE IT                                 
         LA    RF,YESTRDAY                                                      
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,YESTRO                                                        
         CLC   PQAGERD,0(RF)       DID IT EXPIRE BEFORE YESTERDAY?              
         BL    GSPQ036             YES: MAYBE PURGE                             
         B     GSPQ500             NO: LEAVE IT ALONE                           
*                                                                               
GSPQ031  TM    PQTYP1,PQTYBKU      HAS THIS BEEN BACKED UP                      
         BO    GSPQ032                                                          
         CLI   BACKUPW,C'N'        IS THERE NO BACKUP JOB                       
         BE    GSPQ032                                                          
         TM    MINDI,MIFREE1+MIFREE2   DO WE NEED TO FREE SPACE                 
         BZ    GSPQ500             NO: LEAVE IT ALONE                           
*                                                                               
GSPQ032  TM    PQSTAT,PQSTKE       ON KEEP                                      
         BZ    GSPQ034             NO: CHECK TEMP                               
         LA    RF,TWOWEEKS                                                      
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,TODM2WO                                                       
         CLC   PQAGERD,0(RF)       DID IT EXPIRE OVER TWO WEEKS AGO?            
         BL    GSPQ036             YES: THEN POSSIBLY PURGE                     
         B     GSPQ100             NO: VULNERABLE                               
*                                                                               
GSPQ034  TM    PQSTAT,PQSTAC       ACTIVE                                       
         BZ    GSPQ036             NO: MOST LIKELY CAN BE PURGED                
         LA    RF,TODAY                                                         
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,TODAYO                                                        
         CLC   PQAGERD,0(RF)       DID IT EXPIRE BEFORE TODAY?                  
         BL    GSPQ036             YES: POSSIBLY PURGE                          
         B     GSPQ100             NO: VULNERABLE                               
*                                                                               
GSPQ036  DS    0H                                                               
*&&US                                                                           
         L     R1,=V(SSB)                                                       
         CLI   SSODSPAC-SSOOFF(R1),C'R'   IF IT'S REP PRINT QUEUES              
         BE    GSPQ150                    DELETE AS SOON AS THEY EXPIRE         
*&&                                                                             
         LA    RF,YESTRDAY                                                      
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,YESTRO                                                        
         CLC   PQAGELD,0(RF)       WAS IT CREATED BEFORE YESTERDAY?             
         BL    GSPQ150             YES: THEN WE CAN PURGE                       
         BH    GSPQ100             NO: CREATED TODAY, THEN VULNERABLE           
         CLC   PQAGELT,TIMENOWC    CREATED BEFORE THIS TIME YESTERDAY?          
         BNL   GSPQ100             NO: THEN VULNERABLE                          
         B     GSPQ150             YES: EXPIRED 24 HOURS AGO                    
                                                                                
*----------------------------------------------------------------------         
* NON-EXPIRED REPORTS                                                           
*----------------------------------------------------------------------         
GSPQ040  TM    PQSTAT,PQSTTE       TEMP - CREATION / PRINTING                   
         BO    GSPQ500             YES: LEAVE IT                                
         TM    PQSTAT,PQSTKE       IS THIS ON KEEP?                             
         BO    GSPQ500             YES: LEAVE IT ALONE                          
*                                                                               
         TM    PQATTB,PQATERR      ERROR REPORTS ARE VULNERABLE                 
         BO    GSPQ100                                                          
*                                                                               
         TM    PQSTAT,PQSTDEAD     PRINTED / SENT / DEAD?                       
         BZ    GSPQ500             NO: LEAVE IT ALONE                           
         LA    RF,TODAY                                                         
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,TODAYO                                                        
         CLC   PQAGELD,0(RF)       WAS IT CREATED BEFORE TODAY                  
         BL    GSPQ100             YES: THEN VULNERABLE                         
         BH    GSPQ100             CREATED IN THE FUTURE???? SURE...            
         CLC   PQAGELT,TIMEOLDC    WAS IT CREATED BEFORE THE LAST SCAN?         
         BNL   GSPQ500             NO: THEN NOT EVEN VULNERABLE                 
                                                                                
*----------------------------------------------------------------------         
* VULNERABLE REPORTS - SAVE FOR LATER                                           
*----------------------------------------------------------------------         
GSPQ100  CLI   PQSEQ,0             ANY PART2S?                                  
         BE    GSPQ130                                                          
         ICM   R6,15,SI1NXT        ANY PART2S?                                  
         BZ    GSPQ130                                                          
         XC    HALF,HALF                                                        
*                                                                               
         USING SI2PARD,R6                                                       
GSPQ110  A     R6,FIWSHA           GO TO LAST PART2 AND GET SEQUENCE #          
         ICM   R1,15,SI2NXT                                                     
         BZ    GSPQ120                                                          
         LR    R6,R1                                                            
         B     GSPQ110                                                          
*                                                                               
GSPQ120  LLC   R1,SI2NDX+(PQSEQ-PQINDEX)                                        
         TM    PQTYPE,PQTYXTN      DO WE HAVE EXTENSION CIS                     
         BZ    *+8                                                              
         AHI   R1,255              YES: THEN 255 REGULAR PART2S                 
         AHI   R1,-1               DECREMENT COUNT FOR THE PART1                
         STCM  R1,3,HALF           # OF PART2 CIS                               
         A     R1,CNTP2VU          UPDATE VULNERABLE COUNT                      
         ST    R1,CNTP2VU                                                       
         DROP  R6                                                               
*                                                                               
GSPQ130  L     R0,CNTP1VU          COUNT SCANNED VULNERABLE PART1S              
         AHI   R0,1                                                             
         ST    R0,CNTP1VU                                                       
*                                                                               
         CHI   R0,RVMAX            MORE THAN WE CAN HANDLE?                     
         BH    GSPQ500             DON'T ADD IT TO THE TABLE                    
         XC    RVTABD(RVTABL),RVTABD                                            
*                                                                               
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         ICM   R0,3,PQAGELD                                                     
         LA    RF,TODAY                                                         
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,TODAYO                                                        
         ICM   R1,3,0(RF)          LIVE DATE MINUS TODAY                        
         SR    R1,R0               WILL GIVE ME SOME NUMBER                     
         AHI   R1,1                ADD ONE TO THAT TO AVOID ZERO                
         STCM  R1,3,RVONE          WHATEVER THAT IS, SAVE IT                    
         MVC   RVTWO,HALF          SAVE # OF PART2 CIS                          
*                                                                               
         TM    MINDI,MIFREE2       DO WE ABSOLUTELY NEED PART2S?                
         BZ    GSPQ140             NO                                           
         MVC   RVONE,HALF          YES: SORT WITH SIZE FIRST                    
         STCM  R1,3,RVTWO                                                       
*                                                                               
GSPQ140  MVC   RVNDX,PQINDEX       FILE INDEX                                   
*                                                                               
         L     RF,ARVTAB           INCREMENT VULNERABLE TABLE COUNT             
         XR    R1,R1                                                            
         ICM   R1,3,0(RF)                                                       
         AHI   R1,1                                                             
         STCM  R1,3,0(RF)                                                       
*                                                                               
         LA    R5,RVTABL(,R5)                                                   
         B     GSPQ500             NEXT                                         
                                                                                
*----------------------------------------------------------------------         
* PURGE THE REPORT                                                              
*----------------------------------------------------------------------         
GSPQ150  BRAS  RE,FIRRLOCK         LOCK THE REPORT                              
         MVC   SVREF,FIWREF        SAVE THE REFERENCE FOR THE UNLOCK            
*                                                                               
         MVC   SVINDEX,PQINDEX                                                  
         MVC   UKINDEX,PQINDEX                                                  
         OI    UKFLAG,UKFLNUM      PASSING REF#                                 
*                                                                               
         SAM24                                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'INDEX',FIWRES,UKINDEX,ACTREC,ACIREC          
         CLI   DMCB+8,0                                                         
         JE    GSPQ152                                                          
         SAM31                                                                  
         MVC   FIWREF,SVREF        REPORT #                                     
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         BRAS  RE,FIRRC            REPORT TO A(CI)                              
         MVC   FIWNDX,UKINDEX      INDEX                                        
         GOTO1 =V(BADREP),2        OUTPUT BAD REPORT                            
         B     GSPQ500                                                          
*                                                                               
GSPQ152  SAM31                                                                  
         CLC   UKINDEX,SVINDEX     MAKE SURE IT'S THE SAME REPORT               
         BE    GSPQ160             NO: LEAVE IT ALONE                           
         MVC   FIWREF,SVREF        RESTORE REFERENCE NUMBER AND UNLOCK          
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         B     GSPQ500                                                          
*                                                                               
GSPQ160  CLI   WRTFIL,C'Y'                                                      
         BE    GSPQ170                                                          
         MVC   PLMSG,=CL20'*PURGE (WRITE=NO)*'                                  
         MVC   FIWREF,SVREF        RESTORE REFERENCE NUMBER AND                 
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         B     GSPQ180                                                          
*                                                                               
GSPQ170  SAM24                                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'PURGE',FIWRES,UKINDEX,ACTREC,ACIREC          
         CLI   DMCB+8,0                                                         
         JE    GSPQ175             PURGE WILL UNLOCK THE REPORT                 
         SAM31                                                                  
         MVC   FIWREF,SVREF        REPORT #                                     
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         BRAS  RE,FIRRC            REPORT TO A(CI)                              
         MVC   FIWNDX,UKINDEX      INDEX                                        
         GOTO1 =V(BADREP),3        OUTPUT BAD REPORT                            
         B     GSPQ500                                                          
*                                                                               
GSPQ175  SAM31                                                                  
         MVC   PLMSG,=CL20'*REPORT PURGE*'                                      
*                                                                               
GSPQ180  OI    SINDI,SIPURGE                                                    
         BRAS  RE,PRINTNDX                                                      
         J     GSPQ500                                                          
*                                                                               
*---------------------------------------------------------------------          
* CHANGE DATE FORMAT OF REPORT                                                  
*----------------------------------------------------------------------         
GSPQ200  EQU   *                                                                
*                                                                               
*NOP     BRAS  RE,FIRRLOCK         LOCK THE REPORT  ??                          
*NOP     MVC   SVREF,FIWREF        SAVE THE REFERENCE FOR THE UNLOCK            
*                                                                               
         MVC   SVINDEX,PQINDEX                                                  
         MVC   UKINDEX,PQINDEX                                                  
         OI    UKFLAG,UKFLNUM      PASSING REF#                                 
*                                                                               
         SAM24                                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'INDEX',FIWRES,UKINDEX,ACTREC,ACIREC          
         CLI   DMCB+8,0                                                         
         JE    GSPQ252                                                          
         SAM31                                                                  
         MVC   FIWREF,SVREF        REPORT #                                     
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         BRAS  RE,FIRRC            REPORT TO A(CI)                              
         MVC   FIWNDX,UKINDEX      INDEX                                        
         GOTO1 =V(BADREP),2        OUTPUT BAD REPORT                            
         B     GSPQ500                                                          
                                                                                
GSPQ252  SAM31                                                                  
         CLC   UKINDEX,SVINDEX     MAKE SURE IT'S THE SAME REPORT               
         BE    GSPQ260             . NO, LEAVE IT ALONE                         
         MVC   FIWREF,SVREF        RESTORE REFERENCE NUMBER AND UNLOCK          
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         B     GSPQ500                                                          
*                                                                               
GSPQ260  CLI   WRTFIL,C'Y'                                                      
         BE    GSPQ270                                                          
         MVC   PLMSG,=CL20'OCDATE (WRITE=NO)*'                                  
         CLI   SETCDATW,C'N'                                                    
         JNE   *+10                                                             
         MVC   PLMSG,=CL20'NCDATE (WRITE=NO)*'                                  
         MVC   FIWREF,SVREF        RESTORE REFERENCE NUMBER AND                 
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         B     GSPQ280                                                          
*                                                                               
GSPQ270  SAM24                                                                  
         LA    RF,=C'OCD '                                                      
         CLI   SETCDATW,C'N'                                                    
         JNE   *+8                                                              
         LA    RF,=C'NCD '                                                      
         GOTO1 =V(DATAMGR),DMCB,(RF),FIWRES,UKINDEX,ACTREC,ACIREC               
         CLI   DMCB+8,0                                                         
         JE    GSPQ275             STATUS CHANGE WILL UNLOCK THE REPORT         
         SAM31                                                                  
         MVC   FIWREF,SVREF        REPORT #                                     
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         BRAS  RE,FIRRC            REPORT TO A(CI)                              
         MVC   FIWNDX,UKINDEX      INDEX                                        
         GOTO1 =V(BADREP),3        OUTPUT BAD REPORT                            
         B     GSPQ500                                                          
                                                                                
GSPQ275  SAM31                                                                  
         MVC   PLMSG,=CL20'*REPORT OCDATE*'                                     
         CLI   SETCDATW,C'N'                                                    
         JNE   *+10                                                             
         MVC   PLMSG,=CL20'*REPORT NCDATE*'                                     
*                                                                               
GSPQ280  BRAS  RE,PRINTNDX                                                      
         J     GSPQ500                                                          
*                                                                               
GSPQ500  LA    R2,L'SI1PAR(,R2)    GET NEXT INDEX                               
         BCT   R4,GSPQ020                                                       
                                                                                
*----------------------------------------------------------------------         
* FINISHED SCAN                                                                 
*----------------------------------------------------------------------         
         BRAS  RE,WARNMSG          SEND WARNING MESSAGE IF NECESSARY            
         BRAS  RE,PUVU             PURGE VULNERABLES IF NECESSARY               
*                                                                               
GSPQX    J     EXITOK                                                           
         DROP  R2,R3,R5                                                         
*                                                                               
PERMNEW  DC    X'BF9F8F'           DEC31/2059 NEW COMPRSD DATE/TIME             
PERMOLD  DC    X'FF9F8F'           DEC31/2027 OLD COMPRSD DATE/TIME             
         LTORG                                                                  
*&&US                                                                           
***********************************************************************         
* LOOK AT DARE PRINT QUEUE REPORTS AND SEND WARNING IF NECESSARY                
***********************************************************************         
DARECHK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SI1PARD,R2                                                       
         USING PQRECD,R3                                                        
         USING RVTABD,R5                                                        
*                                                                               
         TM    PQSTAT,PQSTAC       IS THIS REPORT ACTIVE?                       
         BZ    DCHKOKX             NO: THEN DON'T CARE ABOUT IT                 
*                                                                               
         CLC   PQSUBID,=C'DAR'     SUBID=DAR                                    
         BE    DCHK010                                                          
         CLC   PQSUBID,=C'DCF'     SUBID=DCF                                    
         BE    DCHK010                                                          
         CLC   PQSUBID,=C'DMG'     SUBID=DMG                                    
         BNE   DCHKOKX                                                          
*                                                                               
DCHK010  CLI   PQCLASS,C'N'        CLASS=N                                      
         BE    DCHK020                                                          
         CLI   PQCLASS,C'G'        CLASS=G                                      
         BNE   DCHKOKX                                                          
*                                                                               
DCHK020  LA    RF,YESTRDAY                                                      
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,YESTRO                                                        
         CLC   PQAGELD,0(RF)       CREATED BEFORE YESTERDAY?                    
         BL    DCHK050             YES: SEND WARNING                            
*                                                                               
         OC    TIMEM18H,TIMEM18H   ALREADY CALCULATED THE DARE LIMIT?           
         BNZ   DCHK030             YES: CHECK DATE AND TIME                     
         MVC   DATEM18H,TODAY      START WITH TODAY                             
         MVC   TODM18HO,TODAY                                                   
         OI    TODM18HO,X'80'      SET OLD COMPRSSED DATE TO TODAY              
         XR    R0,R0                                                            
         L     R1,TIMENOW          AND CURRENT TIME                             
         D     R0,=F'100'                                                       
         LH    RF,WARNDARE         NUMBER OF HOURS FOR DARE WARNING             
         MHI   RF,3600             TIMES ONE HOUR IN SECONDS                    
         CR    R1,RF               ARE WE AT WHATEVER O'CLOCK?                  
         BNH   DCHK022             NOT YET, GO BACK TO YESTERDAY                
         SR    R1,RF               SUBTRACT THE HOURS                           
         B     DCHK024                                                          
DCHK022  SR    RF,R1               MINUS HOURS TO GET TO YESTERDAY              
         LHI   R1,24               24 HOURS                                     
         MHI   R1,3600             X 1 HOUR IN SECONDS TO GET SECONDS           
         SR    R1,RF               MINUS REMAINING FOR TIME YESTERDAY           
         MVC   DATEM18H,YESTRDAY   SET DATE TO YESTERDAY                        
         MVC   TODM18HO,YESTRDAY                                                
         OI    TODM18HO,X'80'      SET OLD COMPRSSED DATE TO YESTERDAY          
DCHK024  MHI   R1,3                                                             
         SRL   R1,2                R1=(SECS*3)/4                                
         STCM  R1,3,TIMEM18H       SET TIME TO X HOURS AGO                      
*                                                                               
DCHK030  LA    RF,DATEM18H                                                      
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,TODM18HO                                                      
         CLC   PQAGELD,0(RF)       CREATED MORE THAN X HOURS AGO?               
         BL    DCHK050             YES: SEND WARNING                            
         BNE   DCHKOKX                                                          
         CLC   PQAGELT,TIMEM18H    CREATED MORE THAN X HOURS AGO?               
         BL    DCHK050             YES: SEND WARNING                            
         B     DCHKOKX                                                          
*                                                                               
DCHK050  MVI   BIGWORK,C' '        BUILD PQ REPORT DETAIL FOR MESSAGE           
         MVC   BIGWORK+1(L'BIGWORK-1),BIGWORK                                   
         MVC   BIGWORK(3),PQSUBID                                               
         MVI   BIGWORK+3,C','                                                   
         EDIT  (B4,FIWREF),(5,BIGWORK+4),ZERO=NOBLANK,FILL=0                    
         MVC   HALF,PQAGELD                                                     
         GOTO1 =V(DATCON),DMCB,(14,HALF),(21,BIGWORK+11)                        
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF TO NEW CMPRSD DATE                  
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         ICM   R1,3,PQAGELT                                                     
         MHI   R1,4                                                             
         D     R0,=F'3'            3/4 SECONDS TO SECONDS                       
         XR    R0,R0                                                            
         D     R0,=F'60'           SECONDS TO MINUTES                           
         XR    R0,R0                                                            
         D     R0,=F'60'           MINUTES TO HOURS                             
         XC    FULL,FULL                                                        
         STC   R1,FULL+1           HOURS                                        
         STC   R0,FULL+3           MINUTES                                      
         EDIT  (B2,FULL),(2,BIGWORK+23),ZERO=NOBLANK,FILL=0                     
         MVI   BIGWORK+25,C':'                                                  
         EDIT  (B2,FULL+2),(2,BIGWORK+26),ZERO=NOBLANK,FILL=0                   
*                                                                               
         CLI   DAREWMSG,DAREWMAX   ALREADY WARNED ENOUGH?                       
         BH    DCHK080             YES                                          
         BL    DCHK070             NO                                           
         MVC   BIGWORK+11(17),=C'THERE MAY BE MORE'                             
DCHK070  GOTO1 =V(NOTYMSG),NTYDARE SEND NOTIFICATION MESSAGE                    
DCHK080  LLC   R1,DAREWMSG                                                      
         AHI   R1,1                                                             
         STC   R1,DAREWMSG         KEEP COUNT OF THE NUMBER OF WARNINGS         
         B     DCHKHIX                                                          
*                                                                               
DCHKOKX  J     EXITOK                                                           
DCHKHIX  J     EXITH                                                            
DCHKLOX  J     EXITL                                                            
         DROP  R2,R3,R5                                                         
         LTORG                                                                  
*&&                                                                             
***********************************************************************         
* CHECK SIZE AND SEND WARNING MESSAGES                                          
***********************************************************************         
WARNMSG  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 =V(PACALC)          CALCULATE % AVAILABLE                        
*                                                                               
         LH    R0,WARNPQ1          WARN AT THIS PERCENT AVAILABLE(5%)           
         MHI   R0,100                                                           
         L     R1,PAPART1          # OF AVAILABLE PART1S                        
         CR    R1,R0               COMPARE PERCENTAGE TO THRESHOLD              
         BL    WMSG010                                                          
         LH    R0,WARNPQ2          WARN AT THIS PERCENT AVAILABLE(15%)          
         MHI   R0,100                                                           
         L     R1,PAPART2          # OF AVAILABLE PART2S                        
         CR    R1,R0               COMPARE PERCENTAGE TO THRESHOLD              
         BL    WMSG020                                                          
         B     WMSGX                                                            
*                                                                               
WMSG010  GOTO1 =V(NOTYMSG),1                                                    
         B     WMSGX                                                            
WMSG020  GOTO1 =V(NOTYMSG),2                                                    
WMSGX    J     EXITOK                                                           
         LTORG                                                                  
                                                                                
***********************************************************************         
* PURGE VULNERABLE REPORTS IF NECESSARY                                         
***********************************************************************         
PUVU     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ARVTAB           VULNERABLE REPORT TABLE                      
         XR    R4,R4                                                            
         ICM   R4,3,0(R5)          ANY VULNERABLES FOR THIS RESOURCE?           
         BZ    PUVUX               NO                                           
*                                                                               
         LA    R5,2(,R5)                                                        
         USING RVTABD,R5                                                        
*                                  SORT IN DESCENDING ORDER                     
         SAM24                                                                  
         GOTO1 =V(QSORT),DMCB,(1,(R5)),(R4),RVTABL,5,0,0                        
         SAM31                                                                  
*                                                                               
PUVU010  GOTO1 =V(PACALC)          CALCULATE % AVAILABLE                        
*                                                                               
         LH    R0,PUVUPQ1          PURGE TO THIS PERCENT AVAILABLE(10%)         
         MHI   R0,100                                                           
         L     R1,PAPART1          # OF AVAILABLE PART1S                        
         CR    R1,R0               COMPARE PERCENTAGE TO THRESHOLD              
         BL    PUVU020             YES: PURGE VULNERABLE                        
         LH    R0,PUVUPQ2          PURGE TO THIS PERCENT AVAILABLE(20%)         
         MHI   R0,100                                                           
         L     R1,PAPART2          # OF AVAILABLE PART2S                        
         CR    R1,R0               COMPARE PERCENTAGE TO THRESHOLD              
         BL    PUVU020             YES: PURGE VULNERABLE                        
         B     PUVUX                                                            
                                                                                
*----------------------------------------------------------------------         
* PURGE REPORTS                                                                 
*----------------------------------------------------------------------         
PUVU020  XC    FIWREF,FIWREF                                                    
         MVC   FIWREF+2(2),RVNDX+(PQREPNO-PQINDEX)                              
         OC    FIWREF,FIWREF                                                    
         BZ    PUVU080                                                          
*                                                                               
         BRAS  RE,FIRRLOCK         LOCK THE REPORT                              
         MVC   SVREF,FIWREF        SAVE THE REFERENCE NUMBER FOR UNLOCK         
*                                                                               
         MVC   UKINDEX,RVNDX                                                    
         OI    UKFLAG,UKFLNUM      PASSING REF#                                 
*                                                                               
         SAM24                                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'INDEX',FIWRES,UKINDEX,ACTREC,ACIREC          
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
         SAM31                                                                  
*                                                                               
         CLC   UKINDEX,RVNDX       MAKE SURE SAME INDEX                         
         BE    PUVU040             YES: PURGE                                   
         MVC   FIWREF,SVREF        NO: RESTORE REFERENCE NUMBER                 
         BRAS  RE,FIRRUNLK         AND UNLOCK THE REPORT                        
         B     PUVU080                                                          
*                                                                               
PUVU040  CLI   WRTFIL,C'Y'                                                      
         BE    PUVU050                                                          
         MVC   PLMSG,=CL20'*VLNRBL(WRITE=NO)*'                                  
         MVC   FIWREF,SVREF        RESTORE REFERENCE NUMBER                     
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         B     PUVU060                                                          
*                                                                               
PUVU050  SAM24                                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'PURGE',FIWRES,UKINDEX,ACTREC,ACIREC          
         CLI   DMCB+8,0                                                         
         JNE   *+2                 PURGE WILL UNLOCK THE REPORT                 
         MVC   PLMSG,=CL20'*VULNERABLE PURGE*'                                  
         SAM31                                                                  
*                                                                               
PUVU060  L     R0,CNTP1VU          DECREMENT VULNERABLE COUNT                   
         AHI   R0,-1                                                            
         ST    R0,CNTP1VU                                                       
*                                                                               
         L     R0,CNTP2VU          REDUCE PART2 VULNERABLE COUNT                
         XR    R1,R1                                                            
         ICM   R1,3,RVTWO                                                       
         TM    MINDI,MIFREE2       DO WE ABSOLUTELY NEED PART2S?                
         BZ    *+8                 NO                                           
         ICM   R1,3,RVONE                                                       
         SR    R0,R1                                                            
         ST    R0,CNTP2VU                                                       
*                                                                               
         OI    SINDI,SIPURGE                                                    
         MVC   FIWCIA(2),UKCIADDR                                               
         MVC   FIWCIA+2(2),=XL2'0100'                                           
         BRAS  RE,PRINTNDX                                                      
*                                                                               
PUVU080  LA    R5,RVTABL(,R5)      GET NEXT INDEX FROM VULNERABLE TABLE         
         BCT   R4,PUVU010                                                       
*                                                                               
PUVUX    J     EXITOK                                                           
         DROP  R5                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* PRINT THE INDEX                                                               
***********************************************************************         
PRINTNDX NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SAM24                                                                  
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,UKSRCID                                                  
         GOTO1 =V(DATAMGR),DMCB,=CL7'DMREAD',=CL7'CTFILE',IOKEY,AIOAREA         
         BNE   PXPQ020                                                          
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CTIDATA                                                       
PXPQ010  CLI   0(R3),0                                                          
         BE    PXPQ020                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    PXPQ012                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PXPQ010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
PXPQ012  MVC   PLUSER,CTDSC                                                     
         B     PXPQ022                                                          
         DROP  R2,R3                                                            
*                                                                               
PXPQ020  GOTO1 =V(HEXOUT),DMCB,UKSRCID,PLUSER,L'UKSRCID,0                       
PXPQ022  MVC   PLSUBID,UKSUBID                                                  
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),UKREPNO                                                
         EDIT  (B4,FULL),(5,PLREPNO)                                            
         MVC   PLPCLASS,UKCLASS                                                 
         GOTO1 =V(HEXOUT),DMCB,UKTYPE,PLTYPE,L'UKTYPE,0                         
         GOTO1 =V(HEXOUT),DMCB,UKATTB,PLATTB,L'UKATTB,0                         
         GOTO1 =V(HEXOUT),DMCB,UKSTAT,PLSTAT,L'UKSTAT,0                         
         EDIT  (B1,UKSEQ),(3,PLSEQ)                                             
         MVC   HALF,UKAGELD                                                     
         GOTO1 =V(DATCON),DMCB,(14,HALF),(21,PLCDTE)                            
         ORG   *-2                                                              
         TM    UKTYP1,UKTYNCD      SET HALF TO NEW CMPRSD DATE                  
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
         MVC   HALF,UKAGERD                                                     
         GOTO1 =V(DATCON),DMCB,(14,HALF),(21,PLAGE)                             
         ORG   *-2                                                              
         TM    UKTYP1,UKTYNCD      SET HALF TO NEW CMPRSD DATE                  
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         XR    R0,R0               RETAIN TIME                                  
         LLC   R1,UKAGERT                                                       
         MHI   R1,10               CONVERT 10 MIN INCREMENTS                    
         D     R0,=F'60'                                                        
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         GOTO1 =V(TIMEOUT)                                                      
         MVC   PLAGET,DUB+2                                                     
*                                                                               
         MVC   FULL,FIWCIA                                                      
         GOTO1 =V(HEXOUT),DMCB,FULL,PLCIA,L'FIWCIA,0                            
*                                                                               
         GOTO1 =V(PRINTL)                                                       
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITL    LHI   RF,0                                                             
         J     EXITCC                                                           
EXITH    LHI   RF,2                                                             
         J     EXITCC                                                           
EXITOK   LHI   RF,1                                                             
EXITCC   CHI   RF,1                                                             
EXIT     XIT1  ,                                                                
                                                                                
***********************************************************************         
* SHARED MEMORY FILE INDEX ROUTINES                                             
***********************************************************************         
         DROP  R8,R9               DROP SIHDRD AND SITABD                       
       ++INCLUDE DDSHFIR                                                        
         LTORG                                                                  
                                                                                
***********************************************************************         
* WORKING STORAGE DC                                                            
***********************************************************************         
         DS    0D                                                               
WORKAREA DC    60000D'00'                                                       
                                                                                
***********************************************************************         
* PRINT QUEUE DSECTS                                                            
***********************************************************************         
       ++INCLUDE DMPRTQD                                                        
       ++INCLUDE DMPRTQK                                                        
       ++INCLUDE DMPRTQF                                                        
                                                                                
***********************************************************************         
* WORKING STORAGE AND SHARED DSECTS                                             
***********************************************************************         
       ++INCLUDE DDSHFIWRK                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PQSHFIMON 11/12/20'                                      
         END                                                                    
