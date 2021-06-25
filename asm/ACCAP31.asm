*          DATA SET ACCAP31    AT LEVEL 021 AS OF 08/21/20                      
*PHASE T61D31A                                                                  
*&&ONLIN SET   Y                                                                
         TITLE 'TIMESHEET ROUTINES - #1'                                        
*                                                                               
* LEVEL CHANGE COMMENTS                                                         
* ---------------------                                                         
* UK LEVELS                                                                     
* ---------                                                                     
* ID   LVL DATE   DESCRIPTION                                                   
* ---- --- ------ -----------------------------------------------------         
* CPAT 020 06APR18 <SPEC-20250> FIXED COST DUMP WHILE ENTERING TIME             
*                               ADJUSTMENTS                                     
* JSHA 021 190820 <DSRD-27176> CHANGE NOT ALLOWED IF TIMEDNAR EXIST             
***********************************************************************         
*                                                                               
T61D31   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
         SPACE 1                                                                
ROUT     NMOD1 0,**ROU1**,RA,R7,CLEAR=YES,RR=R2                                 
         USING TIMEGWSD,R9         R9=A(GLOBAL WORKING STORAGE)                 
         L     RC,BCSVRC                                                        
         USING GEND,RC             RC=A(GEND)                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8             R8=A(SYSD)                                   
         L     R3,=A(PPAREA)                                                    
         AR    R3,R2                                                            
         ST    R3,APPAREA                                                       
                                                                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     MTSAR                1 - TSAR MANAGEMENT                         
         B     BLDTSAR              2 - BUILD TSAR TABLE                        
         B     UPDTSAR              3 - UPDATE RECORDS FROM TSAR TABLE          
         B     ADDCLST              4 - ADD CLUSTER TO RECORD IN AIOBC          
         B     ADDEXST              5 - ADD EXISTING ELEMENTS IN AIOBC          
         B     WRITREC              6 - WRITE/ADD RECORD FROM AIOBC             
         B     PASSIVE              7 - BUILD PASSIVE POINTER KEY               
         B     DELETE               8 - DELETE ALL RECS NOT IN 1C TABLE         
         B     POSTBLD             10 - BUILD POSTING BLOCK                     
         B     TSNSORT             11 - SORT TSAR RECORDS BY TS#                
         B     SJSORT              12 - SORT TSAR RECORDS BY SJ CODE            
         B     UPDTSN              13 - UPDATE TIMESHEET #                      
         B     GETTSN              14 - GET TIMESHEET #                         
         B     NEGCHK              15 - CHECK JOBS DONT GO NEGATIVE             
         B     CHKINC              16 - CHECK FOR INCOMPLETE ITEMS              
         B     SAVTSAR             17 - SAVE TSAR SAVED ITEMS                   
         B     RESTSAR             18 - RESTORE TSAR SAVED ITEMS                
         B     DSAVE               19 - DELETE SAVED RECORDS                    
         B     CNTSAVE             20 - COUNT # SAVED ITEMS                     
         B     MRKTSAR             21 - MARK TSAR ITEMS AS SAVED                
         B     TEMPO               22 - UPDATE TEMPO X-REFERENCE REC            
         B     MOAFIND             23 - FIND HIGH/LOW MOA ON RECORD             
         B     DRAFT               24 - READ FOR DRAFT TYPE 34'S                
         B     DELSEQ              25 - DELETE SEQ SUB REF NUMBER RECS          
         B     MRKAST              26 - MARK ASTEL WITH PERIOD ENDDATE          
*                                                                               
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
*                                                                               
ROUTX    XIT1                      EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* TSAR INTERFACE ROUTINE - TSAR USES AIO1                             *         
*                                                                     *         
* NTRY - P1 = TSAR ACTION (DEFINED IN DDTSARD)                        *         
*        P2 = TSAR BUFFER (1 OR 2)                                    *         
*                                                                     *         
* EXIT - CC=EQU - ACTION COMPLETED WITHOUT ERRORS                     *         
*        CC=NEQ - BCTSERRS=TSAR ERROR CODE                            *         
***********************************************************************         
         SPACE 1                                                                
MTSAR    DS    0H                                                               
         STCM  R1,15,BCFULL        SAVE  ORIGINAL PARM LIST                     
         MVC   BCFLAG1,3(R1)       P1=TSAR ACTION                               
         MVC   BCTSINDS,7(R1)      P2=TSAR BUFFER                               
         NI    TSARSTAT,X'FF'-TSARBUF2 ALWAYS SHUT OFF BIT ON ENTRY             
*                                                                               
         USING TSARD,R3                                                         
         LA    R3,TSRBLK1                                                       
         CLI   BCTSINDS,2          ARE WE READING/WRITING TO BUFF2              
         BNE   *+12                                                             
         LA    R3,TSRBLK2          IF SO USE BLOCK2                             
         OI    TSARSTAT,TSARBUF2   MARK THAT 2ND BUFFER IS IN USE               
*                                                                               
         MVC   TSACTN,BCFLAG1                                                   
         MVC   TSACOM,ACOMFACS                                                  
         L     R1,AIO1                                                          
         ST    R1,TSAREC           ADDRESS OF RECORD                            
*                                                                               
         CLI   BCFLAG1,TSAINI      *** TSAR INITIALIZE ***                      
         BNE   MTSAR10                                                          
         XC    0(L'TSRBLK1,R3),0(R3) CLEAR BLOCK                                
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGN,TSPEXPN      SET NUMBER OF TEMPEST PAGES                  
         OI    TSRECI,TSRVAR       VARIABLE LENGTH RECORDS                      
         MVI   TSKEYL,TRKEYLNQ     KEY LENGTH                                   
         LH    R1,=H'2000'                                                      
         STH   R1,TSRECL           MAX RECORD LENGTH                            
         MVI   TSINDS,TSIALLOC+TSIRTNAF   SET TO ALLOCATE FROM TEMPEST          
         CLI   BCTSINDS,2                 ARE WE INTERFACING W/ BUFF2           
         BNE   *+8                                                              
         OI    TSIND2,TSI2BUF2            USING TSAR BUFFER 2                   
         L     R1,AIO1                                                          
         ST    R1,TSAREC           ADDRESS OF RECORD                            
         XC    TSRNUM,TSRNUM       RECORD NUMBER                                
         MVI   TSACTN,TSAINI       INIT TSAR - CLEAR SPACE                      
*                                                                               
         CLI   BCTSINDS,2                 ARE WE INTERFACING W/ BUFF2           
         BE    *+16                                                             
         TM    TSRMODE,TSRMGOT1    DID WE ALREADY INITIALIZE TSAR BUFF1         
         BNO   MTSARGO                                                          
         B     *+12                                                             
*                                                                               
         TM    TSRMODE,TSRMGOT2    DID WE ALREADY INITIALIZE TSAR BUFF2         
         BNO   MTSARGO                                                          
*                                                                               
         LA    R1,TSRFLDS1         POINT TO FLDS FOR BLK 1                      
         CLI   BCTSINDS,2          ARE WE READING/WRITING TO BUFF2              
         BNE   *+8                                                              
         LA    R1,TSRFLDS2         POINT TO FLDS FOR BLK 2                      
         USING TSRTSAR1,R1         USE 1ST BLKS FLDS AS DSECT                   
         MVC   TSINDS,TSRTSAR1     SET PREVIOUS INDICATORS                      
         OI    TSINDS,TSIREUSE     SET TO REUSE PREVIOUS ALLOC                  
         MVC   TSPAGL,TSRLOWP1     SET PREV LOW PAGE NUMBER                     
         MVC   TSPAGN,TSRNUMP1     SET PREV NUMBER OF PAGES ALLOC               
         B     MTSARGO                                                          
         DROP  R1                                                               
*                                                                               
MTSAR10  CLI   BCFLAG1,TSARES      *** TSAR RESTORE ***                         
         BNE   MTSAR20                                                          
*                                                                               
         LA    R1,TSRFLDS1         POINT TO FLDS FOR BLK 1                      
         CLI   BCTSINDS,2          ARE WE READING/WRITING TO BUFF2              
         BNE   *+8                                                              
         LA    R1,TSRFLDS2         POINT TO FLDS FOR BLK 2                      
         USING TSRTSAR1,R1         USE 1ST BLKS FLDS AS DSECT                   
         MVC   TSINDS,TSRTSAR1     SET PREVIOUS INDICATORS                      
         OI    TSINDS,TSIREUSE     SET TO REUSE PREVIOUS ALLOC                  
         MVC   TSPAGL,TSRLOWP1     SET PREV LOW PAGE NUMBER                     
         MVC   TSPAGN,TSRNUMP1     SET PREV NUMBER OF PAGES ALLOC               
         B     MTSARGO                                                          
         DROP  R1                                                               
*                                                                               
MTSAR20  CLI   BCFLAG1,TSARDH      *** TSAR READ HIGH ***                       
         BNE   *+14                                                             
         XC    TSRNUM,TSRNUM                                                    
         B     MTSARGO                                                          
*                                                                               
         CLI   BCFLAG1,TSADEL      *** TSAR DELETE ***                          
         BNE   *+10                                                             
         XC    TSRNUM,TSRNUM                                                    
*                                                                               
MTSARGO  DS    0H                                                               
         GOTO1 XTSAR,TSARD                                                      
         MVC   BCTSERRS,TSERRS     PASS BACK ERRORS                             
         L     R1,BCFULL           RESET PREVIOUS PARM LIST                     
         XC    8(4,R1),8(R1)       CLEAR 3RD PARM                               
         MVC   8(1,R1),BCTSERRS    SAVE ERROR IN PARM LIST                      
         CLI   TSERRS,0                                                         
         BNE   ROUTH               EXIT WITH ERROR                              
*                                                                               
         CLI   BCTSINDS,2          ARE WE READING/WRITING TO BUFF2              
         BE    *+12                                                             
         OI    TSRMODE,TSRMGOT1    SHOW THAT BUFF1 HAS BEEN INITIALIZED         
         B     *+8                                                              
         OI    TSRMODE,TSRMGOT2    SHOW THAT BUFF2 HAS BEEN INITIALIZED         
*                                                                               
         LA    R1,TSRFLDS1         POINT TO FLDS FOR BLK 1                      
         CLI   BCTSINDS,2          ARE WE READING/WRITING TO BUFF2              
         BNE   *+8                                                              
         LA    R1,TSRFLDS2         POINT TO FLDS FOR BLK 2                      
         USING TSRTSAR1,R1         USE 1ST BLKS FLDS AS DSECT                   
         MVC   TSRLOWP1,TSPAGL     SAVE LOW TSAR PAGE NUMBER                    
         MVC   TSRNUMP1,TSPAGN     SAVE NUMBER OF PAGES ALLOCATED               
         MVC   TSRTSAR1,TSINDS     SAVE TEMPEST INDICATORS                      
         B     ROUTE                                                            
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TSAR TABLE                                                    *         
*                                                                     *         
* NTRY - BACCODE  = 1R ACCCOUNT (C/U/L/A)                             *         
*      - BCYYMMDD = WEEK ENDING DATE                                  *         
*      - BCPERIOD = PERIOD NUMBER                                     *         
*                                                                     *         
* EXIT - BCLINE#  = NEXT AVAILABLE LINE #                             *         
*      - CC=NEQ   = NO RECORDS FOR THIS PERSON/PERIOD                 *         
***********************************************************************         
         SPACE 1                                                                
BLDTSAR  DS    0H                                                               
         MVI   BCBYTE1,C'F'        SET 'FIRST TIME THROUGH' FLAG                
         XC    BCBYTE2,BCBYTE2                                                  
         XC    BCHALF,BCHALF       SET MOA AS NULLS                             
         XC    SAVSUBCD(6),SAVSUBCD  SAVED T/S SUBMITTED DATES                  
         ICM   RF,15,0(R1)         SORT OPTION                                  
         BZ    *+10                                                             
         MVC   BCOPT1(L'BCOPT1+L'BCOPT2+L'BCOPT3),0(RF)  SAVE OPTIONS           
         ICM   RF,15,4(R1)         OPEN MOA                                     
         BZ    *+10                                                             
         MVC   BCHALF,0(RF)        SAVE OFF OPEN MOA                            
         MVC   BCBYTE2,11(R1)      P3=TSAR BUFFER                               
         MVC   BCFULL,AIO          SAVE CALLERS A(AIO)                          
         MVC   BCLINE#,=H'1'       START AT LINE #1                             
*                                                                               
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         L     R4,AIO1                                                          
*                                                                               
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSAINI      INITIALIZE TSAR                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TSWRECD,R6          READ TIMESHEET WEEKLY PASSIVE PTR            
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ    X'3E'                                        
         MVI   TSWKSUB,TSWKSUBQ    X'0F'                                        
         MVC   TSWKCPY,BCACKCPY    COMPANY CODE                                 
         LA    RF,BCACKACT                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SHI   R1,1                                                             
         MVC   TSWKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         EX    R1,*-6                                                           
         OC    TSWKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SHI   R1,1                                                             
         MVC   TSWKPER(0),0(RF)    ISOLATE PERSON CODE                          
         EX    R1,*-6                                                           
         OC    TSWKPER,BCSPACES                                                 
         ICM   R1,7,BCYYMMDD                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSWKEND        WEEK ENDING DATE                             
         GOTO1 HIGH                                                             
         B     BLDT04                                                           
                                                                                
BLDT02   GOTO1 SEQ                                                              
                                                                                
BLDT04   CLC   TSWKEY(TSWKODS+L'TSWKODS-TSWKEY),KEYSAVE                         
         BE    BLDT10                                                           
*                                  NO/NO MORE DATA FOR THIS PER/WK/ODS          
         CLI   BCBYTE1,C'P'        TEST TSARADD PENDING                         
         BNE   *+8                                                              
         BAS   RE,ADDTSAR          ADD FINAL TSAR RECORD                        
         CLI   BCBYTE2,2           DON'T READ SAVED RECS FOR 2ND BUFF           
         BE    BLDT100                                                          
         GOTO1 ARESTSAR                                                         
         BE    BLDT100                                                          
         CLI   BCBYTE1,C'F'        TEST FIRST READ (=HIGH, NOT SEQ)             
         BNE   BLDT100             NO, SO OK TO IGNORE CC FROM RESTSAR          
         MVC   AIO,BCFULL          ELSE RESTORE AIO TO ORIGINAL VALUE           
         B     ROUTH               EXIT CC=HIGH - NO RECORDS FOUND              
*                                                                               
BLDT10   MVC   AIO,AIOBC                                                        
         MVC   BCDA,TSWKDA                                                      
         MVC   BCTIMST1,TSWKSTAT   SAVE T/S STATUS                              
*&&US                                                                           
         CLI   TSWKSTAT,TIMSFAPP   ONLY ALLOW FULLY APPROVED T/S                
         BE    *+8                                                              
         OI    BCFLAG5,BCFLUAT                                                  
*&&                                                                             
         GOTO1 GETREC                                                           
         MVC   BCCONTRA,TSWKULC    1C OR 1N ACCOUNT FROM KEY                    
*                                                                               
         USING TIMELD,R5                                                        
         L     R5,AIOBC                                                         
         AH    R5,=Y(ACCRFST-ACCKEY)                                            
BLDT20   CLI   TIMEL,0             END OF RECORD                                
         BE    BLDT02              READ NEXT                                    
         CLI   TIMEL,GDAELQ                                                     
         BE    BLDT32                                                           
         CLI   TIMEL,TIMELQ                                                     
         BNE   BLDT30                                                           
         CLI   TIMETYP,TIMEINP     TEST TIMEINP                                 
         BE    BLDT40              YES - START NEW CLUSTER                      
         CLI   TIMETYP,TIMEDNAR    TEST TIME DAY NARRATIVE                      
         BNE   BLDT50              NO - CONTINUE WITH EXISTING                  
         OI    BCFLAG5,BCFLTDN     SET TIME DAY NARRATIVE EXIST                 
                                                                                
BLDT30   SR    R0,R0                                                            
         IC    R0,TIMLN                                                         
         AR    R5,R0                                                            
         B     BLDT20                                                           
                                                                                
         USING GDAELD,R5                                                        
BLDT32   CLI   GDATYPE,GDACLSUB    SAVE ANY EXISTING SUBMITTED DATES            
         BNE   *+14                                                             
         MVC   SAVSUBCD,GDADATE                                                 
         B     BLDT30                                                           
         CLI   GDATYPE,GDALMSUB                                                 
         BNE   *+10                                                             
         MVC   SAVSUBMD,GDADATE                                                 
         B     BLDT30                                                           
                                                                                
         USING TIMELD,R5                                                        
*                                                                               
*              BUILD TSAR ITEM                                                  
*                                                                               
BLDT40   CLI   BCBYTE1,C'P'        TEST TSARADD PENDING                         
         BNE   *+8                                                              
         BAS   RE,ADDTSAR                                                       
                                                                                
         GOTO1 AXAIO,AIO1          CLEAR TSAR ITEM BUFFER                       
         MVC   TRKLINE,TIMLINE#    USED TO ESTABLISH SEQUENCE                   
         MVC   TRKACC,TIMACC       SJ/1N ACCOUNT                                
         MVC   TRKTSK,TIMTSK       TASK CODE                                    
         MVC   TRKOFF,TIMOFF       CLIENT OFFICE CODE                           
         MVC   TRKCNTRA,BCCONTRA   CONTRA ACCOUNT (EITHER 1C OR 1N)             
         MVC   TRKDA,BCDA          D/A                                          
         MVC   TRKPEDT,BCYYMMDD    WEEK ENDING DATE                             
         MVC   TRKTSNUM,TIMLINE#   TIMESHEET #                                  
         MVI   TRKSTAT,TRKSUPDT    MARK ITEMS AS HAVING BEEN UPDATED            
         TM    BCPRIND,BCPRIDTM    TEST DAILY TIME USER                         
         BNZ   *+8                 YES - DON'T SET 'PENDING' IND. YET           
         MVI   BCBYTE1,C'P'        SET TSARADD PENDING                          
         LA    R3,TRDATA           R3=A(TSAR DATA AREA)                         
NEW      USING TIMELD,R3                                                        
*&&US*&& B     BLDT60                                                           
                                                                                
         SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AHI   R1,-1                                                            
         MVC   NEW.TIMEL(0),TIMEL       COPY ELEMENT TO TRDATA                  
         EX    R1,*-6                                                           
*        CLI   BCBYTE2,2                                                        
*        BNE   *+10                                                             
*        MVC   NEW.TIMMOA,BCHALF   ONLY AMEND MOA FOR BUFFER 2                  
         IC    R1,TIMLN            RESTORE LENGTH                               
         CLI   TIMLN,TIMILN2Q      TEST EL HAS BILLABLE INFO FOR EURO           
         BL    BLDT44              NO                                           
*                                  YES - EXTEND TSAR COPY OF ELEMENT            
*&&UK                                                                           
         ZAP   NEW.TIMERTE,=P'0'  INIT EURO CHARGE RTE                          
         ZAP   NEW.TIMECRTE,=P'0'  INIT EURO COST RTE                           
         AHI   R1,L'TIMERTE+L'TIMECRTE   BUMP LENGTH                            
*&&                                                                             
         STC   R1,NEW.TIMLN        SET REVISED LENGTH IN TRDATA                 
BLDT44   AR    R3,R1               BUMP TRDATA PTR                              
         B     BLDT30                                                           
*                                  EXISTING CLUSTER                             
BLDT50   CLI   TIMETYP,TIMETAX     TAX DETAIL ITEM                              
         BE    BLDT60                                                           
         CLI   TIMETYP,TIMENAR     NARRATIVE ITEM                               
         BE    BLDT60                                                           
         CLI   TIMETYP,TIMETIME    ETIME ITEM                                   
         BE    BLDT60                                                           
         CLI   TIMETYP,TIMEARIN    ETIME APPROVAL                               
         BE    BLDT60                                                           
         CLI   TIMETYP,TIMERJAP    ETIME REJECT/APPROVE COMMENTS                
         BE    BLDT60                                                           
         CLI   TIMETYP,TIMEITMS    ITEMS/MATERIALS                              
         BE    BLDT60                                                           
         CLI   TIMETYP,TIMEXTRA    EXTRA STATUS ELEMENT                         
         BNE   BLDT30              IGNORE - GET NEXT TIMEL                      
*&&UK                                                                           
         TM    BCPRIND,BCPRIDTM    ARE WE ENTERING DAILY TIME                   
         BZ    BLDT60              NO - ADD ELEMENT TO TSAR                     
         CLI   BCOPT3,C'Y'         ARE WE SHOWING ALL DAYS                      
         BE    *+14                YES                                          
         CLC   BCINPDTE,TIMXTDDT   ELSE DOES DATE MATCH                         
         BNE   BLDT30              NO - IGNORE THIS ONE                         
         MVI   BCBYTE1,C'P'        SET TSARADD NOW PENDING                      
*&&                                                                             
BLDT60   SR    R1,R1               COPY 8B ELEMENT                              
         IC    R1,TIMLN            TO THE TSAR RECORD                           
         AHI   R1,-1                                                            
         MVC   NEW.TIMEL(0),TIMEL                                               
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)         BUMP TRDATA PTR                              
         B     BLDT30                                                           
         DROP  NEW                                                              
*                                                                               
BLDT100  SR    R1,R1                                                            
         ICM   R1,3,BCLINE#        GET LINE NUMBER                              
         CH    R1,=H'1'            HAVE WE ADDED ANYTHING TO THE BUFFER         
         BE    ROUTH               NO - EXIT HIGH                               
         CLI   BCBYTE2,1           ONLY SORT ON SJ FOR TSAR BUFF 1              
         BNE   BLDT110                                                          
         CLI   BCOPT1,C'Y'         SORT BY SJ CODE                              
         BE    *+12                                                             
         CLI   BCPROF1,C'Y'        SORT BY SJ CODE                              
         BNE   BLDT110                                                          
         GOTO1 ASJSORT,BCDMCB,1                                                 
         B     BLDX                                                             
*                                                                               
BLDT110  LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   3(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSNSORT,BCDMCB          SORT BY TS#                             
*                                                                               
BLDX     B     ROUTE                                                            
*                                  ADD A TSAR RECORD                            
ADDTSAR  ST    RE,BCDUB2           SAVE A(RETURN)                               
         SR    R3,R4                                                            
         STCM  R3,3,TRLEN          SET LENGTH OF TSAR RECORD                    
         SR    R1,R1                                                            
         ICM   R1,3,BCLINE#        BUMP NEXT LINE #                             
         LA    R1,1(R1)                                                         
         STCM  R1,3,BCLINE#                                                     
*                                                                               
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSAADD      ADD TSAR ITEM                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BCBYTE1,C' '        CLEAR 'TSARADD PENDING' IND                  
         L     RE,BCDUB2           RESTORE A(RETURN)                            
         BR    RE                                                               
*                                                                               
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* RESTORE SAVED TSAR ITEMS                                            *         
*                                                                     *         
* NTRY - BACCODE  = 1R ACCCOUNT (C/U/L/A)                             *         
*      - BCYYMMDD = WEEK ENDING DATE                                  *         
*                                                                     *         
* EXIT - BCLINE#  = NEXT AVAILABLE LINE #                             *         
*      - CC=NEQ   = SAVED ITEMS FOR THIS PERSON/PERIOD                *         
***********************************************************************         
         SPACE 1                                                                
RESTSAR  DS    0H                                                               
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         L     R4,AIO1                                                          
         XC    BCFLAG3,BCFLAG3                                                  
*                                                                               
         USING TSSRECD,R6          READ TIMESHEET SAVE RECORDS                  
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   TSSKTYP,TSSKTYPQ    X'3E'                                        
         MVI   TSSKSUB,TSSKSUBQ    X'11'                                        
         MVC   TSSKCPY,BCACKCPY    COMPANY CODE                                 
         LA    RF,BCACKACT                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SHI   R1,1                                                             
         MVC   TSSKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         EX    R1,*-6                                                           
         OC    TSSKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SHI   R1,1                                                             
         MVC   TSSKPER(0),0(RF)    ISOLATE PERSON CODE                          
         EX    R1,*-6                                                           
         OC    TSSKPER,BCSPACES                                                 
         ICM   R1,7,BCYYMMDD                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSSKEND        WEEK ENDING DATE                             
         GOTO1 HIGH                                                             
*                                                                               
RES200   CLC   TSSKEY(TSSKODS+L'TSSKODS-TSSKEY),KEYSAVE                         
         BNE   RESX                                                             
         MVI   BCFLAG3,BCFL3OK     ITEM FOUND                                   
         MVC   AIO,AIOBC                                                        
         GOTO1 GETREC                                                           
*                                                                               
         USING TIMELD,R6                                                        
         L     R6,AIOBC                                                         
         AH    R6,=Y(ACCRFST-ACCKEY)                                            
RES300   CLI   0(R6),0             END OF RECORD                                
         BE    RES800                                                           
         CLI   0(R6),TIMELQ        X'8B'                                        
         BNE   RES400                                                           
         CLI   TIMETYP,TIMEFLD     ONLY BRANCH ON COMMENT CLUSTERS              
         BE    RES500                                                           
         CLI   TIMETYP,TIMEINP     INPUT DETAIL ELEMENTS                        
         BE    RES500                                                           
RES400   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     RES300                                                           
*                                                                               
*              BUILD TSAR ITEM                                                  
*                                                                               
RES500   GOTO1 AXAIO,AIO1          CLEAR TSAR ITEM BUFFER                       
         MVC   BCHALF,TIMFLINE                                                  
         CLI   TIMETYP,TIMEFLD     ONLY BRANCH ON COMMENT CLUSTERS              
         BE    *+10                                                             
         MVC   BCHALF,TIMLINE#                                                  
         MVC   TRKLINE,BCHALF      USED TO ESTABLISH SEQUENCE                   
         MVC   TRKDA,DMDSKADD      D/A                                          
         MVC   TRKPEDT,BCYYMMDD    WEEK ENDING DATE                             
         MVC   TRKTSNUM,BCHALF     TIMESHEET #                                  
         MVI   TRKSTAT,TRKSSAVE                                                 
*                                                                               
         LA    R3,TRDATA                                                        
RES600   SR    R1,R1               COPY ENTIRE 8B CLUSTER FROM RECORD           
         IC    R1,1(R6)            TO THE CLUSTER BUFFER                        
         SHI   R1,1                                                             
         MVC   0(0,R3),0(R6)       COPY OVER X'8B' ELEMENT                      
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
         LA    R6,1(R1,R6)                                                      
         CLI   TIMEL,TIMELQ                                                     
         BNE   RES650                                                           
         CLI   TIMETYP,TIMETAX     TAX DETAIL ITEM                              
         BE    RES600                                                           
         CLI   TIMETYP,TIMENAR     NARRATIVE ITEM                               
         BE    RES600                                                           
         CLI   TIMETYP,TIMEXTRA    EXTRA STATUS ELEMENT                         
*&&US*&& BE    RES600                                                           
*&&UK                                                                           
         BNE   RES650                                                           
         TM    BCPRIND,BCPRIDTM    ARE WE ENTERING DAILY TIME                   
         BZ    RES600              NO - ADD ELEMENT TO TSAR                     
         CLI   BCOPT3,C'Y'         ARE WE SHOWING ALL                           
         BE    RES600              YES                                          
         CLC   BCINPDTE,TIMXTDDT   DOES DATE MATCH                              
         BNE   RES400              NO - GET NEXT ELEMENT                        
         B     RES600              YES - ADD ELEMENT TO TSAR                    
*&&                                                                             
*                                                                               
RES650   SR    R3,R4                                                            
         STCM  R3,3,TRLEN          SET LENGTH OF TSAR RECORD                    
         SR    R1,R1                                                            
         ICM   R1,3,BCLINE#        BUMP NEXT LINE #                             
         LA    R1,1(R1)                                                         
         STCM  R1,3,BCLINE#                                                     
         GOTO1 ATSAR,BCDMCB,TSAADD,1    ADD TSAR ITEM                           
         BE    RES300                                                           
         DC    H'0'                                                             
*                                                                               
RES800   GOTO1 SEQ                                                              
         LA    R6,BIGKEY                                                        
         B     RES200                                                           
*                                                                               
RESX     CLI   BCFLAG3,BCFL3OK                                                  
         BE    ROUTE                                                            
         B     ROUTH               CC=HI - NO SAVED ITEMS FOUND                 
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE RECORDS FROM TSAR TABLE                                      *         
*                                                                     *         
* NTRY - BACCODE  = 1R ACCCOUNT (C/U/L/A)                             *         
*      - BCYYMMDD = WEEK ENDING DATE                                  *         
*                                                                     *         
* EXIT - RECORDS ON FILE ARE UPDATED                                  *         
***********************************************************************         
         SPACE 1                                                                
UPDTSAR  DS    0H                                                               
         MVC   BCFULL,AIO                                                       
         USING CSTTABD,R2                                                       
         LA    R2,LWS1CTAB         R2=A(1C COSTING ACCOUNT TABLE)               
         XC    LWS1CNUM,LWS1CNUM   CLEAR # TABLE ENTRIES                        
         XC    BCPL16,BCPL16       CLEAR 1C SAVE KEY AREA                       
*                                                                               
         LA    RE,LWS1CTAB                                                      
         LH    RF,=Y(L'LWS1CTAB)                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         L     R4,AIO1                                                          
         LA    R3,TSARDH                                                        
UPD10    GOTO1 AXAIO,AIO1                                                       
         GOTO1 ATSAR,BCDMCB,(R3),1                                              
         TM    BCTSERRS,TSEEOF                                                  
         BO    UPD20                                                            
         TM    TRKSTAT,TRKSDEL+TRKSSAVE SKIP SAVED/DELETED ITEMS                
         BNZ   UPD15                                                            
         MVC   CSTOFFC,TRKOFF      OFFICE                                       
         TM    BCCPYST4,CPYSOFF2                                                
         BO    *+10                                                             
         MVC   CSTOFFC,BCSPACES    SPACES FOR 1 BYTE OFFICES                    
         MVC   CSTCNTRA,TRKCNTRA   CONTRA ACCOUNT                               
         MVC   CSTLINE,TRKLINE     LINE NUMBER                                  
         MVC   CSTLNSUB,TRKLNSUB   SUB LINE NUMBER                              
         LA    R2,CSTLNQ(R2)                                                    
         SR    R1,R1                                                            
         ICM   R1,3,LWS1CNUM                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,LWS1CNUM       BUMP TABLE COUNT                             
         CH    R1,=Y(LWS1CMAX)                                                  
         BNH   *+6                                                              
         DC    H'0'                CONTRA ACCOUNT TABLE FULL                    
UPD15    LA    R3,TSANXT                                                        
         B     UPD10                                                            
*                                                                               
UPD20    SR    R3,R3                                                            
         ICM   R3,3,LWS1CNUM                                                    
         GOTO1 AXSORT,DMCB,LWS1CTAB,(R3),CSTLNQ,CSTKLNQ,0                       
*                                                                               
         USING CSTTABD,R2                                                       
UPD50    LA    R2,LWS1CTAB         R2 = A(1C COSTING ACCOUNT TABLE)             
         USING TSARRECD,R4                                                      
         L     R4,AIO1             R4 = A(TSAR RECORD AREA)                     
         SR    R3,R3                                                            
         ICM   R3,3,LWS1CNUM       R3 = # TABLE ENTRIES/# TIMES LOOP            
         BZ    UPDX                                                             
*                                                                               
UPD100   DS    0H                                                               
         GOTO1 AXAIO,AIO1                                                       
         MVC   TRKLINE,CSTLINE     LINE NUMBER                                  
         MVC   TRKLNSUB,CSTLNSUB   SUB LINE NUMBER                              
         GOTO1 ATSAR,BCDMCB,TSARDH,1                                            
         CLC   TRKLINE(L'CSTLINE+L'CSTLNSUB),CSTLINE                            
         BE    *+6                                                              
         DC    H'0'                TSAR BUFFER DOEST MATCH CNTRA TABLE          
*                                                                               
         CLC   BCPL16,TRKSRTK      SAME 1C SORT KEY                             
         BE    UPD200                                                           
         OC    BCPL16,BCPL16       TEST 1ST TIME THROUGH                        
         BZ    UPD120                                                           
         TM    BCCPYST4,CPYSOFF2                                                
         BO    UPD110                                                           
         CLC   TRKCNTRA,BCPL16+2                                                
         BE    UPD200                                                           
UPD110   GOTO1 AWRITREC            WRITE RECORD TO FILE                         
         GOTO1 ADELSEQ             DELETE NEXT SEQ RECORD (IF ANY)              
UPD120   MVC   BCPL16,TRKSRTK                                                   
*                                                                               
UPD150   DS    0H                                                               
         GOTO1 AMRKAST             MARK ASTEL WITH PERIOD ENDDATE               
         USING TIMRECD,R6                                                       
         L     R6,AIOBC            *** BUILD TIME RECORD KEY ***                
         GOTO1 AXAIO,AIOBC         CLEAR RECORD I/O AREA                        
         MVC   TIMKEY,BCSPACES                                                  
         MVC   TIMKCULA,BCACCODE   1R ACCOUNT                                   
         MVC   TIMKOFF,TRKOFF      CLIENT OFFICE (1R OFFC IF CNTRA=1N)          
         TM    BCCPYST4,CPYSOFF2                                                
         BO    *+10                                                             
         MVC   TIMKOFF,BCSPACES    SPACES FOR 1 BYTE OFFICES                    
         MVC   TIMKCCPY,CMPY                                                    
         MVC   TIMKULC,TRKCNTRA    CONTRA (EITHER 1C OR 1N)                     
         MVC   TIMKPEDT,TRKPEDT    PERIOD ENDING DATE                           
         MVC   TIMKREF,=C'*TIME*'                                               
         MVI   TIMKSBR,0                                                        
         MVI   TIMRSTAT,TIMSFAPP   INDICATOR BYTES USED TO MARK                 
         TM    BCPRIND,BCPRIMCT    TEST MCS TIME                                
         BZ    *+10                                                             
         MVC   TIMRSTAT,BCTIMST1   SET T/S STATUS FROM PASSED VALUE             
         MVI   TIMRRI2,TIMKRI2Q    THESE RECORDS AS SPECIAL RECORDS             
         MVI   TIMRRI3,TIMKRI3Q    YET MAINTAIN THE LOOK OF REGULAR             
         MVI   TIMRRI4,TIMKRI4Q    TRANSACTION RECORDS                          
         LH    R1,=Y(TIMRFST-TIMKEY)                                            
         LA    R1,1(R1)                                                         
         STCM  R1,3,TIMRLEN                                                     
         MVC   BCSEQ#,=H'1'        INITIALIZE ELEMENT SEQUENCE NUMBER           
*                                                                               
         L     RE,AIOBC                                                         
         AH    RE,=Y(TIMRFST-TIMKEY)                                            
         OC    BCPIDNO,BCPIDNO     *** D8 PERSON ID ELEMENT ***                 
         BZ    UPD160                                                           
         USING PIDELD,RE                                                        
         MVI   PIDEL,PIDELQ        X'D8' ELEMENT                                
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,BCPIDNO       PERSON ID #                                  
         SR    R1,R1                                                            
         ICM   R1,3,TIMRLEN                                                     
         AH    R1,=Y(PIDLNQ)                                                    
         STCM  R1,3,TIMRLEN                                                     
         AHI   RE,PIDLNQ                                                        
         DROP  RE                                                               
                                                                                
UPD160   OC    SAVSUBCD(6),SAVSUBCD                                             
         BZ    *+8                                                              
         BAS   RF,ADDSUBD          ADD SUBMITTED DATE ELS                       
                                                                                
         TM    BCPRIND,BCPRIDTM    ARE WE USING DAILY TIME                      
         BZ    UPD200              NO - DON'T NEED EXISTING ELEMENTS            
         GOTO1 AADDEXST,0          GET EXISTING ELEMENTS FOR OTHER DAYS         
*                                                                               
UPD200   TM    TRKSTAT,TRKSDEL     DONT ADD CLUSTER IF MARKED DELETED           
         BO    UPDNXT                                                           
         XC    CMCHKLN,CMCHKLN     INITIALISE SAVED L'CLUSTER CHUNK             
         GOTO1 AADDCLST,(R4)       ADD CLUSTER TO END OF RECORD                 
         BNE   UPD220              DIDN'T FIT - CREATE NEW RECORD               
         OC    CMCHKLN,CMCHKLN     ELSE TEST CLUSTER WAS SPLIT                  
         BZ    UPDNXT              NO - FINISHED                                
*                                                                               
* IF CLUSTER DIDNT FIT ON CURRENT RECORD THEN WRITE IT OUT                      
* AND BUILD A NEW RECORD WITH THE SUBREFERENCE # BUMPED BY 1.                   
*                                                                               
UPD220   GOTO1 AWRITREC                                                         
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'TIMKEY),TIMKEY                                          
         GOTO1 AXAIO,AIOBC         CLEAR RECORD I/O AREA                        
         MVC   TIMKEY,BIGKEY                                                    
         SR    R1,R1                                                            
         IC    R1,TIMKSBR          BUMP SEQUENCE NUMBER                         
         LA    R1,1(R1)                                                         
         STC   R1,TIMKSBR                                                       
         MVI   TIMRSTAT,TIMSFAPP   INDICATOR BYTES USED TO MARK                 
         TM    BCPRIND,BCPRIMCT    TEST MCS TIME                                
         BZ    *+10                                                             
         MVC   TIMRSTAT,BCTIMST1   SET T/S STATUS FROM PASSED VALUE             
         MVI   TIMRRI2,TIMKRI2Q    THESE RECORDS AS SPECIAL RECORDS             
         MVI   TIMRRI3,TIMKRI3Q    YET MAINTAIN THE LOOK OF REGULAR             
         MVI   TIMRRI4,TIMKRI4Q    TRANSACTION RECORDS                          
         LH    R1,=Y(TIMRFST-TIMKEY)                                            
         LA    R1,1(R1)                                                         
         STCM  R1,3,TIMRLEN                                                     
*                                                                               
         OC    BCPIDNO,BCPIDNO     *** D8 PERSON ID ELEMENT ***                 
         BZ    UPD250                                                           
         L     RE,AIOBC            *** BUILD TIME RECORD KEY ***                
         AH    RE,=Y(TIMRFST-TIMKEY)                                            
         USING PIDELD,RE                                                        
         MVI   PIDEL,PIDELQ        X'D8' ELEMENT                                
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,BCPIDNO       PERSON ID #                                  
         SR    R1,R1                                                            
         ICM   R1,3,TIMRLEN                                                     
         AH    R1,=Y(PIDLNQ)                                                    
         STCM  R1,3,TIMRLEN                                                     
         AHI   RE,PIDLNQ                                                        
         DROP  RE                                                               
*                                                                               
UPD250   OC    SAVSUBCD(6),SAVSUBCD                                             
         BZ    *+8                                                              
         BAS   RF,ADDSUBD          ADD SUBMITTED DATE ELS                       
                                                                                
         GOTO1 AADDCLST,(R4)       ADD CLUSTER TO A BRAND NEW RECORD            
         BE    *+6                                                              
         DC    H'0'                NON-MATERIALS CLUSTER > 1 RECORD             
         OC    CMCHKLN,CMCHKLN     TEST ALL OF CLUSTER NOW ADDED                
         BNZ   UPD220              NO - GO ROUND AGAIN                          
*                                                                               
UPDNXT   LA    R2,CSTLNQ(R2)       NEXT TABLE ENTRY                             
         BCT   R3,UPD100           DECREMENT TABLE COUNT                        
*                                                                               
UPDX     OC    BCPL16,BCPL16                                                    
         BZ    UPDX10                                                           
         GOTO1 AWRITREC            WRITE OUT LAST RECORD STILL IN BUFFR         
         GOTO1 ADELSEQ             DELETE NEXT SEQ RECORD (IF ANY)              
*                                                                               
UPDX10   GOTO1 ADELETE             DELETE RECORDS NOT IN 1C TABLE               
UPDX20   MVC   AIO,BCFULL                                                       
         B     ROUTE                                                            
*                                  ADD SUBMITTED DATE GDAELS                    
*                                  RE=CURRENT EOR, RF=A(RETURN)                 
ADDSUBD  SR    R1,R1                                                            
         ICM   R1,3,TIMRLEN        MAINTAIN RECLEN                              
         USING GDAELD,RE                                                        
         OC    SAVSUBCD,SAVSUBCD   TEST CLIENT APP SUBMITTED DATE               
         BZ    ASUBD10                                                          
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDACLSUB                                                 
         MVC   GDADATE,SAVSUBCD                                                 
         AHI   R1,GDALNQ                                                        
         STCM  R1,3,TIMRLEN        UPDATE RECLEN                                
         AHI   RE,GDALNQ                                                        
                                                                                
ASUBD10  OC    SAVSUBMD,SAVSUBMD   TEST LINE MGR APP SUBMITTED DATE             
         BZ    ASUBDX                                                           
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDALMSUB                                                 
         MVC   GDADATE,SAVSUBMD                                                 
         AHI   R1,GDALNQ                                                        
         STCM  R1,3,TIMRLEN        UPDATE RECLEN                                
ASUBDX   BR    RF                                                               
         DROP  R4,R6,RE                                                         
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT CLUSTER TO RECORD IN AIOBC                              *         
*                                                                     *         
* NTRY - R1=A(TSAR ITEM)                                              *         
*                                                                     *         
* EXIT - CC=EQU - RECORD ADDED CORRECTLY                              *         
*        CC=NEQ - CLUSTER DOESNT FIT ON THIS RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
ADDCLST  DS    0H                                                               
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         LR    R4,R1                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,TRLEN                                                       
         SH    RF,=Y(TRLEN1Q)                                                   
         STH   RF,BCHALF           BCHALF=LENGTH OF 8B ELEMENT CLUSTER          
         STH   RF,SVCHKLN          AND DEFAULT L'CHUNK = ALL                    
         LA    R0,TRDATA           A(ELEMENT CLUSTER)                           
         AH    R0,CMCHKLN          +L'CHUNK ALREADY MOVED OR 0                  
         ST    R0,TCURDTA          START FROM HERE                              
*                                                                               
         L     RE,AIOBC                                                         
         SR    R0,R0                                                            
         ICM   R0,3,ACCORLEN(RE)   R0=CURRENT L'RECORD                          
         LH    RF,BCHALF           RF=L'CLUSTER                                 
         SH    RF,CMCHKLN          -L'CHUNK ALREADY MOVED OR 0                  
         AR    RF,R0                                                            
         CHI   RF,L'BCIO-50        TEST > MAX USABLE RECLEN                     
         BNH   ADDC08              NO - CONTINUE                                
         LHI   RF,L'BCIO-50                                                     
         SR    RF,R0               RF=L'FREE SPACE ON RECORD                    
         L     RE,TCURDTA                                                       
         USING TIMELD,RE                                                        
         LR    R1,RE               INIT R1 FOR SAFETY                           
         SR    R0,R0                                                            
*                                  MAY BE ABLE TO SPLIT CLUSTER...              
ADDC02   CLI   TIMEL,TIMELQ                                                     
         BNE   *+10                                                             
         MVC   TIMSEQ,BCSEQ#+1     SET CURRENT SEQUENCE ON THE FLY              
*                                  (SEQ# WILL BE BUMPED AFTER LAST              
         CLI   TIMEL,0              CHUNK IS ADDED)                             
         BNE   *+6                                                              
         DC    H'0'                SHOULD NEVER HIT EOR HERE                    
         IC    R0,TIMLN                                                         
         SR    RF,R0               SUBTRACT L'EL FROM L'FREE SPACE              
         BNP   ADDC04                                                           
         LR    R1,RE               SAVE A(THIS ELEMENT)                         
         AR    RE,R0               AND BUMP TO NEXT                             
         B     ADDC02                                                           
ADDC04   LR    RE,R1               RE=A(LAST ELEMENT) THAT CAN FIT              
         CLI   TIMETYP,TIMEITMS    TEST ELEMENT IS MATERIALS                    
         BNE   ROUTH               NO - CLUSTER WILL NEED NEW RECORD            
                                                                                
* TIMEITMS TIMELS IN ONE CLUSTER CAN BE SPLIT OVER MORE THAN 1 RECORD           
                                                                                
         L     RF,TCURDTA          OK TO SPLIT CLUSTER                          
         SR    RE,RF               RE=L'TO ADD INTO THIS RECORD                 
         STH   RE,SVCHKLN          L' TO UPDATE RECLEN LATER                    
         LH    R0,CMCHKLN          MAINTAIN CUMULATIVE L'MOVED                  
         AR    R0,RE                                                            
         STH   R0,CMCHKLN          SAVE FOR NEXT CALL                           
         LR    R1,RE               SET LENGTHS FOR MVCL                         
         LR    RF,RE                                                            
         L     RE,AIOBC                                                         
         ICM   R0,3,ACCORLEN(RE)                                                
         AHI   R0,-1                                                            
         AR    RE,R0                                                            
         L     R0,TCURDTA                                                       
         MVCL  RE,R0               FROM TRDATA -> TIME RECORD                   
         B     ADDC28                                                           
         DROP  RE                                                               
* ENOUGH FREE SPACE TO ADD WHOLE CLUSTER/LAST CHUNK:                            
ADDC08   SR    R1,R1                                                            
         ICM   R1,3,BCSEQ#         R1=CURRENT SEQUENCE #                        
         L     RF,TCURDTA                                                       
ADDC10   CLI   0(RF),TIMELQ        INSERT SEQUENCE # IN X'8B' ELEMS             
         BNE   ADDC20                                                           
         STC   R1,TIMSEQ-TIMEL(RF)                                              
         SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     ADDC10                                                           
ADDC20   LA    R1,1(R1)            BUMP SEQUENCE NUMBER                         
         STCM  R1,3,BCSEQ#         FOR NEXT CLUSTER                             
*                                                                               
         L     RE,AIOBC            *** ATTACH ELEMENT CLUSTER ***               
         SR    RF,RF                                                            
         ICM   RF,3,ACCORLEN(RE)                                                
*                                                                               
         SHI   RF,1                                                             
         AR    RE,RF               RE=A(AREA TO ATTACH CLUSTER)                 
         L     R0,TCURDTA          R0=A(TSAR 8B ELEM CLUSTER) SOURCE            
         LH    RF,BCHALF           RF=L'8B ELEMENT CLUSTER                      
         SH    RF,CMCHKLN          -L'CHUNK ALREADY MOVED OR 0                  
         STH   RF,SVCHKLN          L' TO UPDATE RECLEN                          
         LR    R1,RF               R1=L'8B ELEMENT CLUSTER                      
         MVCL  RE,R0               CLUSTER -> END OF RECORD                     
                                                                                
         XC    CMCHKLN,CMCHKLN     NOTHING REMAINING - CLEAR SAVED L'           
                                                                                
ADDC28   DS    0H                                                               
*&&UK                                                                           
         L     RF,AIOBC            **FIX ELEMENTS WITH WRONG DATA**             
         SR    RE,RE                                                            
         ICM   RE,3,ACCORLEN(RF)                                                
         AHI   RE,-1                                                            
         AR    RF,RE               BUMP TO BEGINNING OF ELM CLUSTER             
*                                                                               
         CLI   0(RF),TIMELQ        DIE IF NOT X'8B' ELEMS                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         CHI   R0,TIMB2LNQ         IF EXTENDED BILLING INFO - DO NOT            
         BNE   ADDC30                 INCLUDE EORU FIELDS                       
         LA    R0,TIMILN2Q            LEAVE AS REGULAR BILLING DETAIL           
         STC   R0,1(RF)               RESET WITH CORRECT LEN                    
         LH    R1,SVCHKLN          FIX TOTAL LENGTH OF ELEMNT                   
         LR    R0,R1                                                            
         AHI   R0,-8                  SUBTRACT EXTRA LENGTH                     
         STH   R0,SVCHKLN                                                       
*                                                                               
         LA    R0,TIMB2LNQ            LEN OF INCORRECT ELEMENT                  
         SR    R1,R0                                                            
         LA    RE,TIMILN2Q(RF)        RE=A(START OF EURO FIELDS)                
         LA    R0,TIMB2LNQ(RF)        R0=A(START OF OTHER 8B ELS)               
         LR    RF,R1                  RF=LENGTH OF REMAINING 8BS                
         MVCL  RE,R0                  BUMP ELM UP 8 BYTES                       
         XC    0(8,RE),0(RE)          CLEAR LAST 8 BYTES AFT MVCL               
*&&                                                                             
ADDC30   L     RE,AIOBC            *** UPDATE RECORD LENGTH ***                 
         SR    RF,RF                                                            
         ICM   RF,3,ACCORLEN(RE)                                                
         AH    RF,SVCHKLN          L'CLUSTER OR CHUNK ADDED THIS TIME           
         STCM  RF,3,ACCORLEN(RE)                                                
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* ADD EXISTING ELEMENTS FOR OTHER DAYS OF PERIOD  - DAILY TIME        *         
*                                                                     *         
* NTRY - R1=A(TSAR ITEM)                                              *         
*                                                                     *         
* EXIT - CC=EQU - RECORD ADDED CORRECTLY                              *         
*        CC=NEQ - RANSTER DOESNT FIT ON THIS RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMRECD,R6                                                       
ADDEXST  L     R6,AIOBC            *** READ FOR ACTIVE KEY ***                  
         XC    BCFLAG1,BCFLAG1                                                  
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         MVI   BCFLAG1,X'FF'       DEALING WITH SAVE RECORDS                    
         XC    BCHALF,BCHALF       CLEAR - USE FOR LENGTH OF EL CLUSTER         
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'TIMKEY),TIMKEY                                          
         MVC   SAVEDKEY(L'TIMKEY),TIMKEY                                        
ADDE02   GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'TIMKEY),BIGKEY  IF RECORD FOUND THEN WRITE             
         BNE   ADDEXSTX            NEW RECORD ELSE ADD A NEW ONE                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO2                                                          
*                                                                               
***********************************************************************         
* DELETE ALL PASSIVE POINTERS BEFORE TIMREC IS CHANGED                *         
* THEY WILL BE RE-ADDED WHEN RECORD IS WRITTEN BACK                   *         
***********************************************************************         
*                                                                               
         USING CPTRBLK,BCCPTRBK                                                 
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   LDGLVALN(L'BC1RLNQS),BC1RLNQS   SET 1R LEVLENS FOR CPTR          
         MVC   CPYSTA1(7),BCCPYST1             SET CPY STATUS                   
         GOTO1 APADDLE,BCDMCB,(C'D',(R6)),(C'K',CPTRBLK),0,0,ACOMFACS           
*                                                                               
         LA    R2,BCELEM                                                        
         AH    R6,=Y(TIMRFST-TIMKEY)                                            
         USING TIMELD,R6                                                        
         SR    R1,R1                                                            
ADDE04   CLI   TIMEL,0                                                          
         BE    ADDE10                                                           
         CLI   TIMEL,TIMELQ                                                     
         BE    ADDE08                                                           
ADDE06   SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AR    R6,R1                                                            
         B     ADDE04                                                           
*                                                                               
ADDE08   CLI   TIMETYP,TIMEXTRA                                                 
         BNE   ADDE16                                                           
         CLC   TIMXTDDT,BCINPDTE   DOES DATE MATCH ADD/CHANGE DATE              
         BNE   ADDE18              NO - COPY ELEMENT TO AIOBC                   
         B     ADDE26              GET NEXT CLUSTER OF 8B'S                     
*                                                                               
         USING TIMRECD,R6                                                       
ADDE10   LA    R6,SAVEDKEY                                                      
         MVC   BIGKEY(L'TIMKEY),TIMKEY                                          
         LA    R6,BIGKEY                                                        
         SR    R1,R1                                                            
         IC    R1,TIMKSBR          BUMP SEQUENCE NUMBER                         
         LA    R1,1(R1)                                                         
         STC   R1,TIMKSBR                                                       
         MVC   SAVEDKEY(L'TIMKEY),TIMKEY                                        
         B     ADDE02                                                           
*                                                                               
         USING TIMELD,R6                                                        
ADDE16   IC    R1,TIMLN            LENGTH OF ELEMENT                            
         LH    RE,BCHALF                                                        
         AR    RE,R1                                                            
         STH   RE,BCHALF                                                        
         SHI   R1,1                                                             
         MVC   0(0,R2),TIMEL                                                    
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    R2,R1                                                            
         B     ADDE06                                                           
*                                                                               
ADDE18   IC    R1,TIMLN            LENGTH OF ELEMENT                            
         LH    RE,BCHALF                                                        
         AR    RE,R1                                                            
         STH   RE,BCHALF                                                        
         SHI   R1,1                                                             
         MVC   0(0,R2),TIMEL                                                    
         EX    R1,*-6                                                           
         DROP  R6                                                               
ADDE20   L     RE,AIOBC                                                         
         SR    RF,RF                                                            
         ICM   RF,3,ACCORLEN(RE)   RF=CURRENT LENGTH OF RECORD                  
         AH    RF,BCHALF           +LENGTH OF ELEMENT CLUSTER                   
         CH    RF,=Y(L'BCIO-50)    IF NEW RECORD > 1950 BYTES                   
         BH    ADDE30              THEN DONT ADD IT TO THIS RECORD              
*                                  (MUST HAVE ROOM FOR F1 ELEMENT)              
         SR    R1,R1                                                            
         ICM   R1,3,BCSEQ#         R1=CURRENT SEQUENCE #                        
         LA    RF,BCELEM                                                        
ADDE22   CLI   0(RF),TIMELQ        INSERT SEQUENCE # IN X'8B' ELEMS             
         BNE   ADDE24                                                           
         STC   R1,TIMSEQ-TIMEL(RF)                                              
         SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     ADDE22                                                           
ADDE24   LA    R1,1(R1)            BUMP SEQUENCE NUMBER                         
         STCM  R1,3,BCSEQ#                                                      
*                                                                               
         L     RE,AIOBC            *** ATTATCH ELEMENT CLUSTER ***              
         SR    RF,RF                                                            
         ICM   RF,3,ACCORLEN(RE)                                                
*                                                                               
         SHI   RF,1                                                             
         AR    RE,RF               RE=A(AREA TO ATTATCH CLUSTER)                
         LA    R0,BCELEM           R0=A(8B ELEM CLUSTER) SOURCE                 
         LH    R1,BCHALF           R1=L'8B ELEMENT CLUSTER                      
         LH    RF,BCHALF           RF=L'8B ELEMENT CLUSTER                      
         MVCL  RE,R0               CLUSTER -> END OF RECORD                     
         L     RE,AIOBC            *** UPDATE RECORD LENGTH ***                 
         SR    RF,RF                                                            
         ICM   RF,3,ACCORLEN(RE)                                                
         AH    RF,BCHALF                                                        
         STCM  RF,3,ACCORLEN(RE)                                                
ADDE26   XC    BCELEM,BCELEM       YES - IGNORE CLUSTER OF ELEMENTS             
         XC    BCHALF,BCHALF                                                    
         LA    R2,BCELEM                                                        
         B     ADDE06              GET NEXT CLUSTER OF 8B'S                     
*                                                                               
* IF CLUSTER DIDNT FIT ON CURRENT RECORD THEN WRITE IT OUT                      
* AND BUILD A NEW RECORD WITH THE SUBREFERENCE # BUMPED BY 1.                   
*                                                                               
ADDE30   GOTO1 AWRITREC                                                         
         USING TIMRECD,R5                                                       
         L     R5,AIOBC                                                         
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'TIMKEY),TIMKEY                                          
         GOTO1 AXAIO,AIOBC         CLEAR RECORD I/O AREA                        
         MVC   TIMKEY,BIGKEY                                                    
         SR    R1,R1                                                            
         IC    R1,TIMKSBR          BUMP SEQUENCE NUMBER                         
         LA    R1,1(R1)                                                         
         STC   R1,TIMKSBR                                                       
         MVI   TIMRSTAT,TIMSFAPP   INDICATOR BYTES USED TO MARK                 
*        CLI   BCFLAG1,X'FF'       TEST SAVE RECORD                             
*        BNE   *+8                                                              
*        MVI   TIMRSTAT,TSSKRI1Q   USE SAVE KEY INDICATOR                       
         MVI   TIMRRI2,TIMKRI2Q    THESE RECORDS AS SPECIAL RECORDS             
         CLI   BCFLAG1,X'FF'       DEALING WITH SAVE RECORDS                    
         BE    ADDE32                                                           
         MVI   TIMRRI3,TIMKRI3Q    YET MAINTAIN THE LOOK OF REGULAR             
         MVI   TIMRRI4,TIMKRI4Q    TRANSACTION RECORDS                          
ADDE32   LH    R1,=Y(TIMRFST-TIMKEY)                                            
         LA    R1,1(R1)                                                         
         STCM  R1,3,TIMRLEN                                                     
*                                                                               
         OC    BCPIDNO,BCPIDNO     *** D8 PERSON ID ELEMENT ***                 
         BZ    ADDE20                                                           
         CLI   BCFLAG1,X'FF'       DEALING WITH SAVE RECORDS                    
         BE    ADDE20              YES - NO PID REQUIRED                        
         L     RE,AIOBC            *** BUILD TIME RECORD KEY ***                
         AH    RE,=Y(TIMRFST-TIMKEY)                                            
         USING PIDELD,RE                                                        
         MVI   PIDEL,PIDELQ        X'D8' ELEMENT                                
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,BCPIDNO       PERSON ID #                                  
         SR    R1,R1                                                            
         ICM   R1,3,TIMRLEN                                                     
         AH    R1,=Y(PIDLNQ)                                                    
         STCM  R1,3,TIMRLEN                                                     
         B     ADDE20                                                           
                                                                                
ADDEXSTX B     ROUTE                                                            
         DROP  RE,R5                                                            
         EJECT                                                                  
***********************************************************************         
* WRITE TIME/TIME SAVE RECORD IN AIOBC TO DISK & CREATE PASSIVE PTRS  *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMRECD,R6                                                       
WRITREC  L     R6,AIOBC            A(RECORD)                                    
*                                                                               
*                                  *** READ FOR ACTIVE KEY ***                  
         XC    BCBYTE2,BCBYTE2                                                  
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'TIMKEY),TIMKEY                                          
         OI    DMINBTS,X'08'       READ FOR DELETED RECORDS                     
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'TIMKEY),BIGKEY  IF RECORD FOUND THEN WRITE             
         BNE   WRIT200             NEW RECORD ELSE ADD A NEW ONE                
         LA    R6,BIGKEY                                                        
         NI    TIMKSTA,X'FF'-X'80' MARK ACTIVE KEY UNDELETED                    
         MVI   RDUPDATE,C'Y'       *** WRITE TIMESHEET RECORD BACK ***          
*        MVC   AIO,AIO2                                                         
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
WRIT100  MVC   AIO,AIOBC                                                        
         L     R6,AIO                                                           
         MVI   BCBYTE2,0                                                        
         MVI   ELCODE,TIMELQ       X'8B' ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    WRIT150                                                          
         L     R6,AIO                                                           
         OI    TIMRSTA,X'80'       MARK ACCMST RECORD DELETED                   
         LA    R6,BIGKEY                                                        
         OI    TIMKSTA,X'80'       MARK ACCDIR RECORD DELETED                   
         MVI   BCBYTE2,X'80'                                                    
*                                                                               
         USING TIMRECD,R6                                                       
WRIT150  L     R6,AIOBC                                                         
         GOTO1 AMOAFIND                                                         
         MVC   TIMRLMOS,BCSTDTE                                                 
         MVC   TIMRHMOS,BCENDTE                                                 
         MVC   BCKEYSAV,0(R6)      SAVE ACTIVE POINTER KEY                      
         GOTO1 PUTREC              WRITE ACCMST RECORD BACK                     
         LA    R6,BIGKEY                                                        
         MVC   TIMKLMOS,BCSTDTE                                                 
         MVC   TIMKHMOS,BCENDTE                                                 
         GOTO1 WRITE               WRITE ACCDIR ACTIVE POINTER                  
         B     WRIT300                                                          
*                                                                               
WRIT200  MVC   AIO,AIOBC           *** ADD NEW RECORD ***                       
         L     R6,AIO                                                           
         MVI   ELCODE,TIMELQ       X'8B' ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   WRITX                                                            
*                                                                               
         USING TIMRECD,R6                                                       
         L     R6,AIO              RESET R6 TO START OF AIO                     
         GOTO1 AMOAFIND                                                         
         MVC   TIMRLMOS,BCSTDTE                                                 
         MVC   TIMRHMOS,BCENDTE                                                 
         MVC   BCKEYSAV,0(R6)      SAVE ACTIVE POINTER KEY                      
         GOTO1 ADDREC              ADD ACCMST RECORD                            
         B     WRIT300                                                          
*                                  (RE-)ADD ALL PASSIVE POINTERS                
         USING CPTRBLK,BCCPTRBK                                                 
WRIT300  XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   LDGLVALN(L'BC1RLNQS),BC1RLNQS   SET 1R LEVLENS FOR CPTR          
         MVC   CPYSTA1(7),BCCPYST1             SET CPY STATUS                   
         L     RF,AIOBC            PASS A(DATA REC)                             
         GOTO1 APADDLE,BCDMCB,(C'A',(RF)),CPTRBLK,DMDSKADD,0,ACOMFACS           
*                                                                               
WRIT320  L     R6,AIOBC            GENCON DESTROYS KEY IN AIOBC AFTER           
         MVC   TIMKEY,BCKEYSAV     WRITING BACK THE PASSIVE POINTERS            
         XC    BCKEYSAV,BCKEYSAV   SO WE MUST RESTORE ORIGINAL KEY              
         B     WRITX               TO AVOID MAJOR PROBLEMS                      
*                                                                               
WRITX    NI    DMINBTS,X'FF'-X'08' TURN OFF READ FOR DELETES                    
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD PASSIVE POINTER KEYS                                          *         
* NO LONGER USED                                                      *         
***********************************************************************         
         SPACE 1                                                                
PASSIVE  DS    0H                                                               
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* NTRY - BCACCODE = 1R ACCOUNT (C/U/L/A)                              *         
*      - BCYYMMDD = WEEK ENDING DATE                                  *         
*                                                                     *         
* EXIT - ALL RECORDS/PASSIVE PTRS NOT IN 1C CONTRA TABLE ARE DELETED  *         
***********************************************************************         
         SPACE 1                                                                
DELETE   DS    0H                                                               
         MVC   AIO,AIOBC                                                        
*                                                                               
         USING TSWRECD,R6          READ TIMESHEET WEEKLY PASSIVE PTR            
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ    X'3E'                                        
         MVI   TSWKSUB,TSWKSUBQ    X'0F'                                        
         MVC   TSWKCPY,BCACKCPY    COMPANY CODE                                 
         LA    RF,BCACKACT                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SHI   R1,1                                                             
         MVC   TSWKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         EX    R1,*-6                                                           
         OC    TSWKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SHI   R1,1                                                             
         MVC   TSWKPER(0),0(RF)    ISOLATE PERSON CODE                          
         EX    R1,*-6                                                           
         OC    TSWKPER,BCSPACES                                                 
         ICM   R1,7,BCYYMMDD                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSWKEND        WEEK ENDING DATE                             
         GOTO1 HIGH                                                             
*                                                                               
DEL100   LA    R6,BIGKEY                                                        
         CLC   TSWKEY(TSWKODS+L'TSWKODS-TSWKEY),KEYSAVE                         
         BNE   DELX                                                             
         MVC   BCPASPTR,BIGKEY                                                  
*                                                                               
         USING CSTTABD,R2                                                       
         LA    R2,LWS1CTAB         R2 = A(1C COSTING ACCOUNT TABLE)             
         SR    R3,R3                                                            
         ICM   R3,3,LWS1CNUM       R3 = # TABLE ENTRIES/# TIMES LOOP            
         BZ    DEL200                                                           
*                                                                               
DEL120   CLC   CSTOFFC,TSWKCOFC    SAME OFFICE                                  
         BNE   DEL150                                                           
         CLC   CSTCNTRA,TSWKULC    SAME CONTRA ACCOUNT                          
         BNE   DEL150                                                           
DEL130   GOTO1 SEQ                                                              
         B     DEL100                                                           
DEL150   LA    R2,CSTLNQ(R2)                                                    
         BCT   R3,DEL120                                                        
*                                                                               
DEL200   MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         USING TIMRECD,R6                                                       
         L     R6,AIOBC                                                         
*                                  DELETE EXISTING PASSIVES                     
         USING CPTRBLK,BCCPTRBK                                                 
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   LDGLVALN(L'BC1RLNQS),BC1RLNQS   SET 1R LEVLENS FOR CPTR          
         MVC   CPYSTA1(7),BCCPYST1             SET CPY STATUS                   
         GOTO1 APADDLE,BCDMCB,(C'D',(R6)),(C'K',CPTRBLK),0,0,ACOMFACS           
                                                                                
         TM    BCPRIND,BCPRIDTM    ARE WE DEALING WITH DAILY TIME               
         BZ    DEL230              NO                                           
         SR    R1,R1               YES - NEED TO READ ELEMENTS ON REC           
         LA    R5,TIMRFST                                                       
         USING TIMELD,R5                                                        
DEL202   CLI   TIMEL,0                                                          
         BE    DEL212                                                           
         CLI   TIMEL,TIMELQ                                                     
         BE    DEL206                                                           
DEL204   IC    R1,TIMLN                                                         
         AR    R5,R1                                                            
         B     DEL202                                                           
*                                                                               
DEL206   CLI   TIMETYP,TIMEINP     INPUT DETAIL ELEMENT                         
         BNE   DEL208              NO                                           
         ST    R5,FULL             YES - START OF BLOCK OF ELEMENTS             
         B     DEL204              GET NEXT ELEMENT                             
*                                                                               
DEL208   CLI   TIMETYP,TIMEXTRA    TEMPO EXTRA ELEMENT                          
         BNE   DEL204              NO - GET NEXT ELEMENT                        
         CLC   BCINPDTE,TIMXTDDT   DOES DATE MATCH                              
         BNE   DEL204              NO                                           
DEL      USING TIMEL,R4                                                         
         L     R4,FULL             YES - DELETE ELEMENTS IN THIS GROUP          
DEL210   CR    R4,R5               HAVE WE GONE PAST CURRENT ELEM               
         BH    DEL204              YES - GO TO NEXT BLOCK                       
         MVI   DEL.TIMEL,X'FF'     MARK ELEMENT FOR DELETION                    
         IC    R1,DEL.TIMLN        BUMP TO NEXT ELEMENT                         
         AR    R4,R1                                                            
         B     DEL210                                                           
         DROP  DEL                                                              
*                                                                               
DEL212   MVI   ELCODE,X'FF'        DELETE ALL 8B ELS MARKED AS FF               
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TIMELQ       X'8B' ELEMENT                                
         BAS   RE,GETEL            HAVE WE GOT ANY ELEMENTS LEFT                
         BNE   DEL220              NO - DELETE RECORD                           
*                                  RE-ADD PASSIVES FOR REMAINING ELS            
         USING CPTRBLK,BCCPTRBK                                                 
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   LDGLVALN(L'BC1RLNQS),BC1RLNQS   SET 1R LEVLENS FOR CPTR          
         MVC   CPYSTA1(7),BCCPYST1             SET CPY STATUS                   
         L     RF,AIOBC                                                         
         GOTO1 APADDLE,BCDMCB,(C'A',(RF)),CPTRBLK,DMDSKADD,0,ACOMFACS           
                                                                                
         XC    BIGKEY,BIGKEY       IGNORE RECORD AND GET NEXT                   
         MVC   BIGKEY(L'TSWKEY),BCPASPTR  RESET READ SEQUENCE                   
         GOTO1 HIGH                WILL GET SAME RECORD                         
         B     DEL130              GET SEQUENTIAL RECORD                        
*                                                                               
DEL220   L     R6,AIOBC                                                         
DEL230   MVC   BCKEYSAV,0(R6)      SAVE ACTIVE KEY FROM MST RECORD              
         OI    TIMRSTA,X'80'       MARK ACCMST RECORD DELETED                   
         GOTO1 PUTREC              WRITE ACCMST RECORD BACK                     
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'TIMKEY),BCKEYSAV                                        
*        OI    DMINBTS,X'08'       READ FOR DELETED RECORDS                     
         GOTO1 HIGH                READ FOR ACTIVE POINTER                      
         CLC   BIGKEY(L'TIMKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*        TM    BIGKEY+(TIMKSTA-TIMKEY),X'80'    IS IT ALREADY DELETED?          
*        BNO   *+8                                                              
*        B     DEL250                                                           
         OI    BIGKEY+(TIMKSTA-TIMKEY),X'80'    MARK ACTIVE DELETED             
         GOTO1 WRITE               WRITE ACCDIR ACTIVE POINTER                  
*                                                                               
DEL250   XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'TSWKEY),BCPASPTR        RESET READ SEQUENCE             
         GOTO1 HIGH                WILL GET NEXT RECORD-JUST DELETED            
         B     DEL100              THIS KEY                                     
*                                                                               
DELX     MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* NTRY - CURRENT TIME RECORD IN AIOBC                                 *         
*                                                                     *         
* EXIT - ALL NON-USED SEQUENTIAL SUB-REFERENCE NUMBERS DELETED        *         
***********************************************************************         
         SPACE 1                                                                
DELSEQ   DS    0H                                                               
         USING TIMRECD,R6                                                       
         L     R6,AIOBC                                                         
         USING TSWRECD,R5          R5=A(PASSIVE TIME KEY)                       
         LA    R5,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ    X'3E'                                        
         MVI   TSWKSUB,TSWKSUBQ    X'0F'                                        
         MVC   TSWKCPY,TIMKCPY     COMPANY CODE                                 
         LA    RF,TIMKACT                                                       
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SHI   R1,1                                                             
         MVC   TSWKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         EX    R1,*-6                                                           
         OC    TSWKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SHI   R1,1                                                             
         MVC   TSWKPER(0),0(RF)    ISOLATE PERSON CODE                          
         EX    R1,*-6                                                           
         OC    TSWKPER,BCSPACES                                                 
         ICM   R1,7,TIMKPEDT                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSWKEND        WEEK ENDING DATE                             
         MVC   TSWKULC,TIMKULC     CONTRA ACCOUNT                               
         MVC   TSWKCOFC,TIMKOFF    CLIENT OFFICE                                
         SR    R1,R1                                                            
         IC    R1,TIMKSBR                                                       
         LA    R1,1(R1)                                                         
         STC   R1,TSWKSBR                                                       
         GOTO1 HIGH                                                             
*                                                                               
         CLC   TSWKEY(TSWKSBR-TSWKEY),KEYSAVE                                   
         BNE   DELSEQX                                                          
*                                                                               
         MVI   RDUPDATE,C'Y'       TURN ON  READ FOR UPDATE                     
         GOTO1 GETREC                                                           
         USING TIMRECD,R6                                                       
         L     R6,AIOBC                                                         
*                                  DELETE EXISTING PASSIVES                     
         USING CPTRBLK,BCCPTRBK                                                 
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   LDGLVALN(L'BC1RLNQS),BC1RLNQS   SET 1R LEVLENS FOR CPTR          
         MVC   CPYSTA1(7),BCCPYST1             SET CPY STATUS                   
         GOTO1 APADDLE,BCDMCB,(C'D',(R6)),(C'K',CPTRBLK),0,0,ACOMFACS           
*                                                                               
         TM    BCPRIND,BCPRIDTM    ARE WE DEALING WITH DAILY TIME               
         BZ    DELS014             NO                                           
         SR    R1,R1               YES - NEED TO READ ELEMENTS ON REC           
         LA    R5,TIMRFST                                                       
         USING TIMELD,R5                                                        
DELS002  CLI   TIMEL,0                                                          
         BE    DELS012                                                          
         CLI   TIMEL,TIMELQ                                                     
         BE    DELS006                                                          
DELS004  IC    R1,TIMLN                                                         
         AR    R5,R1                                                            
         B     DELS002                                                          
*                                                                               
DELS006  CLI   TIMETYP,TIMEINP     INPUT DETAIL ELEMENT                         
         BNE   DELS008             NO                                           
         ST    R5,FULL             YES - START OF BLOCK OF ELEMENTS             
         B     DELS004             GET NEXT ELEMENT                             
*                                                                               
DELS008  CLI   TIMETYP,TIMEXTRA    TEMPO EXTRA ELEMENT                          
         BNE   DELS004             NO - GET NEXT ELEMENT                        
         CLC   BCINPDTE,TIMXTDDT   DOES DATE MATCH                              
         BNE   DELS004             NO                                           
DEL      USING TIMEL,R4                                                         
         L     R4,FULL             YES - DELETE ELEMENTS IN THIS GROUP          
DELS010  CR    R4,R5               HAVE WE GONE PAST CURRENT ELEM               
         BH    DELS004             YES - GO TO NEXT BLOCK                       
         MVI   DEL.TIMEL,X'FF'     MARK ELEMENT FOR DELETION                    
         IC    R1,DEL.TIMLN        BUMP TO NEXT ELEMENT                         
         AR    R4,R1                                                            
         B     DELS010                                                          
         DROP  DEL                                                              
*                                                                               
DELS012  MVI   ELCODE,X'FF'        DELETE ALL 8B ELS MARKED AS FF               
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TIMELQ       X'8B' ELEMENT                                
         BAS   RE,GETEL            HAVE WE GOT ANY ELEMENTS LEFT                
         BNE   DELS014             NO                                           
*                                  RE-ADD PASSIVES FOR REMAINING ELS            
         USING CPTRBLK,BCCPTRBK                                                 
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   LDGLVALN(L'BC1RLNQS),BC1RLNQS   SET 1R LEVLENS FOR CPTR          
         MVC   CPYSTA1(7),BCCPYST1             SET CPY STATUS                   
         L     RF,AIOBC                                                         
         GOTO1 APADDLE,BCDMCB,(C'A',(RF)),CPTRBLK,DMDSKADD,0,ACOMFACS           
                                                                                
         B     DELSEQ              DON'T DELETE RECORD                          
*                                                                               
DELS014  L     R6,AIOBC            REFRESH R6                                   
         MVC   BCKEYSAV,0(R6)                                                   
         OI    TIMRSTA,X'80'       MARK ACCMST RECORD DELETED                   
         GOTO1 PUTREC              WRITE ACCMST RECORD BACK                     
         B     DELSEQ              BUMP SEQUENCE AND CHECK NEXT RECORD          
*                                                                               
DELSEQX  MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* MARK THE ACCOUNT STATUS ELEMENT WITH TIMESHEET DATE                 *         
***********************************************************************         
         SPACE 1                                                                
MRKAST   DS    0H                                                               
         MVC   BCFULL,AIO                                                       
*                                                                               
         USING CHDRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         MVC   BIGKEY,BCSPACES                                                  
         MVC   CHDKCPY,CMPY        COMPANY CODE                                 
         MVC   CHDKULA,TRKCNTRA    READ FOR CONTRA ACCOUNT                      
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'CHDKEY),KEYSAVE      SAME KEY?                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVI   RDUPDATE,C'Y'       READ RECORD FOR UPDATE                       
         GOTO1 AXAIO,AIOBC         CLEAR RECORD I/O AREA                        
         MVC   AIO,AIOBC                                                        
         GOTO1 GETREC                                                           
*                                                                               
         USING ASTELD,R6                                                        
         L     R6,AIO                                                           
         AH    R6,=Y(ACTRFST-ACTRECD)                                           
MRKA10   CLI   0(R6),0                                                          
         BNE   MRKA20                                                           
*                                                                               
         LA    R6,ELEM                   ADD STATUS ELEMENT TO RECORD           
         XC    ASTELD(ASTLN1Q),ASTELD                                           
         MVI   ASTEL,ASTELQ                                                     
         MVI   ASTLN,ASTLN1Q                                                    
         GOTO1 DATCON,BCDMCB,(1,TRKPEDT),(2,ASTPEDT)                            
         GOTO1 HELLO,BCDMCB,(C'P',=C'ACCMST  '),AIO,ELEM,0                      
         CLI   12(R1),0                                                         
         BE    MRKA45                                                           
         DC    H'0'                                                             
*                                                                               
MRKA20   CLI   0(R6),ASTELQ        X'31' - ACCOUNT STATUS ELEMENT               
         BE    MRKA30                                                           
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     MRKA10                                                           
*                                                                               
MRKA30   OC    ASTPEDT,ASTPEDT     DO WE ALREAD HAVE A DATE?                    
         BZ    MRKA40                                                           
         GOTO1 DATCON,BCDMCB,(2,ASTPEDT),(1,BCWORK)                             
         CLC   TRKPEDT,BCWORK      ONLY ADD TO RECORD IF LATER                  
         BNH   MRKAX                                                            
*                                                                               
MRKA40   GOTO1 DATCON,BCDMCB,(1,TRKPEDT),(2,ASTPEDT)                            
MRKA45   GOTO1 PUTREC              WRITE ACCMST RECORD BACK                     
*                                                                               
MRKAX    MVC   AIO,BCFULL          RESET AIO                                    
         MVI   RDUPDATE,C'N'       TURN OFF UPDATE FLAG                         
         B     ROUTE                                                            
         EJECT                                                                  
         DROP  R6                                                               
***********************************************************************         
* UPDATE RECORD OF SAVED ITEMS                                        *         
*                                                                     *         
* NTRY - BACCODE  = 1R ACCCOUNT (C/U/L/A)                             *         
*      - BCYYMMDD = WEEK ENDING DATE                                  *         
*                                                                     *         
* EXIT - RECORDS ON FILE ARE UPDATED                                  *         
***********************************************************************         
         SPACE 1                                                                
SAVTSAR  DS    0H                                                               
         MVC   BCFULL,AIO                                                       
         XC    BCFLAG4,BCFLAG4                                                  
*                                                                               
         USING TSSRECD,R6                                                       
         L     R6,AIOBC            *** BUILD TIME SAVE RECORD ***               
         GOTO1 AXAIO,AIOBC         CLEAR RECORD I/O AREA                        
         MVI   TSSKTYP,TSSKTYPQ    X'3E'                                        
         MVI   TSSKSUB,TSSKSUBQ    X'11'                                        
         MVC   TSSKCPY,CMPY        COMPANY                                      
         LA    RF,BCACKACT                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SHI   R1,1                                                             
         MVC   TSSKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         EX    R1,*-6                                                           
         OC    TSSKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SHI   R1,1                                                             
         MVC   TSSKPER(0),0(RF)    ISOLATE PERSON CODE                          
         EX    R1,*-6                                                           
         OC    TSSKPER,BCSPACES                                                 
         ICM   R1,7,BCYYMMDD                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSSKEND        WEEK ENDING DATE                             
         MVI   TSSRRI1,TIMSFAPP    INDICATOR BYTES USED TO MARK                 
         MVI   TSSRRI2,TIMKRI2Q                                                 
         MVI   TSSRRI3,TIMKRI3Q                                                 
         MVI   TSSRRI4,TIMKRI4Q                                                 
         LH    R1,=Y(TSSRFST-TSSKEY)                                            
         LA    R1,1(R1)                                                         
         STCM  R1,3,TSSRLEN                                                     
*                                                                               
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         L     R4,AIO1                                                          
         LA    R3,TSARDH                                                        
SAV100   GOTO1 AXAIO,AIO1                                                       
         GOTO1 ATSAR,BCDMCB,(R3),1                                              
         LA    R3,TSANXT                                                        
         TM    BCTSERRS,TSEEOF                                                  
         BO    SAV200                                                           
         TM    TRKSTAT,TRKSDEL+TRKSUPDT SKIP DELETED/UPDATED ITEMS              
         BNZ   SAV100                                                           
         TM    TRKSTAT,TRKSSAVE    ONLY ADD SAVED ITEMS                         
         BNO   SAV100                                                           
         MVI   BCFLAG4,X'FF'       ADD/WRITE PENDING                            
         TM    BCPRIND,BCPRIDTM    ARE WE USING DAILY TIME                      
         BZ    SAV150              NO - DON'T NEED EXISTING ELEMENTS            
         GOTO1 AADDEXST,1          GET EXISTING ELEMENTS FOR OTHER DAYS         
*                                                                               
SAV150   XC    CMCHKLN,CMCHKLN     INITIALISE SAVED L'CLUSTER CHUNK             
         GOTO1 AADDCLST,(R4)       ADD CLUSTER TO END OF RECORD                 
         BNE   SAV180              DIDN'T FIT - CREATE NEW RECORD               
         OC    CMCHKLN,CMCHKLN     ELSE TEST CLUSTER WAS SPLIT                  
         BZ    SAV100              NO - GET NEXT                                
*                                                                               
* IF CLUSTER DIDNT FIT ON CURRENT RECORD THEN WRITE IT OUT                      
* AND BUILD A NEW RECORD WITH THE SUBREFERENCE # BUMPED BY 1.                   
*                                                                               
SAV180   GOTO1 AWRITREC                                                         
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY,TSSKEY                                                    
         GOTO1 AXAIO,AIOBC         CLEAR RECORD I/O AREA                        
         MVC   TSSKEY(L'BIGKEY),BIGKEY                                          
         SR    R1,R1                                                            
         IC    R1,TSSKSBR          BUMP SEQUENCE NUMBER                         
         LA    R1,1(R1)                                                         
         STC   R1,TSSKSBR                                                       
         LH    R1,=Y(TSSRFST-TSSKEY)                                            
         LA    R1,1(R1)                                                         
         STCM  R1,3,TSSRLEN                                                     
         GOTO1 AADDCLST,(R4)       ADD CLUSTER TO A BRAND NEW RECORD            
         BE    *+6                                                              
         DC    H'0'                SOMETHING SCREWED UP IF DIED HERE!!          
         OC    CMCHKLN,CMCHKLN     TEST ALL OF CLUSTER NOW ADDED                
         BNZ   SAV180              NO - GO ROUND AGAIN                          
         B     SAV100              YES - GET NEXT                               
*                                                                               
SAV200   CLI   BCFLAG4,X'FF'       ADD/WRITE PENDING                            
         BNE   SAV300                                                           
         GOTO1 AWRITREC                                                         
*                                                                               
SAV300   MVC   AIO,BCFULL                                                       
         B     ROUTE                                                            
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE SAVED RECORDS                                                *         
***********************************************************************         
         SPACE 1                                                                
DSAVE    DS    0H                                                               
         USING TSSRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   TSSKTYP,TSSKTYPQ    X'3E'                                        
         MVI   TSSKSUB,TSSKSUBQ    X'11'                                        
         MVC   TSSKCPY,CMPY        COMPANY                                      
         LA    RF,BCACKACT                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SHI   R1,1                                                             
         MVC   TSSKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         EX    R1,*-6                                                           
         OC    TSSKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SHI   R1,1                                                             
         MVC   TSSKPER(0),0(RF)    ISOLATE PERSON CODE                          
         EX    R1,*-6                                                           
         OC    TSSKPER,BCSPACES                                                 
         ICM   R1,7,BCYYMMDD                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSSKEND        WEEK ENDING DATE                             
         GOTO1 HIGH                                                             
*                                                                               
DSAVE10  CLC   KEYSAVE(TSSKSBR-TSSKEY),BIGKEY                                   
         BNE   DSAVEX                                                           
         MVI   RDUPDATE,C'Y'       *** DELETE RECORD ***                        
         GOTO1 GETREC                                                           
         L     R6,AIOBC                                                         
         TM    BCPRIND,BCPRIDTM    ARE WE DEALING WITH DAILY TIME               
         BZ    DSAVE50             NO                                           
         SR    R1,R1               YES - NEED TO READ ELEMENTS ON REC           
         LA    R5,TSSRFST                                                       
         USING TIMELD,R5                                                        
DSAVE20  CLI   TIMEL,0                                                          
         BE    DSAVE45                                                          
         CLI   TIMEL,TIMELQ                                                     
         BE    DSAVE30                                                          
DSAVE25  IC    R1,TIMLN                                                         
         AR    R5,R1                                                            
         B     DSAVE20                                                          
*                                                                               
DSAVE30  CLI   TIMETYP,TIMEINP     INPUT DETAIL ELEMENT                         
         BNE   DSAVE35             NO                                           
         ST    R5,FULL             YES - START OF BLOCK OF ELEMENTS             
         B     DSAVE25             GET NEXT ELEMENT                             
*                                                                               
DSAVE35  CLI   TIMETYP,TIMEXTRA    TEMPO EXTRA ELEMENT                          
         BNE   DSAVE25             NO - GET NEXT ELEMENT                        
         CLC   BCINPDTE,TIMXTDDT   DOES DATE MATCH                              
         BNE   DSAVE25             NO                                           
DEL      USING TIMEL,R4                                                         
         L     R4,FULL             YES - DELETE ELEMENTS IN THIS GROUP          
DSAVE40  CR    R4,R5               HAVE WE GONE PAST CURRENT ELEM               
         BH    DSAVE25             YES - GO TO NEXT BLOCK                       
         MVI   DEL.TIMEL,X'FF'     MARK ELEMENT FOR DELETION                    
         IC    R1,DEL.TIMLN        BUMP TO NEXT ELEMENT                         
         AR    R4,R1                                                            
         B     DSAVE40                                                          
         DROP  DEL                                                              
*                                                                               
DSAVE45  MVI   ELCODE,X'FF'        DELETE ALL 8B ELS MARKED AS FF               
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TIMELQ       X'8B' ELEMENT                                
         BAS   RE,GETEL            HAVE WE GOT ANY ELEMENTS LEFT                
         BE    DSAVE60             NO - DELETE RECORD                           
         L     R6,AIOBC                                                         
*                                                                               
DSAVE50  OI    TSSRSTA,X'80'       MARK DELETED                                 
         GOTO1 PUTREC              WRITE ACCMST RECORD BACK                     
         OI    BIGKEY+(TSSKSTA-TSSKEY),X'80'                                    
         GOTO1 WRITE               WRITE ACCDIR ACTIVE POINTER                  
DSAVE60  LA    R6,BIGKEY                                                        
         GOTO1 SEQ                                                              
         B     DSAVE10                                                          
*                                                                               
DSAVEX   B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* CREATE POSTING TABLE TO BE PASSED TO ACTIMETRN RELOCATABLE MODULE   *         
*                                                                     *         
* NTRY - BACCODE  = 1R ACCCOUNT (C/U/L/A)                             *         
*      - BCYYMMDD = WEEK ENDING DATE                                  *         
*                                                                     *         
* EXIT - CC=EQU - TMS BUFFER BUILT IN BCBUFF2                         *         
*        CC=NEQ - NO RECORDS IN TABLE                                 *         
***********************************************************************         
         SPACE 1                                                                
POSTBLD  DS    0H                                                               
*        MVC   BCFULL,AIO                                                       
*        XC    BCHALF,BCHALF                                                    
*                                                                               
*        L     R1,ABUFF2                                                        
*        AH    R1,=Y(BCBUFLNQ-1)                                                
*        ST    R1,BCDUB2           A(END OF BUFFER)                             
*        ST    R1,BCADDR                                                        
*                                                                               
*        L     RE,ABUFF2                                                        
*        SR    RF,RF                                                            
*        ICM   RF,3,=Y(BCBUFLNQ)                                                
*        SR    R0,R0                                                            
*        SR    R1,R1                                                            
*        MVCL  RE,R0               CLEAR BUFFER                                 
*                                                                               
         USING TSARRECD,R4                                                      
*        L     R4,AIO1             R4 = A(TSAR RECORD AREA)                     
         USING TMSD,R5                                                          
*        ICM   R5,15,ABUFF2        R5 = A(TMS BUFFER)                           
*        MVI   0(R5),X'FF'         MARK END OF TABLE                            
*                                                                               
*        LA    R3,TSARDH                                                        
*OST100  GOTO1 AXAIO,AIO1                                                       
*        GOTO1 ATSAR,BCDMCB,(R3),1                                              
*        LA    R3,TSANXT                                                        
*        TM    BCTSERRS,TSEEOF                                                  
*        BO    POSTX                                                            
*        TM    TRKSTAT,TRKSSAVE    SKIP SAVED ITEMS                             
*        BO    POST100                                                          
*                                                                               
*        SR    R1,R1               INCREMENT TABLE COUNT                        
*        ICM   R1,3,BCHALF                                                      
*        LA    R1,1(R1)                                                         
*        STCM  R1,3,BCHALF                                                      
*        MVC   TMSOFFC,TRKOFF      OFFICE CODE                                  
*        MVC   TMSCNTRA,TRKCNTRA   CONTRA ACCOUNT                               
*                                                                               
         USING TIMELD,R6                                                        
*        LA    R6,TRDATA           R6=A(CURRENT X'8B' DETAIL ITEM)              
*OST200  CLI   TIMEL,TIMELQ                                                     
*        BNE   POST300             NOT X'8B' ELEMENT                            
*                                                                               
*        CLI   TIMETYP,TIMEINP     INPUT DETAIL ITEM                            
*        BNE   POST225                                                          
*        MVC   TMSACC,TIMACC       SJ OR 1N ACCOUNT                             
*        MVC   TMSTSK,TIMTSK       TASK                                         
*        MVC   TMSTTYP,TIMTTYP     TYPE OF TIME                                 
*        MVC   TMSLINE#,TIMLINE#   ITEM LINE #                                  
*        MVC   TMSIND,TIMIND       TIME INDICATOR                               
*        MVC   TMSMOA,TIMMOA       MONTH OF ACTIVITY                            
*        MVC   TMSSTAT,TIMSTAT     STATUS                                       
*        ZAP   TMSHRS,TIMHRS       HOURS                                        
*        ZAP   TMSRATE,=P'0'       SALES RATE                                   
*&&UK*&& ZAP   TMSCRATE,=P'0'      COST RATE                                    
*&&UK*&& ZAP   TMSERTE,=P'0'       EURO RATE                                    
*&&UK*&& ZAP   TMSECRTE,=P'0'      EURO COST RATE                               
*        CLI   TIMLN,TIMILN2Q                                                   
*        BL    POST275                                                          
*        ZAP   TMSRATE,TIMRATE     RATE                                         
*        MVC   TMSRSTA,TIMRBSTA    BILLABLE TIME STATUS                         
*        MVC   TMSREFF,TIMREFF     RATE EFFECTIVE DATE                          
*        MVC   TMSINC,TIMINC       INCOME ACCOUNT                               
*&&UK*&& ZAP   TMSCRATE,TIMCRATE   COST RATE                                    
*&&UK*&& MVC   TMSCREFF,TIMCREFF   COST RATE EFFECTIVE DATE                     
*&&UK*&& ZAP   TMSERTE,TIMERTE     EURO RATE                                    
*&&UK*&& ZAP   TMSECRTE,TIMECRTE   EURO COST RATE                               
*        B     POST275                                                          
*                                                                               
*OST225  CLI   TIMETYP,TIMETAX     TAX DETAIL ITEM                              
*        BE    *+12                                                             
*        CLI   TIMETYP,TIMENAR     NARRATIVE ELEMENT                            
*        BNE   POST275                                                          
*        L     RE,BCADDR           A(NEXT AREA)                                 
*        SR    R1,R1                                                            
*        IC    R1,TIMLN                                                         
*        SR    RE,R1                                                            
*        SHI   R1,1                                                             
*        MVC   0(0,RE),TIMEL       COPY ELEMENT                                 
*        EX    R1,*-6                                                           
*        ST    RE,BCADDR                                                        
*        LA    RF,TMSATAX                                                       
*        CLI   TIMETYP,TIMETAX                                                  
*        BE    *+8                                                              
*        LA    RF,TMSANARR                                                      
*        STCM  RE,15,0(RF)                                                      
*                                                                               
*OST275  SR    R1,R1               BUMP TO NEXT ELEMENT IN CLUSTER              
*        IC    R1,1(R6)                                                         
*        AR    R6,R1                                                            
*        B     POST200                                                          
*                                                                               
*OST300  LA    R5,TMSLNQ(R5)       BUMP TO NEXT TABLE ENTRY                     
*        MVI   0(R5),X'FF'                                                      
*        L     R1,BCADDR           A(END OF BUFFER)                             
*        CR    R5,R1                                                            
*        BL    *+6                                                              
*        DC    H'0'                BUFFER TOO SMALL                             
*        B     POST100                                                          
*                                                                               
*OSTX    SR    R3,R3                                                            
*        ICM   R3,3,BCHALF         R3 = # ENTRIES IN 1C TABLE                   
*        BZ    POSTX10                                                          
*        GOTO1 AXSORT,DMCB,ABUFF2,(R3),TMSLNQ,TMSKSRTQ,0                        
*                                                                               
*OSTX10  MVC   AIO,BCFULL                                                       
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* SORT TSAR RECORDS BY TIMESHEET #                                    *         
*      SORT BUFFER IS PASTED IN PARM 1                                *         
***********************************************************************         
         SPACE 1                                                                
TSNSORT  DS    0H                                                               
         MVC   BCBYTE2,3(R1)       P1=TSAR BUFFER                               
         MVC   BCFULL,AIO          SAVE CALLERS A(AIO)                          
*                                                                               
         USING TSARD,R3            R3=A(TSAR BLOCK)                             
TSN100   LA    R3,TSRBLK1                                                       
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         L     R4,AIO1                                                          
         GOTO1 AXAIO,AIO1                                                       
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSARDH                                              
         TM    BCTSERRS,TSEEOF                                                  
         BO    TSN200                                                           
         CLC   TRKLINE,BCEFFS                                                   
         BE    TSN200                                                           
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSADEL      DELETE IT                               
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TIMELD,R6                                                        
         LA    R6,TRDATA           R6=A(X'8B' DETAIL ITEM)                      
         MVC   TRKLINE,BCEFFS      X'FF'S PUSH IT TO BACK OF LIST               
         MVC   TRKLNSUB,TIMLINE#   STORE NEW LINE #                             
         CLI   TIMETYP,TIMEFLD                                                  
         BNE   *+10                                                             
         MVC   TRKLNSUB,TIMFLINE   LINE #                                       
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSAADD      RE ADD TSAR RECORD WITH LINE #          
         BE    TSN100                                                           
         DC    H'0'                                                             
*                                                                               
*              INSERT NEW LINE #                                                
*                                                                               
         USING TSARD,R3            R3=A(TSAR BLOCK)                             
TSN200   LA    R3,TSRBLK1                                                       
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         L     R4,AIO1                                                          
         MVC   BCSEQ#,=H'1'                                                     
*                                                                               
TSN300   GOTO1 AXAIO,AIO1                                                       
         MVC   TRKLINE,BCEFFS                                                   
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSARDH                                              
         TM    BCTSERRS,TSEEOF                                                  
         BO    TSNX                                                             
         CLC   TRKLINE,BCEFFS                                                   
         BNE   TSNX                                                             
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSADEL                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRKLINE,BCSEQ#                                                   
         XC    TRKLNSUB,TRKLNSUB                                                
         XC    TRKLNREN,TRKLNREN                                                
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSAADD      RE ADD TSAR RECORD WITH LINE #          
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         ICM   R1,3,BCSEQ#                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,BCSEQ#                                                      
         B     TSN300                                                           
*                                                                               
TSNX     MVC   AIO,BCFULL          RESTORE AIO TO ORIGINAL VALUE                
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* SORT TSAR TABLE BY SJ CODE                                          *         
*      SORT BUFFER IS PASTED IN PARM 1                                *         
***********************************************************************         
         SPACE 1                                                                
SJSORT   DS    0H                                                               
         MVC   BCBYTE2,3(R1)       P1=TSAR BUFFER                               
         MVC   BCFULL,AIO          SAVE CALLERS A(AIO)                          
         MVC   BCSEQ#,=H'1'                                                     
*                                                                               
         USING TSARD,R3            R3=A(TSAR BLOCK)                             
         LA    R3,TSRBLK1                                                       
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         L     R4,AIO1                                                          
SJS100   GOTO1 AXAIO,AIO1                                                       
         MVC   TRKLINE,=H'1'                                                    
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSARDH                                              
         TM    BCTSERRS,TSEEOF                                                  
         BO    SJS200                                                           
         OC    TRKLINE,TRKLINE                                                  
         BZ    SJS200                                                           
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSADEL          DELETE IT                           
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    TRKLINE,TRKLINE                                                  
         XC    TRKLNSUB,TRKLNSUB                                                
         MVC   TRKLNREN,BCSEQ#     FORCE TO BE UNIQUE                           
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSAADD     RE ADD TSAR RECORD WITH LINE #           
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         ICM   R1,3,BCSEQ#                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,BCSEQ#                                                      
         B     SJS100                                                           
*                                                                               
*              INSERT NEW LINE #                                                
*                                                                               
         USING TSARD,R3            R3=A(TSAR BLOCK)                             
SJS200   LA    R3,TSRBLK1                                                       
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         L     R4,AIO1                                                          
         MVC   BCSEQ#,=H'1'                                                     
*                                                                               
SJS300   GOTO1 AXAIO,AIO1                                                       
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSARDH                                              
         TM    BCTSERRS,TSEEOF                                                  
         BO    SJSX                                                             
         OC    TRKLINE,TRKLINE                                                  
         BNZ   SJSX                                                             
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSADEL                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRKLINE,BCSEQ#                                                   
         XC    TRKLNSUB,TRKLNSUB                                                
         XC    TRKLNREN,TRKLNREN                                                
         LA    R1,BCDMCB                                                        
         XC    0(12,R1),0(R1)           CLEAR OUT THE 1ST 3 PARMS               
         MVC   7(1,R1),BCBYTE2          SET UP TSAR BUFFER # IN P2              
         GOTO1 ATSAR,BCDMCB,TSAADD     RE ADD TSAR RECORD WITH LINE #           
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         ICM   R1,3,BCSEQ#                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,BCSEQ#                                                      
         B     SJS300                                                           
*                                                                               
SJSX     MVC   AIO,BCFULL          RESTORE AIO TO ORIGINAL VALUE                
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE TIMESHEET/REVISION # ON 1R ACCOUNT RECORD                    *         
*                                                                     *         
* NTRY - BACCODE  = 1R ACCCOUNT (C/U/L/A)                             *         
*        PARM1    = A(ORIGINAL REVISION # FIELD)                      *         
*        PARM2    = A(NEW TIMESHEET # FIELD)                          *         
*                                                                     *         
* EXIT - BCLINE#  = NEXT AVAILABLE LINE #                             *         
*      - CC=NEQ   = NO RECORDS FOR THIS PERSON/PERIOD                 *         
***********************************************************************         
         SPACE 1                                                                
UPDTSN   DS    0H                                                               
         L     RF,0(R1)                                                         
         MVC   BCRVSN#,0(RF)       ORIGINAL REVISION NUMBER                     
         L     RF,4(R1)                                                         
         MVC   BCTSNUM,0(RF)       LAST TIMESHEET NUMBER                        
         MVC   BCFULL,AIO                                                       
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY           *** READ FOR ACCOUNT RECORD ***              
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,BCACCODE   1R ACCOUNT                                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'ACTKEY),BIGKEY                                         
         BE    *+6                                                              
         DC    H'0'                SOMETHING REALLY WRONG IF NOT FOUND          
*                                                                               
         MVI   RDUPDATE,C'Y'       READ RECORD FOR UPDATE                       
         GOTO1 AXAIO,AIOBC         CLEAR RECORD I/O AREA                        
         MVC   AIO,AIOBC                                                        
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTELD,R6                                                        
         MVI   ELCODE,RSTELQ       X'30' ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RSTLN,RSTLN2Q                                                    
         BNL   *+6                                                              
         DC    H'0'                CANT UPDATE SMALL ELEMENTS                   
*                                                                               
         CLC   RSTSNUM,BCRVSN#     FATAL ERROR IF NEQ                           
         BNE   ROUTH                                                            
         SR    R1,R1                                                            
         ICM   R1,3,BCTSNUM                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,RSTSNUM        SET NEXT TS #                                
*                                                                               
         GOTO1 PUTREC              WRITE ACCMST RECORD BACK                     
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* GET LATEST TIMESHEET/REVISION # ON 1R ACCOUNT RECORD                *         
*                                                                     *         
* NTRY   R1 = 0   = READ ACCOUNT RECORD                               *         
*        BACCODE  = 1R ACCCOUNT (C/U/L/A)                             *         
*                                                                     *         
*        R1 = 1   = AIO HAS A(ACCOUNT RECORD SET)                     *         
*                                                                     *         
* EXIT - BCTSNUM  = TIMESHEET #                                       *         
***********************************************************************         
         SPACE 1                                                                
GETTSN   DS    0H                                                               
         XC    BCTSNUM,BCTSNUM     CLEAR TIMESHEET/REVISION #                   
         MVC   BCFULL,AIO                                                       
         OR    R1,R1                                                            
         BNZ   GETTSN10                                                         
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY           *** READ FOR ACCOUNT RECORD ***              
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,BCACCODE   1R ACCOUNT                                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'ACTKEY),BIGKEY                                         
         BNE   ROUTH               SOMETHING WRONG IF NOT FOUND                 
         GOTO1 AXAIO,AIOBC         CLEAR RECORD I/O AREA                        
         MVC   AIO,AIOBC                                                        
         MVI   RDUPDATE,C'Y'       READ RECORD FOR UPDATE                       
         GOTO1 GETREC                                                           
*                                                                               
GETTSN10 L     R6,AIO                                                           
         USING RSTELD,R6                                                        
         MVI   ELCODE,RSTELQ       X'30' ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   ROUTH                                                            
         MVC   BCTSNUM,RSTSNUM                                                  
         OC    RSTSNUM,RSTSNUM     IS IT A NEW PERSON                           
         BNZ   GETTSN20            NO                                           
         MVC   BCTSNUM,=H'100'     YES - ADD 100 TO ENSURE TMS LINE             
         MVC   RSTSNUM,BCTSNUM     YES - ADD 100 TO ENSURE TMS LINE             
         GOTO1 PUTREC              WRITE ACCMST RECORD BACK                     
GETTSN20 MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         B     ROUTE               NUMBER IS UNIQUE TO TEMPO LINE NUM           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* INSURE THAT JOBS DONT GO NEGATIVE                                   *         
*                                                                     *         
* EXIT - CC=EQU - ALL JOBS ARE HAVE +HOURS                            *         
*      - CC=NEQ - JOB HAS NEGATIVE HOURS - DONT ALLOW UPDATE          *         
***********************************************************************         
         SPACE 1                                                                
NEGCHK   DS    0H                                                               
         MVC   BCFULL,AIO                                                       
         XC    LWS1CNUM,LWS1CNUM   CLEAR # TABLE ENTRIES                        
*                                                                               
         LA    RE,LWS1CTAB                                                      
         LH    RF,=Y(L'LWS1CTAB)                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         L     R4,AIO1                                                          
         LA    R3,TSARDH                                                        
NEG02    GOTO1 AXAIO,AIO1                                                       
         GOTO1 ATSAR,BCDMCB,(R3),1                                              
         TM    BCTSERRS,TSEEOF                                                  
         BO    NEG20                                                            
         TM    TRKSTAT,TRKSDEL+TRKSSAVE SKIP DELETED/SAVED ITEMS                
         BNZ   NEG15                                                            
*                                                                               
         USING TIMELD,R6                                                        
         LA    R6,TRDATA           R6=A(CURRENT X'8B' DETAIL ITEM)              
         CLI   TIMEL,TIMELQ                                                     
         BE    *+6                 NOT X'8B' ELEMENT                            
         DC    H'0'                                                             
         CLI   TIMETYP,TIMEINP     INPUT DETAIL ITEM                            
         BE    *+6                                                              
         DC    H'0'                                                             
*        CLI   TIMTTYP,TIMTNC                                                   
*        BE    NEG15               SKIP NON CLIENT TIME                         
*                                                                               
         USING NEGSJD,R2                                                        
         LA    R2,LWS1CTAB         R2=A(SJ -HRS TABLE)                          
         SR    R0,R0                                                            
         ICM   R0,3,LWS1CNUM       NUMBER OF ENTRIES                            
         BZ    NEG12                                                            
*                                                                               
NEG04    CLC   NEGSJACT,TIMACC     SJ ACCOUNT                                   
         BNE   NEG10                                                            
         CLC   NEGSJTSK,TIMTSK     TASK CODE                                    
         BE    NEG06                                                            
         CLC   NEGSJTSK,BCSPACES   DO WE CURRENTLY HAVE SPACES                  
         BNE   NEG10               NO                                           
         OC    TIMTSK,TIMTSK       YES - IS THE NEW ONE BLANK                   
         BNZ   NEG10               NO                                           
NEG06    CLC   NEGTTYPE,TIMTTYP    TYPE OF TIME (B/N/R)                         
         BNE   NEG10                                                            
*        TM    TIMIND,TIMIWO       IS IT A WRITE-OFF?                           
*        BO    NEG15               YES - DON'T ADD TO TABLE                     
         AP    NEGSJHRS,TIMHRS     ADD IN HOURS                                 
         B     NEG15                                                            
NEG10    LA    R2,NEGSJLNQ(R2)     NEXT TABLE ENTRY                             
         BCT   R0,NEG04                                                         
*                                                                               
NEG12    DS    0H                  ADD NEW TABLE ENTRY                          
         MVC   NEGSJACT,TIMACC     SJ ACCOUNT                                   
         MVC   NEGSJTSK,TIMTSK     SJ TASK CODE                                 
         OC    NEGSJTSK,NEGSJTSK                                                
         BNZ   *+10                                                             
         MVC   NEGSJTSK,BCSPACES                                                
         MVC   NEGTTYPE,TIMTTYP    TYPE OF TIME (B/N/R)                         
         ZAP   NEGSJHRS,TIMHRS     HOURS                                        
         SR    R1,R1                                                            
         ICM   R1,3,LWS1CNUM                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,LWS1CNUM       BUMP TABLE COUNT                             
         CH    R1,=Y(LWS1CMAX)                                                  
         BNH   *+6                                                              
         DC    H'0'                SJ TABLE FULL - #ITEMS>300                   
*                                                                               
NEG15    LA    R3,TSANXT                                                        
         B     NEG02                                                            
*                                                                               
NEG20    DS    0H                                                               
         USING NEGSJD,R2                                                        
         LA    R2,LWS1CTAB         R2=A(SJ -HRS TABLE)                          
         SR    R0,R0                                                            
         ICM   R0,3,LWS1CNUM       NUMBER OF ENTRIES                            
         BZ    ROUTE                                                            
NEG25    CP    NEGSJHRS,=P'0'                                                   
         BNL   NEG30                                                            
         MVC   BCWORK,BCSPACES                                                  
         MVC   BCWORK(12),NEGSJACT+2                                            
         LA    RF,BCWORK+12                                                     
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'/'                                                       
         MVC   2(L'NEGSJTSK,RF),NEGSJTSK                                        
         B     ROUTH               SET FOR ERROR MSG                            
*                                                                               
NEG30    LA    R2,NEGSJLNQ(R2)                                                  
         BCT   R0,NEG25                                                         
         B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FOR TSAR RECORDS WITH ERRORS                                  *         
***********************************************************************         
         SPACE 1                                                                
CHKINC   DS    0H                                                               
         MVC   BCFULL,AIO          SAVE A(USERS IO AREA)                        
         XC    BCHALF,BCHALF                                                    
         XC    BCHALF2,BCHALF2                                                  
*                                                                               
         USING TSARRECD,R4                                                      
         L     R4,AIO1                                                          
         LA    R3,TSARDH                                                        
CHKINC10 GOTO1 AXAIO,AIO1                                                       
         GOTO1 ATSAR,BCDMCB,(R3),1  CHECK TSAR RECS FOR INCOMP FLAG             
         TM    BCTSERRS,TSEEOF                                                  
         BO    CHKINCX                                                          
         LA    R2,TMSLNQ           R2 = LENGTH OF TMS TABLE ITEM                
*                                                                               
         USING TIMELD,R6                                                        
         LA    R6,TRDATA           R6=A(CURRENT X'8B' DETAIL ITEM)              
CHKINC12 CLI   0(R6),0             END OF RECORD                                
         BE    CHKINC16                                                         
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         CLI   TIMEL,TIMELQ                                                     
         BNE   CHKINC14            NOT X'8B' ELEMENT                            
         CLI   TIMETYP,TIMEINP     ONLY ADD L'TAX + L'NARRATIVE                 
         BE    CHKINC14                                                         
         AR    R2,R1               R2 = TOTAL LENGTH SO FAR                     
CHKINC14 AR    R6,R1               BUMP TO NEXT ELEMENT                         
         B     CHKINC12                                                         
*                                                                               
CHKINC16 AH    R2,BCHALF2          ADD TO TOTAL ACCUMULATOR                     
         STH   R2,BCHALF2                                                       
         LA    R3,TSANXT                                                        
         OC    TRKERROR,TRKERROR   ANY ERRORS                                   
         BZ    CHKINC10                                                         
         MVC   BCHALF,TRKERROR     RETURN ERROR CODE IN BCHALF                  
         B     ROUTH                                                            
*                                                                               
CHKINCX  CLC   BCHALF2,=Y(30000-100)    EXCEEDED MAX BUFFER SIZE                
         BH    ROUTL                                                            
         B     ROUTE                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* COUNT # SAVED ITEMS                                                 *         
*                                                                     *         
* EXIT - BCHALF = # ITEMS MARKED AS SAVED                             *         
***********************************************************************         
         SPACE 1                                                                
CNTSAVE  DS    0H                                                               
         MVC   BCFULL,AIO          SAVE A(USERS IO AREA)                        
         XC    BCHALF,BCHALF                                                    
*                                                                               
         USING TSARRECD,R4                                                      
         L     R4,AIO1                                                          
         LA    R3,TSARDH                                                        
CNTSAV10 GOTO1 AXAIO,AIO1                                                       
         GOTO1 ATSAR,BCDMCB,(R3),1  CHECK TSAR RECS FOR INCOMP FLAG             
         TM    BCTSERRS,TSEEOF                                                  
         BO    CNTSAVX                                                          
         LA    R3,TSANXT                                                        
         TM    TRKSTAT,TRKSSAVE                                                 
         BNO   CNTSAV10                                                         
         SR    R1,R1                                                            
         ICM   R1,3,BCHALF                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,BCHALF         BCHALF = # SAVED ITEMS                       
         B     CNTSAV10                                                         
*                                                                               
CNTSAVX  OC    BCHALF,BCHALF                                                    
         BZ    ROUTE                                                            
         B     ROUTH                                                            
         EJECT                                                                  
***********************************************************************         
* MARK TSAR ITEMS AS SAVED                                            *         
***********************************************************************         
         SPACE 1                                                                
MRKTSAR  DS    0H                                                               
         MVC   BCFULL,AIO          SAVE A(USERS IO AREA)                        
*                                                                               
         USING TSARRECD,R4                                                      
         L     R4,AIO1                                                          
         LA    R3,TSARDH                                                        
MRK100   GOTO1 AXAIO,AIO1                                                       
         GOTO1 ATSAR,BCDMCB,(R3),1  CHECK TSAR RECS FOR INCOMP FLAG             
         LA    R3,TSANXT                                                        
         TM    BCTSERRS,TSEEOF                                                  
         BO    MRKX                                                             
         TM    TRKSTAT,TRKSUPDT    MARK EVERYTHING EXCEPT UPDTD ITEMS           
         BO    MRK100              AS SAVED                                     
         OI    TRKSTAT,TRKSSAVE                                                 
         GOTO1 ATSAR,BCDMCB,TSAWRT,1                                            
         B     MRK100                                                           
*                                                                               
MRKX     B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* MAINTAIN TEMPO CROSS REFERENCE RECORD                               *         
*                                                                     *         
* NTRY - BACCODE  = 1R ACCCOUNT (C/U/L/A)                             *         
*      - BCYYMMDD = WEEK ENDING DATE                                  *         
*      - BCELEM   = TIMEL TEMPO X-REF ELEMENT                         *         
*                                                                     *         
* EXIT - RECORD ON FILE IS UPDATED                                    *         
***********************************************************************         
         SPACE 1                                                                
TEMPO    DS    0H                                                               
         MVC   BCFULL,AIO                                                       
         XC    BCFLAG4,BCFLAG4                                                  
*                                                                               
         USING TSARRECD,R4         R4=A(TSAR ITEM)                              
         L     R4,AIO1                                                          
         GOTO1 AXAIO,AIO1                                                       
         GOTO1 ATSAR,BCDMCB,TSARDH,1                                            
         TM    BCTSERRS,TSEEOF     DELETE ANY RECORDS                           
         BNO   *+12                  IF THERE ARE NO MORE TZAR RECORDS          
         MVI   BCFLAG4,BCFL4DEL    MUST DELETE RECORD                           
         B     TEMP10                                                           
         CLI   ACTEQU,ACTADD       DONT UPDATE RECORD ON CHANGE                 
         BNE   TEMP30                                                           
*                                                                               
         USING TSXRECD,R6                                                       
TEMP10   L     R6,AIOBC            *** BUILD TEMPO X-REF RECORD ***             
         GOTO1 AXAIO,AIOBC         CLEAR RECORD I/O AREA                        
         MVI   TSXKTYP,TSXKTYPQ    X'3E'                                        
         MVI   TSXKSUB,TSXKSUBQ    X'13'                                        
         MVC   TSXKCPY,CMPY        COMPANY                                      
         LA    RF,BCACKACT                                                      
         SR    R1,R1                                                            
         IC    R1,BC1RLNQ3                                                      
         SHI   R1,1                                                             
         MVC   TSXKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         EX    R1,*-6                                                           
         OC    TSXKODS,BCSPACES                                                 
         LA    RF,1(R1,RF)                                                      
         IC    R1,BC1RLEV4                                                      
         SHI   R1,1                                                             
         MVC   TSXKPER(0),0(RF)    ISOLATE PERSON CODE                          
         EX    R1,*-6                                                           
         OC    TSXKPER,BCSPACES                                                 
         MVC   TSXKEND,BCYYMMDD                                                 
         MVI   TSXRRI1,TSXKRI1Q                                                 
         MVI   TSXRRI2,TIMKRI2Q                                                 
         MVI   TSXRRI3,TIMKRI3Q                                                 
         MVI   TSXRRI4,TIMKRI4Q                                                 
         OC    TSXRSTA(1),BCFLAG4  DELETE IF NOT THERE                          
         TM    GFACTST6,X'40'      SCRIPT UPLOAD                                
         BNO   *+8                                                              
         OI    TSXRSTA1,TSXTEMPO   RECORD CREATED DURING SCRIPT UPLOAD          
         TM    BCPRIND,BCPRIDTM    ARE WE RUNNING UNDER DAILY TIME              
         BZ    *+8                                                              
         OI    TSXRSTA1,TSXDAILY   DAILY TIME CREATED RECORD                    
*                                                                               
         LA    RE,TSXRFST          *** 8B TIMEL X-REF ELEMENT **                
         SR    R1,R1                                                            
         ICM   R1,1,BCELEM+1                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SHI   R1,1                                                             
         MVC   0(0,RE),BCELEM                                                   
         EX    R1,*-6                                                           
         LA    RE,1(R1,RE)                                                      
*                                                                               
         USING PIDELD,RE           *** D8 PERSON ID ELEMENT ***                 
         MVI   PIDEL,PIDELQ        X'D8' ELEMENT                                
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,BCPIDNO       PERSON ID #                                  
         LA    RE,PIDLNQ+1(RE)                                                  
         SR    RE,R6                                                            
         STCM  RE,3,TSXRLEN                                                     
         DROP  RE                                                               
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'TSXKEY),TSXKEY                                          
         OI    DMINBTS,X'08'       READ FOR DELETED RECORDS                     
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'TSXKEY),BIGKEY                                         
         BNE   TEMP20                                                           
*                                                                               
         LA    R6,BIGKEY                                                        
         NI    TSXKSTA,X'FF'-X'80' MARK UNDELETED                               
         OC    TSXKSTA(1),BCFLAG4 DELETE IF NOT THERE                           
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIOBC                                                        
         L     R6,AIOBC                                                         
         GOTO1 PUTREC              WRITE ACCMST RECORD BACK                     
         GOTO1 WRITE               WRITE ACCDIR ACTIVE POINTER                  
         B     TEMP30                                                           
*                                                                               
TEMP20   DS    0H                                                               
         MVC   AIO,AIOBC                                                        
         L     R6,AIOBC                                                         
         GOTO1 ADDREC              ADD ACCMST RECORD                            
*                                                                               
TEMP30   MVC   AIO,BCFULL                                                       
         NI    DMINBTS,X'FF'-X'08' TURN OFF READ FOR DELETES                    
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         B     ROUTE                                                            
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* FIND HIGH/LOW MOA OVER ENTIRE TIME RECORD                           *         
*                                                                     *         
* NTRY - AIO = CONTAINS CURRENT TIMEREC                               *         
*                                                                     *         
* EXIT - BCSTDTE = LOWEST MOA ON RECORD                               *         
*      - BCENDTE = HIGHEST MOA ON RECORD                              *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
MOAFIND  DS    0H                                                               
         MVC   BCSTDTE,BCEFFS                                                   
         XC    BCENDTE,BCENDTE                                                  
*                                                                               
         USING TIMELD,R6                                                        
         L     R6,AIO                                                           
         AH    R6,=Y(TIMRFST-TIMKEY)                                            
MOA100   CLI   0(R6),0                                                          
         BE    MOAX                                                             
         CLI   0(R6),TIMELQ        X'8B' ELEMENT                                
         BNE   MOA125                                                           
         CLI   TIMETYP,TIMEINP     INPUT DETAIL ELEMENT                         
         BE    MOA200                                                           
MOA125   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     MOA100                                                           
*                                                                               
MOA200   CLC   TIMMOA,BCSTDTE      SET LOWEST MOA                               
         BH    *+10                                                             
         MVC   BCSTDTE,TIMMOA                                                   
         CLC   TIMMOA,BCENDTE      SET HIGHEST MOA                              
         BL    *+10                                                             
         MVC   BCENDTE,TIMMOA                                                   
         B     MOA125                                                           
*                                                                               
MOAX     CLC   BCSTDTE,BCEFFS                                                   
         BNE   *+10                                                             
         XC    BCSTDTE,BCSTDTE                                                  
         OC    BCENDTE,BCENDTE                                                  
         BNZ   *+10                                                             
         MVC   BCENDTE,BCEFFS                                                   
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* LOOK FOR PENDING TYPE 34 TRANSFERS                                  *         
***********************************************************************         
         SPACE 1                                                                
DRAFT    DS    0H                                                               
*&&US                                                                           
         MVC   BCFULL,AIO          SAVE A(USERS IO AREA)                        
*                                                                               
         USING TSARRECD,R4                                                      
         L     R4,AIO1                                                          
         LA    R3,TSARDH                                                        
DRAFT10  GOTO1 AXAIO,AIO1                                                       
         GOTO1 ATSAR,BCDMCB,(R3),1  CHECK TSAR RECS FOR INCOMP FLAG             
         TM    BCTSERRS,TSEEOF                                                  
         BO    DRAFTX                                                           
         LA    R3,TSANXT                                                        
*                                                                               
         USING TIMELD,R6                                                        
         LA    R6,TRDATA           R6=A(CURRENT X'8B' DETAIL ITEM)              
         CLI   TIMEL,TIMELQ                                                     
         BE    *+6                                                              
         DC    H'0'                FIRST ELEMENT MUST BE 8B DETAIL              
         CLI   TIMETYP,TIMEINP                                                  
         BNE   DRAFT10                                                          
         CLI   TIMTTYP,TIMTCB      BILLABLE TIME                                
         BNE   DRAFT10                                                          
*                                                                               
         USING TRNRECD,R5          READ FOR DRAFT TIMESHEET                     
         LA    R5,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,BCACKCPY    COMPANY CODE                                 
         MVC   TRNKULA,TRKACC      SJ ACCOUNT CODE                              
         MVC   TRNKWORK,TRKTSK     WORKCODE                                     
         MVC   TRNKCULC,BCACCODE   1R ACCOUNT CODE                              
         MVC   TRNKDATE,BCYYMMDD   WEEK ENDING DATE                             
         MVC   TRNKREF,BCSPACES    CREATE BATCH REFERENCE #                     
         MVI   TRNKREF,C'T'                                                     
         SR    R1,R1               PERIOD NUMBER INTO CHARS 2-4                 
         IC    R1,BCPERIOD         OF BATCH REFERENCE                           
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TRNKREF+1(3),DUB+6(2)                                            
         MVI   TRNKSBR,0                                                        
         GOTO1 HIGH                                                             
*                                                                               
DRAFT20  CLC   TRNKEY(TRNKSBR-TRNKEY),KEYSAVE                                   
         BNE   DRAFT10                                                          
         TM    TRNKSTAT,TRNSREVS   REVERSAL                                     
         BO    DRAFT30                                                          
         TM    TRNKSTAT,TRNSDRFT   ELIMINATE LIVE ITEMS (NON DRAFT)             
         BNO   DRAFT30                                                          
         MVC   AIO,AIOBC                                                        
         GOTO1 GETREC                                                           
         L     R2,AIOBC                                                         
         AH    R2,=Y(ACCRFST-ACCKEY)                                            
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ        X'44' ELEMENT                                
         BNE   DRAFT30                                                          
         CLI   TRNTYPE,34          TRANSFERS                                    
         BNE   DRAFT30                                                          
         USING TIMELD,R6                                                        
         LA    R6,TRDATA           R6=A(CURRENT X'8B' DETAIL ITEM)              
         OI    TIMSTAT,TIMLOCK+TIMDRPND     LOCK AND MARK PENDING               
         MVC   TRKREF,TRNBTCH      SAVE REF IN TSAR RECORD                      
         MVC   TRKDTE,TRNKDATE     SAVE DATE IN TSAR RECORD                     
         GOTO1 ATSAR,BCDMCB,TSAWRT,1                                            
         B     DRAFT10                                                          
*                                                                               
DRAFT30  GOTO1 SEQ                                                              
         LA    R6,BIGKEY                                                        
         B     DRAFT20                                                          
*                                                                               
DRAFTX   MVC   AIO,BCFULL          RESTORE AIO TO ORIGINAL VALUE                
         DROP  R2,R6                                                            
         B     ROUTE                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PASSIVE POINTER BUILD AREA (USED BY ACLDCPTR)                       *         
***********************************************************************         
         SPACE 1                                                                
PPAREA   DC    (ACCKLEN*20)X'00'                                                
         EJECT                                                                  
***********************************************************************         
* SJ CLI/PRD/JOB NEGATIVE HOURS DSECT                                 *         
***********************************************************************         
         SPACE 1                                                                
NEGSJD   DSECT                                                                  
NEGSJACT DS    CL14                SJ ACCOUNT                                   
NEGSJTSK DS    CL2                 SJ TASK CODE                                 
NEGTTYPE DS    XL1                 TYPE OF TIME                                 
NEGSJHRS DS    PL6                 HOURS ACCUMULATOR                            
NEGSJLNQ EQU   *-NEGSJD                                                         
         EJECT                                                                  
***********************************************************************         
* INCLUDES                                                            *         
***********************************************************************         
                                                                                
* ACCAPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCAPWORKD                                                     
         PRINT ON                                                               
* ACCAPDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCAPDSECT                                                     
         PRINT ON                                                               
* ACCAP30GW                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCAP30GW                                                      
         PRINT ON                                                               
         ORG   BCFLDS                                                           
         DS    0D                  DEFINE SOME TEMP STORAGE                     
APPAREA  DS    A                   A(1080BYTE AREA)                             
TCPTRBLK EQU   L'BCFLDS-(*-BCFLDS)                                              
         ORG                                                                    
         ORG   BCSPARE                                                          
TCURDTA  DS    A                   A(CURRENT TIMEL CHUNK)                       
SVCHKLN  DS    H                   SAVED LENGTH OF CURRENT CHUNK                
CMCHKLN  DS    H                   CUMULATIVE LENGTH OF MOVED CHUNK             
SAVSUBCD DS    PL3                 SAVED CLI APPR SUBMITTED DATE                
SAVSUBMD DS    PL3                 SAVED LINE MGR APPR SUBMITTED DATE           
         ORG                                                                    
         SPACE 1                                                                
* ACCAP30DST                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCAP30DST                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*&&UK                                                                           
TIMELD   DSECT                                                                  
         ORG   TIMELD+TIMILN2Q                                                  
TIMERTE  DS    PL4                 EURO CHARGE RATE                             
TIMECRTE DS    PL4                 EURO COST RATE                               
TIMB2LNQ EQU   *-TIMELD            LENGTH OF BILLABLE DATA W/EURO INFO          
*&&                                                                             
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
* ACTIMEBLK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACTIMEBLK                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* ACLDCPTRD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACLDCPTRD                                                      
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACCAP31   08/21/20'                                      
         END                                                                    
