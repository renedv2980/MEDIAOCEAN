*          DATA SET PPJOB03    AT LEVEL 191 AS OF 07/07/08                      
*PHASE T40F03A                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE BINSRCH2                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPJOB03 - PRINTPAK JOB FILE - "SUB" (UNIT) ADCODE'              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SMYE 07/NN/08 DISALLOW USE OF "INACTIVE" JOB CODES AS UNIT CODES              
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T40F03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40F03,R9                                                      
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T40FFFD,RA                                                       
         RELOC RELO02                                                           
*                                                                               
         CLI   SCRNTYP,3           TEST HAVE "SUB" SCREEN                       
         BE    JFM2                YES                                          
*                                  NO - FETCH SCREEN                            
         GOTO1 VCALLOV,DMCB,JOBLAST,X'D9040FFB'                                 
*                                                                               
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SCRNTYP,3                                                        
         MVI   CHGSW,0                                                          
         MVI   NEWSCRN,C'N'                                                     
*                                                                               
JFM2     DS    0H                                                               
*                                                                               
*   CLEAR BINSRCH TABLE AND SET PARAMETERS                                      
*                                                                               
         XC    BINPARMS(24),BINPARMS                                            
         XC    BINTBL,BINTBL       100 BYTES ONLY                               
*                                                                               
         SR    R0,R0               ADDRS OF INSERT                              
         LA    R1,BINTBL           ADDRS OF TBL                                 
         SR    R2,R2               NUM OF RECS SO FAR                           
         LA    R3,6                L'REC                                        
         LA    R4,6                BYTE 0=KEY DISP,1-3=L'KEY                    
         LA    R5,10               MAX NUM OF RECS                              
         STM   R0,R5,BINPARMS                                                   
*                                                                               
         CLC   =C'CHA',JOBACT                                                   
         BE    CHA                                                              
         CLC   =C'SUB',JOBACT                                                   
         BE    DIS                                                              
JFM5     DS    0H                                                               
         LA    R3,ACTERR                                                        
         LA    R2,JOBACTH                                                       
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                  CHANGE ROUTINES                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CHA      DS    0H                                                               
         CLI   CHGSW,1             IF NOT DISPLAYED FOR CHANGE                  
         BNE   DIS                    DISPLAY RECORD INSTEAD                    
*                                                                               
CHA2     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(14),SVMED     M/C/CLT/PRD/JOB                              
         MVI   KEY+3,X'15'                                                      
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    CHACOD                                                           
*                                                                               
         LA    R3,CHGERR           CHG MUST BE PRECEDED BY DISPLAY              
         LA    R2,JOBACTH                                                       
         B     ERROR                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                  VALIDATE "UNIT" CODES AND SHARES             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CHACOD   DS    0H                  PREPARE FOR EDITING                          
         BAS   RE,GETREC                                                        
*                                                                               
         CLI   PJOBREC+33,X'15'    MAIN JOB ELEMENT ?                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MSTSTA(6),MSTSTA    CLEAR "GROUP" START AND END DATES            
         MVC   MSTSTA(6),PJOBSTA   SAVE  "GROUP" START AND END DATES            
*                                                                               
         SR    R6,R6               CLEAR FOR ACCUMULATING SHARES                
         SR    R7,R7               CLEAR FOR COUNTING CODE ENTRIES              
         SR    R8,R8               CLEAR FOR COUNTING SHARE ENTRIES             
         XC    UPDTBL,UPDTBL       CLEAR FOR SAVING CODE/SHARE ENTRIES          
         LA    RE,UPDTBL                                                        
         ST    RE,UPDPTR           SET ADDRESS OF CODE/SHARE ENTRY              
*                                                                               
         MVC   SAVEKEY,KEY         SAVE "GROUP" KEY                             
         LA    R2,JOBCOD1H         POINT TO FIRST CODE FIELD                    
*                                                                               
CHACODB  DS    0H                  SCREEN EDITING LOOP BEGINS HERE              
         CLI   5(R2),0             ANYTHING ENTERED ?                           
         BNE   CHACODC             YES                                          
         LR    RE,R2               NO                                           
         ZIC   R0,0(R2)            GET FIELD LENGTH                             
         AR    RE,R0               BUMP TO SHARE FLD HDR                        
         CLI   5(RE),0             ANYTHING IN SHARE ?                          
         BE    CHACODX             NO - OKAY                                    
*                                                                               
         LA    R3,MISSERR          MISSING INPUT FIELD (CODE)                   
         B     ERROR                                                            
*                                                                               
CHACODC  DS    0H                                                               
         CLI   5(R2),12            AD-ID ENTERED ?                              
         BE    CHACODG             YES - VALIDATE IT                            
         CLI   5(R2),7             LENGTH 1 TO 6 ?                              
         BL    CHACODG             YES - VALIDATE IT                            
         LA    R3,185              AD CODE NOT FOUND (INVALID LENGTH)           
         B     ERROR                                                            
*                                                                               
CHACODG  DS    0H                  VALIDATE CODE ENTERED EXISTS                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(14),SVMED     M/C/CLT/PROD/JOB                             
         MVI   KEY+3,X'15'                                                      
         CLI   5(R2),12            AD-ID ONLY ?                                 
         BE    CHAGETC1            YES                                          
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+10(6),8(R2)     SET UNIT-CODE INTO KEY                       
         OC    KEY+10(6),SPACES                                                 
         B     CHAGETH                                                          
*                                                                               
CHAGETC1 DS    0H                                                               
         MVI   KEY+3,PADIKRCQ      'C1' PASSIVE KEY CODE                        
         MVC   KEY+10(12),8(R2)    AD-ID                                        
*                                                                               
CHAGETH  BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE     FOUND ?                                      
         BE    CHAGETR             YES - GET THE UNIT RECORD                    
         LA    R3,RECERR                                                        
         B     ERROR                                                            
*                                                                               
CHAGETR  DS    0H                                                               
         BAS   RE,GETREC           GET THE UNIT RECORD                          
*                                                                               
*        TEST FOR VARIOUS DISALLOWED CONDITIONS FOR UNIT RECORD                 
*                                                                               
         CLC   PJOBREC(25),SAVEKEY     UNIT-CODE = TO "GROUP" CODE ?            
         BE    CHAERR4                 YES - NOT ALLOWED                        
*                                                                               
         CLI   PJOBKJOB,JBSUB1Q   "GROUP" RECORD ? (SPECIAL JOB CODE)           
         BE    CHAERR4             YES - NOT ALLOWED                            
*                                                                               
         LA    R4,PJOBREC+33                                                    
         MVI   ELCODE,PJSUBIDQ    X'90'=UNIT CODE ELEM                          
*                                                                               
         CLI   0(R4),PJSUBIDQ    "GROUP" RECORD ? (HAS UNIT-CODE ELEM)          
         BE    CHAERR4             YES - NOT ALLOWED                            
*                                                                               
         BRAS  RE,NEXTEL           SAME AS ABOVE - HAS UNIT-CODE ELEM ?         
         BE    CHAERR4                                                          
*                                                                               
         CLC   PJOBCAP1(8),=C'INACTIVE'    "INACTIVE" JOB CODE ?                
         BE    CHAERR8             NOT ALLOWED AS UNIT CODE                     
*                                                                               
         OC    PJOBSTA(6),PJOBSTA  DOES UNIT CODE HAVE START-END DATES?         
         BZ    CHAGETX             NO - OKAY                                    
*                                                                               
         OC    MSTSTA(6),MSTSTA    DOES GROUP CODE HAVE DATES ?                 
         BZ    CHAERRA             NO - UNIT DATES NG W/O GROUP DATES           
*                                                                               
         CLC   MSTSTA,PJOBSTA      GROUP START EARLIER THAN UNIT-CODE ?         
         BL    CHAERR7             YES - OVERLAP ERROR                          
*                                                                               
         CLC   MSTEND,PJOBEND      GROUP END LATER THAN UNIT-CODE ?             
         BH    CHAERR7             YES - OVERLAP ERROR                          
*                                                                               
CHAGETX  DS    0H                                                               
         XC    KEY,KEY             SET KEY TO THAT OF RECORD IN CASE            
         MVC   KEY(25),PJOBREC       PASSIVE KEY WAS USED PREVIOUSLY            
         BAS   RE,HIGH             AND REREAD FOR INST.RECORD TEST              
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,SEQ              LOOK FOR INSTRUCTION RECORD                  
         CLC   KEY(16),KEYSAVE     SAME THRU JOB CODE ?                         
         BNE   CHAERR5             NO - NO INST RECORD                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                  CHECK FOR DUPLICATION ON SCREEN              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         XC    WORK,WORK                                                        
         MVC   WORK(6),PJOBKJOB    JOB CODE                                     
*                                                   GET THE ENTRY               
         GOTO1 =V(BINSRCH),BINPARMS,(X'00',WORK),RR=RELO02                      
         CLI   0(R1),X'01'         NOT FOUND ?                                  
         BNE   CHAERR6             NO - FOUND - DUPLICATE ENTRY                 
*                                                   ADD UNIT CODE               
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK),RR=RELO02                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         L     RE,UPDPTR           ADDRESS OF UPDATE TABLE ROW                  
         MVC   0(6,RE),PJOBKJOB    SAVE UNIT CODE HERE (SHARE WILL BE           
*                                             ADDED TO UPDTBL BELOW)            
         CLI   PJOBKJOB,X'FF'      ADID "ALONE"                                 
         BNE   *+10                NO                                           
         MVC   10(12,RE),PJOBADID  YES - SAVE ADID CODE HERE                    
*                                                                               
         LA    R7,1(R7)            ADD TO CODE ENTRY COUNT                      
*                                                                               
CHACODX  DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT FIELD (SHARE)                   
         BNE   *+6                                                              
         DC    H'0'                SCREEN SHOULD NEVER END HERE                 
*                                                                               
         CLI   5(R2),0             SHARE ENTERED ?                              
         BE    CHASHRX             NO                                           
*                                  VALIDATE SHARE                               
         GOTO1 =V(NUMVAL),DMCB,(3,8(R2)),(X'01',0),RR=RELO02                    
         CLI   DMCB,0                                                           
         BNE   CHAERR1             INVALID FIELD                                
         L     R0,4(R1)            BINARY VALUE AT 4(R1)                        
         CHI   R0,0                                                             
         BE    CHAERR1             INVALID FIELD - CANNOT BE ZERO               
         AR    R6,R0               R6 IS SUM OF SHARES                          
*                                                                               
         CVD   R0,DUB                                                           
         L     RE,UPDPTR           ADDRESS OF UPDATE TABLE ROW                  
         MVC   6(4,RE),DUB+4       PUT PACKED SHARE VALUE HERE                  
*                                                                               
         LA    R8,1(R8)            ADD TO SHARE ENTRY COUNT                     
*                                                                               
CHASHRX  DS    0H                                                               
*                                                                               
         L     RE,UPDPTR           ADDRESS OF UPDATE TABLE ROW                  
*                                                                               
         CLI   0(RE),C' '          WAS ANYTHING ENTERED ?                       
         BNH   CHASHRX4            NO - DO NOT BUMP TO NEXT TABLE ENTRY         
*                                                                               
         LA    RE,UPDTBLN(RE)      BUMP TO NEXT TABLE ENTRY                     
         ST    RE,UPDPTR           ADDRESS OF NEXT UPDATE TABLE ROW             
*                                                                               
CHASHRX4 DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT SCREEN CODE FIELD               
*                                                                               
         BNE   CHACODB             MORE FIELDS TO TEST                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   FINAL EDITING AND RECORD RESTORATION                        
*     R7->CODE COUNT        R8->SHARE COUNT       R6->SHARE SUM                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
ENDED    DS    0H                                                               
         CHI   R7,2                MINIMUM OF 2 UNIT CODES ON SCREEN ?          
         BL    CHAERRC             NO - ERROR                                   
*                                                                               
         CR    R7,R8               SAME NO OF CODE AND SHARE ENTRIES?           
         BE    ENDED20             YES - OKAY SO FAR                            
         CHI   R8,0                ANY SHARE ENTRIES ?                          
         BNE   CHAERR2             YES - "ALL OR NOTHING" ERROR                 
*                                                                               
ENDED20  DS    0H                                                               
         CHI   R8,0                ANY SHARE ENTRIES                            
         BNE   ENDED40             YES                                          
         CHI   R6,0                NO - ACCUMULATION MUST BE ZERO               
         BE    ENDED40                                                          
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
ENDED40  DS    0H                                                               
         CHI   R7,0                ANY CODE ENTRIES ?                           
         BE    ENDEDR              NO - FINISH UPDATE                           
         CHI   R8,0                ANY SHARE ENTRIES ?                          
         BE    ENDEDR              NO - MUST CALCULATE IN UPDATE                
         C     R6,=F'100000'       ADD UP TO 100.000 ?                          
         BNE   CHAERR3             NO - ERROR                                   
*                                                                               
ENDEDR   DS    0H                  RESTORE GROUP FOR UPDATE                     
         XC    KEY,KEY                                                          
         MVC   KEY,SAVEKEY         RESTORE "GROUP" KEY                          
         BAS   RE,HIGH             AND SEQUENCE                                 
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETREC           GET RECORD FOR UPDATE                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   CHECK FOR ANY CHANGES OTHER THAN % SHARE IF GROUP HAS BEEN ON I/O           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LA    R4,PJOBREC+33       R4 USED IN NEXTEL                            
         MVI   ELCODE,PJSAIDQ      X'99'=UNIT CODE ACTIVITY ELEM                
         BRAS  RE,NEXTEL                                                        
         BNE   ENDEDX              NOT FOUND - MUST BE NEW GROUP                
*                                                                               
         USING PJSAELM,R4                                                       
         TM    PJSACH1,PJSAUSED    USED ON INS ORDER ?                          
         BNO   ENDEDX              NO - CHANGE OKAY                             
         DROP  R4                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   RECORD USED ON INSERTION ORDER - CHECK FOR UNIT CODE CHANGE                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         LA    R4,PJOBREC+33       POINT BACK TO ELEMENT START                  
         MVI   ELCODE,PJSUBIDQ     X'90'=UNIT CODE ELEMENT                      
         SR    R5,R5               USE TO COUNT # OF EXISTING ELEMENTS          
ENDEDT   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   ENDEDT3                                                          
         LA    R5,1(R5)            ADD TO ELEMENT COUNT                         
         B     ENDEDT              LOOK FOR MORE                                
*                                                                               
ENDEDT3  DS    0H                  R7 HAS COUNT OF SCREEN ENTRIES               
         CR    R5,R7               ELEMENT COUNT = ENTRIES COUNT ?              
         BNE   CHAERRB            ERROR - UNIT CODE(S) ADDED OR REMOVED         
*                  NO ENTRIES ADDED OR DELETED - NOW SEE IF ANY CHANGED         
         LA    R4,PJOBREC+33       POINT BACK TO ELEMENT START                  
         MVI   ELCODE,PJSUBIDQ     X'90'=UNIT CODE ELEMENT                      
         USING PJSUBEL,R4                                                       
ENDEDT5  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   ENDEDX              DONE - OKAY                                  
         LA    R5,UPDTBL          TABLE OF UNIT CODES ENTERED ON SCREEN         
ENDEDT7  DS    0H                                                               
         CLI   0(R5),0             END OF TABLE ?                               
         BE    CHAERRB             YES - ERROR - UNIT CODE CHANGED              
         CLC   PJSUBCOD,0(R5)      UNIT CODE FROM REC STILL ON SCREEN ?         
         BE    ENDEDT5             YES - LOOK FOR MORE                          
         LA    R5,UPDTBLN(R5)      BUMP TO NEXT ENTRY                           
         B     ENDEDT7             SEE IF MATCH FOUND                           
*                                                                               
ENDEDX   DS    0H                                                               
*                                                                               
         DROP  R4                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*    SCREEN HAS BEEN EDITED - SHARE CALCULATION MAY BE NEEDED                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                  DELETE ALL '90' ELEMS FROM REC               
         LA    R4,PJOBREC+33       R4 USED IN NEXTEL                            
         MVI   ELCODE,PJSUBIDQ     X'90'=UNIT CODE ELEM                         
*                                                                               
CHGDELC  DS    0H                                                               
         CLI   0(R4),PJSUBIDQ                                                   
         BE    CHGDELP                                                          
         CLI   0(R4),0                                                          
         BE    CHGDELX             EOR                                          
*                                                                               
CHGDELN  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   CHGDELX             DONE WITH DELETES                            
*                                                                               
CHGDELP  DS    0H                  DELETE ELEMENT                               
         GOTO1 VRECUP,DMCB,(1,PJOBREC),(R4),0                                   
         B     CHGDELC             LOOK FOR MORE ELEMENTS                       
*                                                                               
CHGDELX  DS    0H                                                               
         CHI   R7,0                ANY CODE ENTRIES ?                           
         BE    CHAEND              NO - DONE                                    
*                                                                               
         CHI   R8,0                ANY SHARE ENTRIES                            
         BNE   CHGOUT              YES - OUTPUT ELEMENTS                        
*                                                                               
*                                  CALCULATE SHARES FOR EACH ENTRY              
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         A     R1,=F'100000'                                                    
         DR    R0,R7               DIVIDE 100.000 BY NO OF ENTRIES              
         CVD   R1,DUB              DUB HAS INDIVIDUAL SHARE PCT                 
         MVC   SHRSAV,DUB+4                                                     
         ZAP   DUB,=P'100000'      SET FOR USE AT "LAST CODE"                   
         SR    R6,R6               PREP R6 AND R7 TO UPDATE SHARES              
         SHI   R7,1                  IN UPDTBL                                  
*                                  UPDATE UPDTBL WITH CALCULATED SHARE          
         LA    RE,UPDTBL                                                        
*                                                                               
CHGTBL   DS    0H                                                               
         MVC   6(4,RE),SHRSAV                                                   
         SP    DUB,SHRSAV                                                       
         AHI   R6,1                                                             
         CR    R6,R7               LAST SHARE ?                                 
         BL    *+10                NO                                           
         MVC   SHRSAV,DUB+4        YES - USE "REMAINING" SHARE                  
         LA    RE,UPDTBLN(RE)      BUMP TO NEXT TABLE ENTRY                     
         CLI   0(RE),C' '          ANY ENTRY ?                                  
         BH    CHGTBL              YES                                          
*                                                                               
CHGOUT   DS    0H                  CREATE NEW ELEMS IN WORKA                    
         LA    R5,WORKA                                                         
         USING PJSUBEL,R5          UNIT CODE ELEMENT                            
         LA    R6,UPDTBL           EDITED UNIT CODE/SHARE STORED HERE           
*                                                                               
CHGOUTD  DS    0H                                                               
         XC    WORKA,WORKA         INIT ELEMENT BUILD AREA                      
         MVI   PJSUBCDE,PJSUBIDQ   SET UNIT-CODE ELEMENT ID                     
         MVI   PJSUBLEN,PJSUBLNQ   AND LENGTH                                   
         MVC   PJSUBCOD,0(R6)      UNIT CODE FROM TABLE                         
         MVC   PJSUBPCT,6(R6)      SHARE     FROM TABLE                         
*                                                                               
         CLI   PJSUBCOD,X'FF'      ADID "ONLY" CODE ?                           
         BNE   CHGOUTK             NO - GO ADD ELEMENT                          
         MVC   PJSUBAID(L'PJOBADID),10(R6)     ADID CODE FROM TABLE             
         MVI   PJSUBLEN,PJSUBMAX   AND REPLACE LENGTH (NOW 27)                  
*                                                                               
         DROP  R5                                                               
*                                                                               
CHGOUTK  DS    0H                                                               
         LA    R4,PJOBREC+33                                                    
         MVI   ELCODE,PJSAIDQ      ACTIVITY ELEMENT (KEEP IT AT END)            
         BRAS  RE,NEXTEL                                                        
*                                                                               
*                 ADD UNIT-CODE ELEMENT (X'FE' MEANS "SPECIAL SYSTEM")          
         GOTO1 VRECUP,DMCB,(X'FE',PJOBREC),WORKA,(C'R',(R4)),          X        
               =AL2(33,25,499)     499 IS MAX RECORD SIZE                       
*                                                                               
         LA    R3,204              RECORD TOO BIG                               
         LA    R2,JOBACTH                                                       
         CLI   DMCB+8,0            ONLY ERROR CAN BE NO ROOM IN REC             
         BE    ERROR                                                            
*                                                                               
         LA    R6,UPDTBLN(R6)      BUMP TO NEXT ENTRY                           
         CLI   0(R6),0             ANY (MORE) CODES ?                           
         BH    CHGOUTD             YES                                          
*                                                                               
CHAEND   DS    0H                  DO ACTIVITY ELEMENT AND WRITE RECORD         
         LA    R4,PJOBREC+33                                                    
         CLI   0(R4),PJSAIDQ       POINTING TO ACTIVITY ELEMENT ?               
         BE    CHAENDR             YES - WE HAVE AN EXISTING ELEMENT            
         MVI   ELCODE,PJSAIDQ                                                   
         BRAS  RE,NEXTEL                                                        
         BE    CHAENDR             WE HAVE AN EXISTING ELEMENT                  
*                                  NO ACTIVITY ELEM - ADD ONE                   
         XC    WORKA,WORKA         INIT ELEMENT BUILD AREA                      
         LA    R5,WORKA                                                         
         USING PJSAELM,R5          UNIT CODE ACTIVITY ELEMENT                   
         MVI   PJSACDE,PJSAIDQ     SET UNIT-CODE ELEMENT ID                     
         MVI   PJSALEN,PJSACTLQ    AND LENGTH                                   
         MVC   PJSADTEA,BTODAY     TODAY'S DATE ("CREATED" DATE)                
         MVC   PJSAPID,SVPID       PID OF GREATOR                               
         OI    PJSACH1,PJSAADD     RECORD ADDED                                 
         B     CHAENDX             ADD ACTIVITY ELEMENT                         
*                                                                               
         DROP  R5                                                               
*                                                                               
CHAENDR  DS    0H                  CHANGE ACTIVITY ELEMENT                      
         USING PJSAELM,R4          R4 POINTING TO ACTIVITY ELEMENT              
         MVC   PJSADTE,BTODAY      TODAY'S DATE (CHANGE DATE)                   
         MVC   PJSAPID,SVPID       PID OF CHANGER                               
         OI    PJSACH1,PJSACHA     RECORD CHANGED                               
         B     CHAPUT                                                           
*                                                                               
         DROP  R4                                                               
*                                                                               
CHAENDX  DS    0H                                                               
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PJOBREC),WORKA,(R4)        ADD ELEMENT            
*                                                                               
CHAPUT   DS    0H                                                               
*                                                                               
         BAS   RE,PUTREC                                                        
*                                                                               
         MVI   CHAFLG,C'C'         TELLS DIS TO USE "CHANGE" MESSAGE            
         B     DIS                                                              
*                                                                               
         SPACE 3                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                  ERROR MESSAGE HANDLING                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CHAERR1  DS    0H                  INVALID FIELD ERROR                          
         LA    R3,INVERR                                                        
         B     ERROR                                                            
CHAERR2  DS    0H                  SHARE ENTRY MUST BE ALL OR NONE              
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'ALLORNO),ALLORNO                                        
         LA    R2,JOBPCT1H                                                      
         B     CHAERRX1                                                         
CHAERR3  DS    0H                  SHARES DO NOT ADD UP TO 100.000              
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'NOT100),NOT100                                          
         LA    R2,JOBPCT1H                                                      
         B     CHAERRX1                                                         
CHAERR4  DS    0H                  UNIT CODE MAY NOT BE GROUP CODE              
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'BADSUB),BADSUB                                          
         B     CHAERRX1                                                         
CHAERR5  DS    0H                  UNIT CODE REC MUST HAVE INST REC             
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'NOINST),NOINST                                          
         B     CHAERRX1                                                         
CHAERR6  DS    0H                  DUPLICATES PRIOR CODE ENTRY                  
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'DUPCOD),DUPCOD                                          
         B     CHAERRX1                                                         
CHAERR7  DS    0H                  GROUP DATES OVERLAP UNIT CODE DATES          
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'DTOVLAP),DTOVLAP                                        
         B     CHAERRX1                                                         
CHAERR8  DS    0H                  "INACTIVE" UNIT CODES NOT ALLOWED            
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'NOINACT),NOINACT                                        
         B     CHAERRX1                                                         
CHAERR9  DS    0H                  NOT GROUP RECORD - USE ADF ACTION            
         LA    R2,JOBJOBH          POINT TO "AD CODE" FIELD                     
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'NOTMSTR),NOTMSTR                                        
         B     CHAERRX1                                                         
CHAERRA  DS    0H                  GROUP DATES REQUIRED FOR UNIT CODE           
         XC    JOBMSG,JOBMSG         DATES TO BE USED                           
         MVC   JOBMSG(L'NOMSTDT),NOMSTDT                                        
         B     CHAERRX1                                                         
CHAERRB  DS    0H                 USED IN BUY - ONLY % SHARE CHANGEABLE         
         LA    R2,JOBJOBH         POINT TO "AD CODE" FIELD                      
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'NOCHG),NOCHG                                            
         B     CHAERRX1                                                         
CHAERRC  DS    0H                  MINIMUM OF 2 UNIT CODES REQUIRED             
         LA    R2,JOBCOD1H         POINT TO FIRST UNIT CODE FIELD               
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'TOOFEW),TOOFEW                                          
         B     CHAERRX1                                                         
*                                                                               
CHAERRX1 DS    0H                                                               
         FOUT  JOBMSGH                                                          
         MVI   ERRAREA,X'FF'                                                    
         B     JOBEXIT                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                  DISPLAY ROUTINES                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DIS      DS    0H                                                               
*                                                                               
         LA    R2,JOBCOD1H         POINT TO FIRST INPUT FIELD HEADER            
*                                                                               
         BRAS  RE,CLRFLDS      CLEAR I/P AREAS OF SCREEN (FROM CODE ON)         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(14),SVMED     M/C/CLT/PROD/JOB                             
         MVI   KEY+3,X'15'                                                      
         LA    R2,JOBJOBH                                                       
         CLI   5(R2),0             AD-ID ONLY ?                                 
         BNE   DISR                NO - HAVE JOB CODE                           
         MVI   KEY+3,PADIKRCQ      'C1' PASSIVE KEY CODE                        
         MVC   KEY+10(12),SVADID   AD-ID                                        
         LA    R2,JOBADIDH                                                      
DISR     BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    DIS2                                                             
         LA    R3,RECERR                                                        
         B     ERROR                                                            
*                                                                               
DIS2     DS    0H                                                               
*                                                                               
         CLI   KEY+10,JBSUB1Q     DOES AD CODE BEGIN WITH ! ?                   
         BE    DIS2C               YES - OKAY                                   
*                                                                               
         B     CHAERR9             ERROR - NOT A GROUP CODE                     
*                                                                               
DIS2C    DS    0H                                                               
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         OC    PJOBKPUB,PJOBKPUB   HEADER RECORD ?                              
         BZ    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         MVC   KEY(L'PJOBKEY),PJOBKEY  RESET KEY (IN CASE AD-ID ONLY            
*                                                  IS EVER ALLOWED)             
         CLI   PJOBKJOB,X'FF'      AD-ID ONLY ?                                 
         BNE   DIS2P               NO                                           
         LA    R2,JOBJOBH          ERROR -                                      
         XC    JOBMSG,JOBMSG       ADID ONLY NOT ALLOWED AS "GROUP"             
         MVC   JOBMSG(L'ADIDERR),ADIDERR                                        
         FOUT  JOBMSGH                                                          
         MVI   ERRAREA,X'FF'                                                    
         B     JOBEXIT                                                          
*                                                                               
DIS2P    DS    0H                                                               
*                                                                               
         XC    JOBADID,JOBADID     CLEAR SCREEN FIELD                           
*                                                                               
         MVC   JOBJOB,PJOBKJOB     "REGULAR" JOB CODE                           
         FOUT  JOBJOBH                                                          
         CLI   PJOBADID,C' '       IS THERE AN ADID ?                           
         BNH   DIS3                NO                                           
         MVC   JOBADID,PJOBADID    ADID CODE                                    
         FOUT  JOBADIDH                                                         
*                                                                               
DIS3     DS    0H                                                               
         TM    KEY+25,X'C0'                                                     
         BZ    DIS4                                                             
         LA    R3,DELERR                                                        
         LA    R2,JOBCOD1H                                                      
         B     ERROR                                                            
*                                                                               
DIS4     DS    0H                                                               
         BRAS  RE,INFMT                                                         
         MVI   CHGSW,1             SET TO ALLOW CHANGE NEXT TIME                
*                                                                               
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'IRDMSG),IRDMSG       DISPLAY MESSAGE                    
         CLI   CHAFLG,C'C'         COMING FROM CHANGE ?                         
         BNE   DISEXIT             NO                                           
         MVI   CHAFLG,0            CLEAR FLAG                                   
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'IRCMSG),IRCMSG       CHANGE MESSAGE                     
DISEXIT  DS    0H                                                               
         FOUT  JOBMSGH                                                          
*                                                                               
         LA    R2,JOBACTH                                                       
*                                                                               
JOBEXIT  DS    0H                  EXIT FROM MODULE                             
*                                                                               
         LA    R3,JOBMSGH              POINT TO SERVICE                         
         ZIC   R0,0(R3)                REQUEST                                  
         AR    R3,R0                   FIELD.                                   
         OI    6(R3),X'81'             CHANGE TO MODIFIED & TRANSMIT.           
*                                                                               
*        IF PFKEY HIT THEN CURSOR REMAINS WHERE IT WAS                          
*                                                                               
         ICM   RF,15,ATIOB         POINT TO TIOB                                
*                                                                               
         CLI   TIOBAID-TIOBD(RF),0 SKIP IF ENTER HIT                            
         BE    *+12                                                             
         OI    TIOBINDS-TIOBD(RF),TIOBSETC LEAVES CURSOR WHERE IT WAS           
         B     *+8                                                              
         OI    6(R2),OI1C .        INSERT CURSOR                                
*                                                                               
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
         B     EXXMOD                                                           
*                                                                               
         SPACE 3                                                                
IRCMSG   DC    C'GROUP CODE DATA CHANGED'                                       
IRDMSG   DC    C'GROUP CODE DATA DISPLAYED'                                     
ADIDERR  DC    C'** ERROR - GROUP CODE CANNOT BE AD-ID ONLY'                    
ALLORNO  DC    C'** ERROR - ENTER ALL OR NO % SHARE VALUES'                     
NOT100   DC    C'** ERROR - SHARES DO NOT ADD UP TO 100.000'                    
BADSUB   DC    C'** ERROR - GROUP CODE NOT ALLOWED AS UNIT CODE'                
NOINST   DC    C'** ERROR - UNIT CODE MUST HAVE INSTRUCTION RECORD'             
DUPCOD   DC    C'** ERROR - DUPLICATE AD CODES/AD-IDS NOT ALLOWED'              
DTOVLAP  DC    C'** ERROR - UNIT CODE DATES MUST COVER ALL GROUP DATES'         
NOTMSTR  DC    C'** ERROR - NOT A GROUP CODE'                                   
NOMSTDT  DC    C'** ERROR - UNIT CODE HAS DATES - GROUP DOES NOT'               
NOCHG    DC    C'** ERROR - USED IN I/O - ONLY % SHARE CHANGEABLE'              
TOOFEW   DC    C'** ERROR - MINIMUM OF TWO UNIT CODES REQUIRED'                 
NOINACT  DC    C'** ERROR - "INACTIVE" UNIT CODES NOT ALLOWED'                  
*******NOCODE   DC    C'** ERROR - PCT ENTRY REQUIRES CODE ENTRY'               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*          DATA SET PPGENEROL  AT LEVEL 028 AS OF 05/01/02                      
         SPACE 3                                                                
*                  INITIALISATION CODE                                          
         SPACE 3                                                                
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(44),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LA    R3,IOAREA                                                        
         ST    R3,AREC                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CH    R5,=H'250'                                                       
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R5,=H'250'                                                       
         B     CLEARWRK                                                         
         SPACE 2                                                                
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
         SPACE 2                                                                
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
         SPACE 2                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
         SPACE 2                                                                
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                        OR NON NUMERIC                       
         CHI   R1,10              WAS A BUG IF LENGTH IS BIGGER                 
         BL    PACK01             THEN 10                                       
         BR    RE                                                               
PACK01   BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
         SPACE 2                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         SPACE 2                                                                
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
         SPACE 2                                                                
VARMOVE  MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBDIR)                     
         SPACE 3                                                                
READPUB  MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
WRITEPUB MVC   COMMAND,=C'DMWRT'                                                
         B     PUBDIRY                                                          
         SPACE 2                                                                
ADDPUBD  MVC   COMMAND,=C'DMADD '                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBFILE)                    
         SPACE 3                                                                
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUTPUB   MVC   COMMAND,=C'PUTREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
ADDPUB   MVC   COMMAND,=C'ADDREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),APUBIO,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   DS    0H                                                               
         STCM  R2,15,SVERRCUR                                                   
         XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         TITLE 'PPJOB03 - PRINTPAK JOB FILE - "UNIT" ADCODE'  INFMT             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
INFMT    NTR1  BASE=*,LABEL=*      DISPLAY CODES AND PCT SHARES                 
*                                                                               
         LA    R4,PJOBREC+33                                                    
         LA    R2,JOBCOD1H                                                      
*                                                                               
INF2     DS    0H                                                               
         MVI   ELCODE,PJSUBIDQ                                                  
         CLI   0(R4),PJSUBIDQ                                                   
         BE    INF3                                                             
INF2A    DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   INFACT                                                           
*                                                                               
         USING PJSUBEL,R4                                                       
INF3     DS    0H                                                               
         MVC   8(L'PJSUBCOD,R2),PJSUBCOD                                        
         CLI   PJSUBCOD,X'FF'     AD-ID ONLY ?                                  
         BNE   INF4                NO                                           
*                                                                               
         MVC   8(L'PJOBADID,R2),PJSUBAID                                        
         B     INF4                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  HERE TO INF4 HAS BEEN MADE UNNECESSARY BY INCLUDING THE ACTUAL 12-           
*  BYTE ADID CODE IN THE UNITCODE ELEMENT (NO NEED FOR LOOKUP AS BELOW)         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
***      MVC   SAVEKEY,KEY         SAVE KEY WITH D/A                            
*                                                                               
***      XC    KEY,KEY             AD-ID ONLY 'C2' PASSIVE                      
***      LA    R5,KEY                                                           
***      USING PADNRECD,R5                                                      
***      MVC   PADNKEY(PADNKJOB-PADNKEY),PJOBREC                                
***      MVI   PADNKRCD,PADNKRCQ   'C2' PASSIVE                                 
***      MVC   PADNKJOB,PJSUBCOD   "SPECIAL" JOB CODE (X'FFNNNN')               
*                                  GET PASSIVE KEY                              
***      GOTOR VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEYSAVE                  
*                                                                               
***      CLC   KEY(25),KEYSAVE                                                  
***      BE    *+6                                                              
***      DC    H'0'                MUST BE THERE                                
***      DROP  R5                                                               
*                                  GET RECORD                                   
***      GOTOR VDATAMGR,DMCB,=C'GETREC',=C'PRTFILE',KEYSAVE+27,IOAREA,          
***            DMWORK                                                           
***      CLI   DMCB+8,0                                                         
***      BE    *+6                                                              
***      DC    H'0'                NO ERRORS TOLERATED                          
*                                                                               
***      MVC   8(L'PJOBADID,R2),PJOBADID                                        
*                                                                               
***      XC    KEY,KEY                                                          
***      MVC   KEY,SAVEKEY         SAVEKEY HAS D/A                              
*                                  RESTORE KEY                                  
***      GOTOR VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEY,KEYSAVE                  
*                                                                               
***      CLC   KEY(25),KEYSAVE                                                  
***      BE    *+6                                                              
***      DC    H'0'                MUST BE THERE                                
*                                  RESTORE RECORD                               
***      GOTOR VDATAMGR,DMCB,=C'GETREC',=C'PRTFILE',KEY+27,IOAREA,              
***            DMWORK                                                           
***      CLI   DMCB+8,0                                                         
***      BE    *+6                                                              
***      DC    H'0'                MUST BE THERE                                
*                                                                               
INF4     DS    0H                                                               
         OI    6(R2),X'80'         XMIT                                         
         BRAS  RE,BUMPU            NEXT FIELD (PCT)                             
         EDIT  (P4,PJSUBPCT),(7,8(R2)),3                                        
         OI    6(R2),X'80'         XMIT                                         
         BRAS  RE,BUMPU            NEXT FIELD (JOBCODE)                         
         B     INF2A               NEXT ELEMENT                                 
*                                                                               
         DROP  R4                                                               
*                                                                               
INFACT   DS    0H                  DISPLAY ACTIVITY                             
*                                                                               
         LA    R4,PJOBREC+33                                                    
         CLI   0(R4),PJSAIDQ       POINTING TO ACTIVITY ELEMENT ?               
         BE    INFACTD             YES - WE HAVE AN EXISTING ELEMENT            
         MVI   ELCODE,PJSAIDQ                                                   
         BRAS  RE,NEXTEL                                                        
         BNE   INFACTX             NO ACTIVITY ELEMENT                          
*                                                                               
INFACTD  DS    0H                  DISPLAY ACTIVITY                             
*                                                                               
         USING PJSAELM,R4          ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         OC    PJSADTEA(6),PJSADTEA     SKIP IF NO DATE(S) AVAILABLE            
         BZ    INFACTX                                                          
*                                                                               
         OC    PJSADTE,PJSADTE     CHANGE DATE AVAILABLE ?                      
         BZ    INFACTH             NO - MUST BE "NEW"                           
*                                                                               
         MVC   JOBLUPD,=CL13'LAST UPDATED:'                                     
         GOTOR VDATCON,DMCB,(3,PJSADTE),(5,JOBDTEU)   DISP DATE CHANGED         
         B     INFACTM                                                          
*                                                                               
INFACTH  DS    0H                                                               
         MVC   JOBLUPD,=CL13'  CREATED ON:'                                     
         GOTOR VDATCON,DMCB,(3,PJSADTEA),(5,JOBDTEU)   DISP DATE ADDED          
*                                                                               
INFACTM  DS    0H                                                               
         FOUT  JOBLUPDH            TRANSMIT MESSAGE FLD                         
         FOUT  JOBDTEUH            TRANSMIT DATE FLD                            
*                                                                               
         OC    PJSAPID,PJSAPID     SKIP IF NO PID AVAILABLE                     
         BZ    INFACTX                                                          
*                                                                               
         LA    R3,PJSAPID          POINT R3 TO PID                              
*                                                                               
         GOTOR TRNPID,DMCB,0(R3)   DISPLAY PID AND NAME                         
*                                                                               
INFACTX  DS    0H                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
INFEXT   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         TITLE 'PPJOB03 - PRINTPAK JOB FILE - "UNIT" ADCODE'  CLRFLDS           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CLRFLDS  NTR1  BASE=*,LABEL=*      CLEAR I/P (UNPROTECTED) FIELDS               
*                                                                               
CLRFLUP  CLI   0(R2),0             END OF SCREEN ?                              
         BE    CLRFLDX             YES - DONE                                   
         TM    1(R2),X'20'         PROTECTED ?                                  
         BO    CLRFNXT             YES - NEXT FIELD                             
         ZIC   RE,0(R2)            FIELD LENGTH                                 
         SHI   RE,8                SUBTRACT HDR                                 
         TM    1(R2),X'02'         EXTENDED FIELD HEADER ?                      
         BZ    *+8                 NO                                           
         SHI   RE,8                SUBTRACT HDR                                 
         BCTR  RE,0                PREP FOR EXECUTED XC                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR LENGTH OF FIELD                        
         OI    6(R2),X'80'         XMIT                                         
CLRFNXT  ZIC   RE,0(R2)                                                         
         AR    R2,RE               NEXT SCREEN FIELD                            
         B     CLRFLUP                                                          
CLRFLDX  DS    0H                  CLEAR ACTIVITY LINE DATA                     
         XC    JOBLUPD,JOBLUPD                                                  
         XC    JOBDTEU,JOBDTEU                                                  
         XC    JOBPIDU,JOBPIDU                                                  
         XC    JOBNAMU,JOBNAMU                                                  
         FOUT  JOBLUPDH                                                         
         FOUT  JOBDTEUH                                                         
         FOUT  JOBPIDUH                                                         
         FOUT  JOBNAMUH                                                         
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPJOB03 - PRINTPAK JOB FILE - "UNIT" ADCODE'  TRNPID            
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY    P1 ==>   A(PID)                                              *         
*                                                                     *         
*                                                                     *         
*EXIT    PUTS PERSONAL ID IN PIDU SCREEN FIELD                        *         
*        PUTS NAME IN NAMU SCREEN FIELD                               *         
*                                                                     *         
*NOTE    MORE EXTENSIVE THAN ROUTINE IN OTHER MODULES                 *         
*        THEY JUST DISPLAY THE NAME                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRNPID   NTR1  BASE=*,LABEL=*      TRANSLATE PID TO NAME AND MORE               
*                                                                               
         L     R5,0(R1)            POINT TO PID                                 
*                                                                               
         OC    0(2,R5),0(R5)       SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE                                         
*                                                                               
         LA    R4,KEY                                                           
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   CT0KAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,PJOBKAGY       USE JOB REC'S AGENCY                      
*                                                                               
         MVC   CT0KNUM,0(R5)       SET PID                                      
*                                                                               
         GOTOR VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,APUBIO                   
*                                                                               
         L     R4,APUBIO           POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,KEY          SKIP IF RECORD NOT FOUND                     
         BNE   TPIDNOTF                                                         
*                                                                               
*        FIND USER'S ID                                                         
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TPIDCTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDCTDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         - MATCH ON ELEMENT CODE                      
         BE    TPIDCTFD                                                         
*                                                                               
TPIDCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TPIDCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDCTFD DS    0H                                                               
*                                                                               
*        DISPLAY PERSON ID                                                      
*                                                                               
         MVC   JOBPIDU,2(RE)       USER'S PID                                   
*                                                                               
         LA    R3,JOBNAMU          USE DATAAREA OF SCREEN FIELD                 
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ  SET RECORD SUB TYPE                            
*                                                                               
         MVC   SAPEAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,PJOBKAGY       USE JOB REC'S AGENCY                      
*                                                                               
         MVC   SAPEPID,2(RE)       SET USERID FROM PREVIOUS RECORD              
*                                                                               
         GOTOR VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,APUBIO                   
*                                                                               
         L     R4,APUBIO           POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEY     SKIP IF REC NOT FOUND           
         BNE   TPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND NAME ELEMENT                                                      
*                                                                               
TPIDNMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDNMDN                                                         
*                                                                               
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
*                                                                               
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    TPIDNMFD                                                         
*                                                                               
TPIDNMCN DS    0H                                                               
*                                                                               
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDNMLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
TPIDNMDN DS    0H                  NAME ELEMENT NOT FOUND                       
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDNMFD DS    0H                                                               
*                                                                               
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   TPIDNOTF            NO NAME IN ELEMENT                           
*                                                                               
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         LA    R1,WORK             BUILD NAME IN WORKAREA                       
         XC    WORK,WORK                                                        
*                                                                               
TPIDFMLP DS    0H                  FORMAT PERSON'S NAME                         
*                                                                               
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
*                                                                               
         IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
*                                                                               
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
*                                                                               
TPIDFMCN DS    0H                                                               
*                                                                               
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   TPIDFMDN              END OF ELEMENT REACHED                     
*                                                                               
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         LA    R1,1(R1)            ADD IN A SPACING CHARACTER                   
*                                                                               
         B     TPIDFMLP                                                         
*                                                                               
TPIDFMDN DS    0H                                                               
*                                                                               
         B     TPIDSQSH                                                         
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         MVC   WORK(7),=CL7'UNKNOWN'                                            
         LA    R1,WORK+7           POINT TO NEXT OUTPUT POSITION                
*                                                                               
TPIDSQSH DS    0H                                                               
*                                                                               
         LR    R0,R1               END OF OUTPUT MINUS START                    
         LA    RF,WORK             START OF WORKAREA                            
         SR    R0,RF               EQUALS OUTPUT LENGTH                         
*                                                                               
         MVC   DMCB+4(4),=X'D9000A0D'           LOAD SQUASHER                   
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,WORK,(R0)              SQUASH NAME                     
*                                                                               
*        MOVE NAME TO SCREEN                                                    
*                                                                               
         MVC   0(40,R3),SPACES         INIT OUT PUT AREA                        
*                                                                               
         L     RF,4(R1)               SAVE SQUASHED LENGTH                      
*                                                                               
         CHI   RF,40                  IF NAME TOO LONG                          
         BNH   *+8                                                              
         LA    RF,40                     USE MAX FOR RETURN AREA                
*                                                                               
TPIDMVC  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY NAME                                 
*                                                                               
TRNPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4,RE                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
NEXTEL   NTR1  BASE=*,LABEL=*      LOOK FOR ELEMENTS                            
NXTLNXT  ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLC   ELCODE,0(R4)                                                     
         BE    NXTLXIT             FOUND                                        
         CLI   0(R4),0             END OF RECORD ?                              
         BNE   NXTLNXT             NO - LOOK MORE                               
         LTR   R4,R4               NOT FOUND (NOT EQUAL)                        
NXTLXIT  DS    0H                                                               
         XIT1  REGS=(R4)                                                        
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
BUMP     NTR1  BASE=*,LABEL=*      BUMP TO NEXT SCREEN FIELD                    
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BE    BMPXIT                                                           
         LTR   R2,R2               NOT EOS - NOT EQUAL                          
BMPXIT   DS    0H                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
BUMPU    NTR1  BASE=*,LABEL=*      BUMP TO NEXT UNPROTECTED FIELD               
BMPUNXT  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BE    BMPUXIT                                                          
         TM    1(R2),X'20'         PROTECTED ?                                  
         BNZ   BMPUNXT             YES - NEXT FIELD                             
         LTR   R2,R2               UNPROTECTED FIELD - RETURN NOT =             
BMPUXIT  DS    0H                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPJOBWRK          DUMMY SECTIONS START HERE                    
*                                                                               
T40FFFD  DSECT                                                                  
         ORG                                                                    
SAVEKEY  DS    CL32                                                             
CHAFLG   DS    C                   "C" = DISPLAY CHANGE MESSAGE                 
BINPARMS DS    6F                                                               
MSTSTA   DS    XL3                 "GROUP" START DATE - YMD                     
MSTEND   DS    XL3                 "GROUP" END    DATE - YMD                    
SHRSAV   DS    PL4                 CALCULATED % SHARE                           
UPDPTR   DS    F                   POINTER TO ROW OF UPDTBL BELOW               
UPDTBL   DS    XL221               10 X (AD CODE (6) + % SHARE (4) +            
*                                    ADID CODE (12)) + 1                        
UPDTBLN  EQU   22                  LENGTH OF SINGLE UPDTBL ENTRY                
BINTBL   DS    CL100                                                            
*                                                                               
*PADIRECD (DSECT)                                                               
       ++INCLUDE PPGENPADID                                                     
*                                                                               
       ++INCLUDE CTGENFILE                                                      
*                                                                               
       ++INCLUDE SEACSFILE                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'191PPJOB03   07/07/08'                                      
         END                                                                    
