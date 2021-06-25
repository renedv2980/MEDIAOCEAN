*          DATA SET RECNT34    AT LEVEL 213 AS OF 01/18/01                      
*PHASE T80234A                                                                  
         TITLE 'T80234 - APPLY/RECALL/REJECT MAKEGOOD OFFERS'                   
*                                                                               
*******************************************************************             
*                                                                 *             
*     RECNT34 (T80234) --- APPLY/RECALL/REJECT MAKEGOOD OFFERS    *             
*                                                                 *             
* --------------------------------------------------------------- *             
*                                                                 *             
* 22DEC99 SKU NEW MAKEGOOD OFFER                                  *             
* 11JUN99 SKU SKIP ADDING X'76' ELEMENTS UNTIL MAX SIZE OF BUY    *             
*             RECORDS IS INCREASED                                *             
* 12NOV98 SKU FIX DARE APPLY BUG                                  *             
* 17JUL98 SKU ALLOCATE STORAGE CORRECTLY                          *             
* 15JUN98 SKU SUPPORT DIRECT CREDIT                               *             
* 02MAR98 SKU NEW FLAG TO SET DARE MANUAL CHANGES BIT             *             
* 19AUG97 SKU KATZ NATIONAL/AMERICA MERGE SPECIAL CODING          *             
* 06AUG97 SKU USE USER SIGNON FOR RECEIVER ID FOR DARE MAKEGOOD   *             
* 01JUL97 SKU SEND DARE MAKEGOOD CONFIRMATION THROUGH CONTRACT    *             
* 11JUN97 SKU MOVE DETAIL LINE TO BUY COMMENT LINE 2              *             
* 05DEC96 SKU SUPPORT OWNERSHIP CONCEPT                           *             
* 17OCT96 SKU SUPPORT BONUS AND PREEMPT                           *             
* 11SEP96 SKU CANNOT APPLY DARE MAKEGOOD OFFERS HERE              *             
* 03MAR96 SKU FIX APPLY X'66' REMOVAL BUG                         *             
* 26JAN96 SKU DARE AUTO-APPLY                                     *             
* 03JAN96 SKU DATE/TIME CHECK BUG FIX                             *             
* 13DEC95 SKU 2K CONTRACT SUPPORT                                 *             
* 02JAN95 SKU CONCEPTION                                          *             
*                                                                 *             
*******************************************************************             
* NOTE: WE ARE CREATING A COPY OF THE TWA, THE PSEUDO TWA, TO     *             
*       PASS THE MAKEGOODS OFF AS BUYS.                           *             
*******************************************************************             
*                                                                               
T80234   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWORKX-MYWORKD,T80234                                           
         LR    R7,RC                                                            
         USING MYWORKD,R7          LOCAL WORK AREA                              
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         L     R2,4(R1)            POINTS TO CURRENT SELECT FIELD               
         L     R8,ASPULAR                                                       
         USING SPOOLD,R8                                                        
         ST    R8,ASPOOLD                                                       
*                                                                               
         LR    R9,RA                                                            
         AHI   R9,TWAWORKQ                                                      
         USING TWAWORK,R9                                                       
*                                                                               
         BAS   RE,CHECKMG          CHECK IF ACTION VALID                        
         BNZ   ERROR               ALSO PROCESSES REJECT/RECALL                 
                                                                                
         CLI   8(R2),C'A'          ONLY APPLY DO THE FOLLOWING                  
         BNE   EXXMOD                                                           
                                                                                
         ST    R2,TWAMKGR2         SAVE INCASE OF ERROR                         
                                                                                
         LA    R2,MGTWA            DESTINATION                                  
         LHI   R3,TWAMAX           TO-LENGTH                                    
         LR    R4,RA               SOURCE                                       
         LHI   R5,TWAMAX           FROM-LENGTH                                  
         MVCL  R2,R4               MAKE A COPY OF THE TWA,(PSEUDO TWA)          
                                                                                
         LA    RA,MGTWA                                                         
         USING TWAD,RA                                                          
                                                                                
         LR    R9,RA                                                            
         AHI   R9,TWAWORKQ                                                      
                                                                                
         LA    RF,BUYLAST          CLEAR 'PSEUDO' BUY SCREEN                    
         LA    RE,BUYDAYSH                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
                                                                                
         XC    BUYACT,BUYACT                                                    
         MVC   BUYACT(3),=C'BUY'                                                
                                                                                
         XC    MGHDRDA,MGHDRDA     CLEAR MKG HEADER REC D/A                     
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+28(4),TWAMKGD2  SET TO GET SELECTED RECORD                   
                                                                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
         XC    KEY,KEY                                                          
         MVC   KEY(RMKGKPLN-RMKGKEY),IOAREA                                     
         GOTO1 VHIGH                                                            
         B     MGAPP20                                                          
                                                                                
MGAPP10  DS    0H                                                               
         GOTO1 VSEQ                                                             
                                                                                
MGAPP20  DS    0H                                                               
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   MGAPP300            ALL DONE                                     
         OC    RMKGKPLN(6),RMKGKPLN SKIP COMMENT RECORDS                        
         BNZ   MGAPP30                                                          
         MVC   MGHDRDA,KEY+28      SAVE OFF MKG HEADER D/A                      
         MVC   TWAMKGDH,MGHDRDA                                                 
*                                                                               
         B     MGAPP10                                                          
         DROP  R6                                                               
                                                                                
MGAPP30  DS    0H                                                               
         OC    MGHDRDA,MGHDRDA                                                  
         BNZ   *+6                 MUST HAVE MKG HEADER REC D/A!                
         DC    H'0'                                                             
                                                                                
         MVC   MKGKEY,KEY          SAVE OFF SO WE CAN RESTORE LATER             
*                                                                               
         MVC   TWAMKGD2,KEY+28     DA OF CURRENT MAKE-GOOD                      
         MVC   TWABADDR,TWAMKGD2   TREAT MAKE-GOOD AS BUY                       
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         TM    RMKGKRTY,X'F0'      ALL LINES?                                   
         BZ    MGAPP40                                                          
         MVI   ELCODE,X'20'        IF CHOICE, GET STATUS ELEMENT                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         DROP  R6                                                               
                                                                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      SELECTED?                                    
         BZ    MGAPP10                                                          
         DROP  R6                                                               
*                                                                               
MGAPP40  DS    0H                  CHECK IF DIRECT CREDIT PROCESSING            
         NI    TWAMKGF2,X'FF'-X'40'                                             
*                                                                               
         LA    R6,IOAREA           (PREEMPT)                                    
         MVI   ELCODE,X'04'                                                     
         BRAS  RE,GETEL                                                         
         BNE   MGAPP100                                                         
         USING RMKGCMEL,R6                                                      
         CLC   =C'CR=',RMKGCMNT                                                 
         BNE   MGAPP100                                                         
*                                                                               
         OI    TWAMKGF2,X'40'                                                   
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
BUYD     USING RBUYKEY,KEY                                                      
         MVI   BUYD.RBUYKTYP,X'0B'                                              
         MVC   BUYD.RBUYKREP,REPALPHA                                           
*                                                                               
         MVC   BUYD.RBUYKCON,TWACNUM                                            
*                                                                               
         MVC   BUYD.RBUYKPLN,=3X'FF'                                            
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGMGEL,R6                                                      
*                                                                               
MGAPP50  DS    0H                                                               
         CLC   KEY(RBUYKMLN-RBUYKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BUYD.RBUYKLIN,RMKGMGLI                                           
         BE    MGAPP60                                                          
         GOTO1 VSEQ                                                             
         B     MGAPP50                                                          
         DROP  R6,BUYD                                                          
*                                                                               
MGAPP60  DS    0H                                                               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
         MVC   TWABADDR,KEY+28     RESET BUY DISK ADDRESS                       
*                                                                               
         B     MGAPP160                                                         
*                                                                               
MGAPP100 DS    0H                                                               
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'11'        DETAIL COMMENT??                             
         BRAS  RE,GETEL                                                         
         BNE   MGAPP160                                                         
         XC    WORK2,WORK2                                                      
         USING RMKGDCEL,R6                                                      
         ZIC   R1,RMKGDCLN                                                      
         BCTR  R1,0                OVERHEAD                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),RMKGDCEL                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 VDELELEM,DMCB,(11,RBUYREC)                                       
*                                                                               
         LA    R6,RBUYREC+34                                                    
MGAPP140 CLI   0(R6),X'04'         GET ADDRESS TO INSERT COMMENT                
         BH    MGAPP150            THIS SHOULD GIVE US THE SECOND LINE          
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     MGAPP140                                                         
*                                                                               
MGAPP150 DS    0H                                                               
         MVI   WORK2,X'04'         CHANGE TO BUY COMMENT                        
*                                                                               
         GOTO1 VRECUP,DMCB,(2,RBUYREC),WORK2,(R6)                               
*                                                                               
MGAPP160 DS    0H                                                               
         GOTO1 =A(SETFDHDL),RR=Y                                                
*                                  DISPLAY BUY                                  
         GOTO1 VLOAD,DMCB,(X'25',0),0                                           
*                                                                               
*                                                                               
* IF A MAKEGOOD OFFER RECORD RESIDES IN THE RBUYREC AREA, THEN WE WANT          
* TO PROCEED WITH REGULAR MAKEGOOD PROCESSING                                   
*                                                                               
* IF A BUY RECORD IS FOUND INSTEAD, WE ARE ACTUALLY DOING DIRECT                
* CREDIT PROCESSING. THE BUY RECORD IS THE TARGET BUY TO BE CREDITED            
*                                                                               
         TM    TWAMKGF2,X'40'      CREDIT IN PROCESS??                          
         BNZ   MGAPP163                                                         
*                                                                               
         XCEF  RBUYREC,1000        NO, CLEAR BUY AREA FOR NEW BUY               
         B     MGAPP270                                                         
*                                                                               
MGAPP163 DS    0H                                                               
         MVC   BUYACT(3),=C'CHA'                                                
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGMGEL,R6                                                      
*                                                                               
         NI    BUYDTESH+4,X'FF'-X'20'                                           
         XC    BUYDTES,BUYDTES                                                  
         XC    BUYDTE2,BUYDTE2                                                  
         XC    WORK2,WORK2                                                      
         MVC   WORK2(3),=C'CR='                                                 
         LA    R2,WORK2+3                                                       
*                                                                               
MGAPP165 DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,RMKGMGD1),(7,(R2))                                
         AHI   R2,5                                                             
         OC    RMKGMGD2,RMKGMGD2                                                
         BZ    MGAPP170                                                         
         MVI   0(R2),C'-'                                                       
         AHI   R2,1                                                             
         GOTO1 DATCON,DMCB,(3,RMKGMGD2),(7,(R2))                                
         AHI   R2,5                                                             
*                                                                               
MGAPP170 DS    0H                                                               
         MVI   0(R2),C'('                                                       
         EDIT  RMKGMGSP,(3,1(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         AHI   R2,1                                                             
         MVI   0(R2),C')'                                                       
         AHI   R2,1                                                             
*                                                                               
MGAPP180 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   MGAPP190                                                         
         MVI   0(R2),C','                                                       
         AHI   R2,1                                                             
         B     MGAPP165                                                         
         DROP  R6                                                               
*                                                                               
MGAPP190 DS    0H                  MOVE DATES TO SCREEN                         
         MVC   BUYDTES,WORK2                                                    
         MVC   BUYDTE2,WORK2+L'BUYDTES                                          
*                                                                               
MGAPP200 DS    0H                  TRANSFER DETAIL COMMENT                      
         NI    BUYCOM1H+4,X'FF'-X'20'                                           
         NI    BUYCOM2H+4,X'FF'-X'20'                                           
         XC    BUYCOM2,BUYCOM2                                                  
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,GETEL                                                         
         BNE   MGAPP270                                                         
         USING RMKGDCEL,R6                                                      
*                                                                               
         ZIC   R1,RMKGDCLN                                                      
         AHI   R1,-3                                                            
*                                                                               
*&&DO                                                                           
         CLC   =C'MG=',BUYCOM1                                                  
         BE    MGAPP220                                                         
         CLC   =C'CR=',BUYCOM1                                                  
         BE    MGAPP220                                                         
*                                                                               
         XC    BUYCOM1,BUYCOM1                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUYCOM1(0),RMKGDCCM                                              
         B     MGAPP270                                                         
*&&                                                                             
*                                                                               
MGAPP220 DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUYCOM2(0),RMKGDCCM                                              
         DROP  R6                                                               
*                                                                               
MGAPP270 DS    0H                                                               
         GOTO1 =A(SETFLDLN),RR=Y   SET ACTUAL DATA FIELD LENGTHS                
*                                                                               
         OI    TWAMKGF2,X'80'      SET TO SKIP X'66' CHECK IN RECNT15           
         OI    TWADARE,X'08'       DON'T FLAG MANUAL CHANGES                    
*                                                                               
         GOTO1 VLOAD,DMCB,(X'15',0),0                                           
*                                                                               
         NI    TWAMKGF2,X'FF'-X'80'                                             
         NI    TWADARE,X'FF'-X'08'                                              
*                                                                               
         MVC   RBUYKPLN,=3X'FF'    ALWAYS SET TO NO PLAN                        
*                                  WE NEED THIS FOR BONUS, WHEN WE NEED         
*                                  TO ADD A NEW BUY LINE                        
*                                  UPDATE BUCKETS IN CONTRACT                   
MGAPP280 DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'30',0),0                                           
*                                                                               
         TM    TWAMKGF2,X'40'      CREDIT IN PROCESS??                          
         BZ    MGAPP285                                                         
*                                                                               
         GOTO1 VMOVEREC,DMCB,RBUYREC,AIO3                                       
*                                                                               
MGAPP285 DS    0H                                                               
         BAS   RE,UPDTTGBY         IF MAKEGOOD, UPDATE TARGET BUY               
         MVC   KEY,MKGKEY                                                       
*                                                                               
MGAPP290 DS    0H                                                               
         GOTO1 VHIGH                                                            
         B     MGAPP10                                                          
*                                                                               
MGAPP300 DS    0H                  IF DARE, SEND CONFIRMATION TO AGENCY         
         BAS   RE,DARECNF                                                       
*                                                                               
         TM    TWACFCFL,X'80'      NEED TO SET FLAG IN THE 'REAL' TWA           
         BZ    MGAPPX              TO DELETE CFC COMMENTS                       
*                                                                               
         L     RF,4(RD)            POINT TO D-CHAIN                             
         L     RF,60(RF)           RETRIEVE PREVIOUS RA                         
         AHI   RF,TWAWORKQ                                                      
RAD      USING TWAWORK,RF                                                       
         OI    RAD.TWACFCFL,X'80'   FLAG IN ORIGINAL TWA                        
         DROP  RAD                                                              
*                                                                               
MGAPPX   DS    0H                                                               
         B     EXXMOD              RA WILL BE RESTORED UPON EXIT                
         EJECT                                                                  
***********************************************************************         
* CHECK IF ANY 'OR' MAKEGOODS OFFERS IN A GROUP. IF FOUND, MAKE SURE            
* AT LEAST ONE RECORD WITHIN THE OFFER HAS BEEN SELECTED                        
*                                                                               
* ALSO PROCESS REJECT/RECALL BY UPDATING FLAGS IN HEADER REC                    
* AND REFRESH LIST WITH NEW STATUS                                              
*                                                                               
* NOTE THAT 'R' CAN BE REJECT OR RECALL DEPENDING ON WHO THE                    
* OFFERER IS AND WHO IS CURRENTLY LOOKING AT THE OFFER                          
***********************************************************************         
CHECKMG  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),TWAMKGD2  SET TO GET SELECTED RECORD                   
                                                                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
         XC    KEY,KEY                                                          
         MVC   KEY(RMKGKPLN-RMKGKEY),IOAREA                                     
         GOTO1 VHIGH               GET HEADER REC                               
*                                                                               
* CHECK OFFER STATUS IN GROUP COMMENT REC                                       
*                                                                               
         CLC   KEY(L'RMKGKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6          VALID ACTIONS ARE:                           
*                                  CHECK IF:                                    
         LA    R3,480              ERROR IF                                     
         TM    RMKGSCST,RMKGSAPQ   ALREADY APPLIED                              
         BO    CHKMGNO             ELSE,                                        
         CLI   8(R2),C'A'          APPLY?                                       
         BE    CHKMG140                                                         
         CLI   8(R2),C'R'          REJECT/RECALL?                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BE    CHKMG05                                                          
*                                                                               
* USER IS REP. IF MG IS A DARE MG AND MG WAS SENT TO AGENCY, WE MUST            
* CANCEL OR RECALL FROM AGENCY FIRST                                            
*                                                                               
         OC    RMKGSFG1,RMKGSFG1                                                
         BZ    CHKMG03                                                          
         LA    R3,588                                                           
         TM    RMKGSFG1,RMGF1MRR+RMGF1MCN+RMGF1MCM                              
         BZ    CHKMGNO                                                          
*                                                                               
CHKMG03  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS REP,                                 
         BO    CHKMG50             OFFERER IS REP: RECALL                       
         B     CHKMG10             OFFERER IS STA: REJECT                       
*                                                                               
CHKMG05  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS STATION,                             
         BZ    CHKMG50             OFFERER IS STA: RECALL                       
*                                  OFFERER IS REP: REJECT                       
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* FOR ACTION REJECT, ONLY THE OFFEREE CAN REJECT AND OFFERER IS NOT             
* WORKING ON THE OFFER                                                          
*                                                                               
CHKMG10  DS    0H                                                               
         LA    R6,RCONREC                                                       
         USING RCONMGEL,R6                                                      
         LA    R5,IOAREA                                                        
         USING RMKGREC,R5                                                       
*                                                                               
         LA    R3,481                                                           
         TM    RMKGSCST,RMKGSRJQ   ALREADY REJECTED                             
         BO    CHKMGNO                                                          
*                                                                               
         LA    R3,482                                                           
         TM    RMKGSCST,RMKGSRCQ   ALREADY RECALLED                             
         BO    CHKMGNO                                                          
*                                                                               
         LA    R3,501              OFFERER CANNOT REJECT A MGO                  
         CLI   TWAACCS,C'$'                                                     
         BE    CHKMG20                                                          
*                                                                               
* IF DARE, CURRENT DARE STATUS MUST BE CANCELLED OR RECALLED OR                 
* REJECTED FROM AGENCY                                                          
*                                                                               
         OC    RMKGSFG1,RMKGSFG1                                                
         BZ    CHKMG15                                                          
         TM    RMKGSFG1,RMGF1MCN+RMGF1MCM+RMGF1MRR                              
         BNZ   CHKMG15                                                          
         LA    R3,588                                                           
         B     CHKMGNO                                                          
*                                                                               
CHKMG15  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS REP,                                 
         BO    CHKMGNO             MUST BE OFFERED BY STATION                   
*                                                                               
         LA    R3,628              STATION MUST NOT BE WORKING ON THIS          
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    CHKMGNO                                                          
         B     CHKMG40                                                          
*                                                                               
CHKMG20  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS STATION,                             
         BZ    CHKMGNO             MUST BE OFFERED BY REP                       
*                                                                               
         LA    R3,629              REP MUST NOT BE WORKING ON THIS              
         TM    RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                       
         BO    CHKMGNO                                                          
*                                                                               
CHKMG40  DS    0H                                                               
         OI    RMKGSCST,RMKGSRJQ   MARK OFFER REJECTED                          
*                                                                               
         LR    RF,R2                                                            
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         MVC   12(3,RF),=C'REJ'     REFRESH SCREEN STATUS                       
         OI    6(RF),X'80'         XMIT                                         
*                                                                               
         B     CHKMG100                                                         
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
* FOR ACTION RECALL, ONLY THE OFFERER CAN RECALL AND OFFEREE MUST               
* NOT BE WORKING ON THE OFFER                                                   
*                                                                               
CHKMG50  DS    0H                                                               
         LA    R6,RCONREC                                                       
         USING RCONMGEL,R6                                                      
         LA    R5,IOAREA                                                        
         USING RMKGREC,R5                                                       
*                                                                               
         LA    R3,481                                                           
         TM    RMKGSCST,RMKGSRJQ   ALREADY REJECTED                             
         BO    CHKMGNO                                                          
*                                                                               
         LA    R3,482                                                           
         TM    RMKGSCST,RMKGSRCQ   ALREADY RECALLED                             
         BO    CHKMGNO                                                          
*                                                                               
         LA    R3,500              ONLY OFFERER CAN RECALL MGO                  
         CLI   TWAACCS,C'$'                                                     
         BE    CHKMG60                                                          
*                                                                               
* IF DARE, CURRENT DARE STATUS MUST BE CANCELLED OR RECALLED OR                 
* REJECTED FROM AGENCY                                                          
*                                                                               
         OC    RMKGSFG1,RMKGSFG1                                                
         BZ    CHKMG55                                                          
         TM    RMKGSFG1,RMGF1MCN+RMGF1MCM+RMGF1MRR                              
         BNZ   CHKMG55                                                          
         LA    R3,588                                                           
         B     CHKMGNO                                                          
*                                                                               
CHKMG55  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS REP,                                 
         BZ    CHKMGNO             MUST BE OFFERED BY REP                       
*                                                                               
         LA    R3,628              AND STA MUST NOT BE WORKING ON THIS          
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    CHKMGNO                                                          
         B     CHKMG70                                                          
*                                                                               
CHKMG60  DS    0H                                                               
         TM    RMKGSCST,RMKGSCRQ   USER IS STATION,                             
         BO    CHKMGNO             MUST BE OFFERED BY STATION                   
*                                                                               
         LA    R3,629              AND STA MUST NOT BE WORKING ON THIS          
         TM    RMKGSFG2,RMGF2RPQ+RMGF2WPQ                                       
         BO    CHKMGNO                                                          
*                                                                               
CHKMG70  DS    0H                  MARK OFFER RECALLED                          
         OI    RMKGSCST,RMKGSRCQ                                                
*                                                                               
         LR    RF,R2                                                            
         ZIC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         MVC   12(3,RF),=C'REC'     REFRESH SCREEN STATUS                       
         OI    6(RF),X'80'         XMIT                                         
*                                                                               
CHKMG100 DS    0H                  RECORD RECALL/REJECT DATE/TIME               
         GOTO1 DATCON,DMCB,(5,0),(2,RMKGSLRD)                                   
*                                                                               
         TIME  DEC                                                              
*                                  RETRIEVE TIME:  RETURNED IN RO               
*                                     AS HH:MM:SS:TT                            
         LA    R4,RMKGSLRT                                                      
         STCM  R0,4,1(R4)          STORE MINUTES IN SAVE AREA                   
         STCM  R0,2,2(R4)          STORE SECONDS IN SAVE AREA                   
         SRL   R0,24               SHIFT HOURS TO LOW-ORDER                     
         STC   R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,WORK+1,1,=C'TOG'                                
         PACK  DUB,WORK+1(2)                                                    
         CVB   R2,DUB                                                           
         LA    R2,DDSTMADJ(R2)     ADD ADJUSTMENT FOR DDS TIME                  
         EDIT  (R2),(2,WORK+17),FILL=0,ZERO=NOBLANK                             
         GOTO1 HEXIN,DMCB,WORK+17,(R4),2,0                                      
*                                                                               
* RECORD LAST DATE/TIME ACTIVITY ALSO                                           
*                                                                               
         MVC   RMKGSLAD,RMKGSLRD                                                
         MVC   RMKGSLAT,RMKGSLRT                                                
         DROP  R5,R6                                                            
*                                                                               
         GOTO1 VPUTREC,DMCB,IOAREA WRITE OUT HEADER RECORD                      
         B     CHKMGYES                                                         
         EJECT                                                                  
*                                                                               
* CHECK FOR ACTION APPLY                                                        
*                                                                               
CHKMG140 DS    0H                  ONLY REP CAN APPLY                           
         LA    R3,505                                                           
         CLI   TWAACCS,C'$'                                                     
         BE    CHKMGNO                                                          
*                                                                               
         LA    R6,RCONREC          FOR A DARE ORDER                             
         MVI   ELCODE,X'1D'        REP CAN APPLY APPROVAL RECEIVED FROM         
         BRAS  RE,GETEL            AGENCY                                       
         BNE   CHKMG143            OR REP CAN AUTO-APPLY THROUGH DARE           
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'02'      OK TO APPLY FOR EDI ONE-SHOT                 
         BO    CHKMG143                                                         
*                                                                               
         LA    R5,IOAREA           IN WHICH CASE THE GROUP IS MARKED AS         
         USING RMKGREC,R5          APPROVED                                     
*                                                                               
         OC    RMKGSFG1,RMKGSFG1   NEVER SENT TO AGENCY                         
         BZ    CHKMG143            OK TO APPLY                                  
*                                                                               
         TM    RCONDRFG,X'80'      IF LINKED TO DARE ORDER                      
         BZ    CHKMG142            MUST APPLY THRU DARE                         
*                                                                               
         TM    TWADARE,X'02'       APPLY FROM DARE?                             
         BO    CHKMG142            YES                                          
         LA    R3,616                                                           
         B     CHKMGNO                                                          
         DROP  R6                                                               
*                                                                               
CHKMG142 DS    0H                                                               
         LA    R3,520                                                           
         TM    RMKGSFG1,RMGF1MAR+RMGF1MCF                                       
         BZ    CHKMGNO                                                          
         DROP  R5                                                               
*                                                                               
CHKMG143 DS    0H                                                               
         TM    RCONMODR+1,X'40'    AND GRAPHNET, REP CAN APPLY AT               
         BO    CHKMG150            ANYTIME                                      
*                                                                               
         LA    R3,512              WAS ORDER REJECTED/RECALLED?                 
         LA    R5,IOAREA                                                        
         USING RMKGREC,R5                                                       
         TM    RMKGSCST,RMKGSRJQ+RMKGSRCQ                                       
         BNZ   CHKMGNO             MUST REVISE BEFORE APPLY                     
*                                                                               
         LA    R3,513              IS STATION IN WIP?                           
         TM    RMKGSFG2,RMGF2STQ+RMGF2WPQ                                       
         BO    ERROR                                                            
         DROP  R5                                                               
*                                                                               
CHKMG150 DS    0H                                                               
         GOTO1 VSEQ                                                             
                                                                                
CHKMG160 DS    0H                                                               
         LA    R3,466              UNSELECTED CHOICE OFFER DETECTED             
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   CHKMGYES            ALL DONE                                     
         OC    RMKGKPLN(6),RMKGKPLN SKIP COMMENT RECORDS                        
         BZ    CHKMG150                                                         
         TM    RMKGKRTY,X'10'      FIND 'CHOICE' RECORDS                        
         BZ    CHKMG150                                                         
         MVC   MGOFFNUM,RMKGKRTY   SAVE OFFER NUMBER                            
         MVI   MGORSELD,C'N'       DEFAULT NO RECORD SELECTED IN OFFER          
         DROP  R6                                                               
                                                                                
CHKMG170 DS    0H                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'20'        GET STATUS ELEMENT                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
                                                                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      SELECTED?                                    
         BZ    CHKMG180                                                         
         MVI   MGORSELD,C'Y'       YES, AT LEAST ONE HAS BEEN SELECTED          
         DROP  R6                                                               
*                                                                               
CHKMG180 DS    0H                                                               
         GOTO1 VSEQ                                                             
         CLC   KEY(RMKGKRTY-RMKGKEY),KEYSAVE                                    
         BE    CHKMG190            ALL DONE                                     
         CLI   MGORSELD,C'N'       ANYTHING SELECTED FROM PREV OFFER?           
         BE    CHKMGNO                                                          
         B     CHKMGYES                                                         
                                                                                
CHKMG190 DS    0H                                                               
         LA    R6,KEY                                                           
         USING RMKGKEY,R6                                                       
         MVC   WORK(1),RMKGKRTY                                                 
         NC    WORK(1),MGOFFNUM                                                 
         DROP  R6                                                               
                                                                                
         TM    WORK,X'0F'          CHECK IF SAME OFFER                          
         BO    CHKMG200            YES, SAME OFFER                              
         CLI   MGORSELD,C'Y'       IF AT LEAST ONE REC WAS SELECTED             
         BE    CHKMG180            DO SEQ UNTIL NEXT OFFER                      
         B     CHKMG170            ELSE CHECK IF REC SELECTED                   
                                                                                
CHKMG200 DS    0H                  DIFFERENT OFFER                              
         CLI   MGORSELD,C'N'       ANY RECORDS SELECTED FROM PREV OFF?          
         BE    CHKMGNO                                                          
         B     CHKMG160            YES, RESET AND START AGAIN                   
*                                                                               
CHKMGYES SR    R1,R1                                                            
         B     *+8                                                              
CHKMGNO  LA    R1,1                SET CONDITION CODE AT EXIT                   
         LTR   R1,R1                                                            
         XIT1  REGS=(R3)           SAVE POINTER TO ERROR MESSAGE                
         EJECT                                                                  
***********************************************************************         
* IF DARE, SEND CONFIRMATION TO AGENCY                                          
***********************************************************************         
DARECNF  NTR1                                                                   
*                                                                               
         MVC   KEY+28(4),TWAMKGDH  SET TO GET HEADER RECORD                     
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
         OC    RMKGSFG1,RMKGSFG1   NEVER SENT TO AGENCY                         
         BZ    DCNFX               DON'T NEED TO SEND ROK MESSAGE               
         DROP  R6                                                               
                                                                                
         LA    R6,RCONREC          FOR A DARE ORDER                             
         MVI   ELCODE,X'1D'        REP CAN APPLY APPROVAL RECEIVED FROM         
         BRAS  RE,GETEL            AGENCY                                       
         BNE   DCNFX               OR REP CAN AUTO-APPLY THROUGH DARE           
*                                                                               
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'+X'01'   MUST BE LINKED TO DARE ORDER              
         BZ    DCNFX               OR TAKEOVER DARE CONTRACT                    
         DROP  R6                                                               
*                                                                               
         L     RE,ASPULAR                                                       
         XCEF  (RE),6500                                                        
         BAS   RE,INITIAL                                                       
         BAS   RE,OPENPQ                                                        
         BAS   RE,EDICT                                                         
         GOTO1 =A(MKGROK),RR=Y                                                  
*                                                                               
         BAS   RE,DAREHIST         UPDATE DARE MAKEGOOD HISTORY                 
*                                                                               
DCNFX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
*              INITIALIZATION                                                   
***********************************************************************         
INITIAL  NTR1                                                                   
         L     R1,AFACILS                                                       
         LM    R2,R4,8(R1)                                                      
         ST    R3,ATIA                                                          
         MVC   SCANNER(16),24(R4)                                               
         LA    R2,BOOKVAL          FIND ADDRESSES OF CORE RESIDENT              
         SR    R3,R3               MODULES WITH PHONY CALLOV READS.             
         LA    R4,17                                                            
         SPACE 2                                                                
INIT2    DS    0H                                                               
         CH    R3,=H'9'                                                         
         BE    INIT2A                                                           
         CH    R3,=H'10'                                                        
         BE    INIT2A                                                           
         CH    R3,=H'11'                                                        
         BE    INIT2A                                                           
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         STC   R3,DMCB+7                                                        
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(R2)                                                         
INIT2A   LA    R3,1(R3)                                                         
         BCT   R4,INIT2                                                         
         GOTO1 DATCON,DMCB,(5,0),(10,RCDATE)                                    
*                                                                               
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   SPOOLBUF,ATIA                                                    
         XC    VPRINT,VPRINT                                                    
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
         B     EXXMOD                                                           
         EJECT                                                                  
**********************************************************************          
* OPEN THE PRINT QUEUE                                                          
**********************************************************************          
OPENPQ   NTR1                                                                   
         XC    SPOOLKEY,SPOOLKEY                                                
         OI    SPOOLIND,SPUINIT ALLOW USER VALUES-CLASS,LPP,COPIES              
         LA    R3,SPOOLKEY                                                      
         USING PQPLD,R3                                                         
         MVC   PLSUBID,=C'DMG'                                                  
         MVC   PLDESC,=CL11'MAKEGOOD'                                           
         MVI   PLCLASS,C'G'                                                     
         DROP  R3                                                               
                                                                                
VPQ20    DS    0H                                                               
*                                                                               
         LA    RE,TWASPKEY                                                      
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'XX'                                                     
         DROP  RE                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
***********************************************************************         
EDICT    NTR1                                                                   
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
                                                                                
         MVC   P+9(14),=C'EDICT=*DDSDARR'                                       
*                                                                               
         MVI   P+34,C'W'           WIDE REPORT - 132 CHARS                      
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PRINT A ++DDS CARD                                                            
*                                                                               
         LA    R3,P                                                             
         USING EDICTD,R3                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVI   EDISYST,C'D'        UPPER CASE 'D'                               
         MVC   EDIPROG,=C'MKG'     TYPE=MAKEGOOD                                
         MVC   EDIIDEN,=C'TRN'     TRANSACTION DATA                             
         DROP  R3                                                               
*                                  SEND SPECIAL PRINT LINE                      
EDICT50  DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
                                                                                
EDICTX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* UPDATE DARE MAKEGOOD HISTORY                                                  
***********************************************************************         
DAREHIST NTR1                                                                   
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+28(4),MGHDRDA   GET MKG HEADER REC                           
*                                                                               
         MVI   UPDATE,C'Y'                                                      
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
IOD      USING RMKGSDEM,R6                                                      
         MVI   IOD.RMKGSFG1,RMGF1MCF MARK APPIED, CLEAR ALL ELSE                
*                                                                               
         XC    WORK,WORK                                                        
                                                                                
         LA    R5,WORK                                                          
         USING RMKGATEM,R5                                                      
         MVI   RMKGATCD,X'02'      ELEMENT CODE                                 
         MVI   RMKGATLN,RMKGATLQ   LENGTH                                       
         MVC   RMKGATVR,IOD.RMKGSCVR SAVE OFF VERSION#/ACTION                   
         MVI   RMKGATAT,X'08'      SAVE OFF VERSION#/ACTION                     
* TODAY                                                                         
         GOTO1 DATCON,DMCB,(5,0),(2,RMKGATDT)                                   
                                                                                
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,RMKGATTM                                                    
         DROP  R5,IOD                                                           
                                                                                
         GOTO1 VADDELEM,DMCB,IOAREA,WORK                                        
*                                                                               
         GOTO1 VPUTREC,DMCB,IOAREA                                              
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* X'76' ELEMENTS WILL NO LONGER BE RETAINED UNTIL MAX SIZE OF BUYS IS           
* INCREASED 6/11/99                                                             
*                                                                               
* CHANGE CORRESPONDING X'66' ELEMENT TO X'76' IN TARGET BUYLINE                 
* ALSO UPDATES X'20' STATUS ELEMENT IN THE MAKE-GOOD RECORD                     
* NOTE:                                                                         
* ASSUMES UPDATED MISSED BUY RECORD IN IO3 AS SPECIFIED IN                      
* RECNT30                                                                       
***********************************************************************         
UPDTTGBY NTR1                                                                   
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+28(4),MGHDRDA   GET MKG HEADER REC                           
                                                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
IOD      USING RMKGSDEM,R6                                                      
         OI    IOD.RMKGSCST,RMKGSAPQ MARK APPIED, CLEAR ALL ELSE                
         NI    IOD.RMKGSCST,X'FF'-RMKGSBOQ-RMKGSRCQ-RMKGSRJQ-RMKGSRVQ           
         MVC   IOD.RMKGAPMN,RCONMOD RECORD CURRENT MOD NUM                      
*                                                                               
* GET LAST ACTIVITY DATE/TIME                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,IOD.RMKGSLAD)                               
*                                                                               
         TIME  DEC                                                              
*                                  RETRIEVE TIME:  RETURNED IN RO               
*                                     AS HH:MM:SS:TT                            
         LA    R4,IOD.RMKGSLAT                                                  
         STCM  R0,4,1(R4)          STORE MINUTES IN SAVE AREA                   
         STCM  R0,2,2(R4)          STORE SECONDS IN SAVE AREA                   
         SRL   R0,24               SHIFT HOURS TO LOW-ORDER                     
         STC   R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,WORK+1,1,=C'TOG'                                
         PACK  DUB,WORK+1(2)                                                    
         CVB   R2,DUB                                                           
         LA    R2,DDSTMADJ(R2)     ADD ADJUSTMENT FOR DDS TIME                  
         EDIT  (R2),(2,WORK+17),FILL=0,ZERO=NOBLANK                             
         GOTO1 HEXIN,DMCB,WORK+17,(R4),2,0                                      
         DROP  IOD                                                              
                                                                                
         GOTO1 VPUTREC,DMCB,IOAREA                                              
                                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
*                                                                               
         MVC   KEY+28(4),TWAMKGD2  SET TO GET SELECTED MG REC                   
                                                                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         L     R6,AIO3                                                          
         OC    0(L'RBUYKEY,R6),0(R6)                                            
         BZ    UPDTBY50            BONUS MKGD HAS NOTHING TO WRITE BACK         
         XC    KEY,KEY                                                          
         MVC   KEY,0(R6)                                                        
         GOTO1 VHIGH                                                            
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,(R6)   RE-READ UPDATED MISSED RECORD                
*                                                                               
UPDTBY05 DS    0H                                                               
         L     R6,AIO3                                                          
         USING RBMGMSEL,R6                                                      
         MVI   ELCODE,X'66'                                                     
         BRAS  RE,GETEL                                                         
         BNE   UPDTBY40                                                         
                                                                                
UPDTBY08 DS    0H                                                               
         LA    R5,IOAREA           FIND MAKE-GOOD'S CORRESPONDING               
         USING RMKGKEY,R5          MAKEGOOD BUY MISSED ELEMENT                  
                                                                                
UPDTBY10 DS    0H                                                               
         CLC   RMKGKGRP,RBMGMSGD   MUST BE SAME GROUP                           
         BE    UPDTBY30                                                         
         DROP  R5                                                               
                                                                                
UPDTBY20 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    UPDTBY10                                                         
         B     UPDTBY40            ALL DONE                                     
                                                                                
UPDTBY30 DS    0H                  SAVE OFF A COPY OF X'66'                     
         XC    WORK,WORK                                                        
         ZIC   R1,RBMGMSLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),RBMGMSEL                                                 
         MVI   WORK,X'76'          AND CHANGE IT TO A X'76'                     
         DROP  R6                                                               
*                                                                               
* REMOVE X'66' AND ADD IT BACK AS X'76'                                         
*                                                                               
         L     R5,AIO3                                                          
         GOTO1 VRECUP,DMCB,(C'R',(R5)),(R6),0,0                                 
*        GOTO1 VADDELEM,DMCB,(R5),WORK                                          
         B     UPDTBY05                                                         
*                                                                               
UPDTBY40 DS    0H                                                               
         GOTO1 VPUTREC,DMCB,AIO3                                                
*                                                                               
* UPDATE STATUS ELEMENT IN MAKE-GOOD RECORD                                     
*                                                                               
UPDTBY50 DS    0H                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
*                                                                               
         MVC   KEY+28(4),TWAMKGD2  SET TO GET SELECTED MG REC                   
                                                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         LA    R6,IOAREA                                                        
         USING RMKGSTEL,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RMKGSTST,C'A'       BUY WAS APPLIED                              
                                                                                
         LA    R5,RBUYREC                                                       
         USING RBUYREC,R5                                                       
         MVC   RMKGSTL#,RBUYKLIN   LINE NUMBER GENERATED FOR M/G                
                                                                                
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         DROP  R5,R6                                                            
                                                                                
UPDTBYX  DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R7                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFED                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE SPDARMKGDD                                                     
       ++INCLUDE REGENDAR                                                       
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
* LOCAL VARIABLES                                                               
MYWORKD  DSECT                                                                  
MKGKEY   DS    CL27                                                             
MGOFFNUM DS    X                                                                
MGORSELD DS    C                   'OR' MAKEGOOD OFFER AT LEAST 1 SELTD         
MGHDRDA  DS    F                   MAKEGOOD HEADER REC DISK ADDRESS             
MGSTATUS DS    X                   STATUS FLAG                                  
*                                                                               
MGTWA    DS    CL(TWAMAX)          18K OF MY OWN TWA                            
MYWORKX  EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* SET OVERALL FIELD HEADER LENGTHS                                              
***********************************************************************         
T80234   CSECT                                                                  
SETFDHDL NTR1  BASE=*,LABEL=*                                                   
         LA    R2,FDHDTBL                                                       
                                                                                
SETFDH10 DS    0H                                                               
         ZICM  RF,0(R2),2                                                       
         AR    RF,RA                                                            
         MVC   0(1,RF),2(R2)                                                    
         OI    1(RF),X'02'         SET EXTENDED FIELD HEADER                    
         LA    R2,L'FDHDTBL(R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BNE   SETFDH10                                                         
                                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET ACTUAL LENGTH OF DATA IN FIELDS                                           
***********************************************************************         
SETFLDLN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,FDHDTBL                                                       
                                                                                
SETFLD10 DS    0H                                                               
         ZICM  RF,0(R2),2                                                       
         LA    R1,8(RF,RA)         POINT TO FIELD                               
         ZIC   R0,3(R2)            LENGTH OF FIELD                              
*                                                                               
         TM    4(R2),X'04'         COMMENT??                                    
         BZ    SETFLD20            YES, POINT TO END OF FIELD                   
         AR    R1,R0               AND SCAN BACKWARDS                           
*                                                                               
SETFLD13 BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BE    SETFLD15                                                         
         CLI   0(R1),0                                                          
         BE    SETFLD15                                                         
         LA    R1,0(RF,RA)         POINT TO FIELD HEADER                        
         STC   R0,5(R1)            INSERT LENGTH, IF ANY                        
         B     SETFLD40                                                         
*                                                                               
SETFLD15 DS    0H                                                               
         BCT   R0,SETFLD13                                                      
         B     SETFLD40            NO LENGTH LEFT                               
*                                                                               
SETFLD20 DS    0H                                                               
         CLI   0(R1),C' '          SPACE                                        
         BE    SETFLD30                                                         
         CLI   0(R1),0             OR NULL IS SENTINEL                          
         BE    SETFLD30                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,SETFLD20                                                      
                                                                                
SETFLD30 DS    0H                                                               
         ZIC   RE,3(R2)            CALCULATE ACTUAL LENGTH OF DATA              
         SR    RE,R0                                                            
         LA    R1,0(RF,RA)         POINT TO FIELD HEADER                        
         STC   RE,5(R1)            INSERT LENGTH, IF ANY                        
         CLI   4(R2),0                                                          
         BE    SETFLD40                                                         
         TM    4(R2),X'08'                                                      
         BZ    SETFLD40                                                         
         OI    4(R1),X'08'         SET NUMERIC                                  
*                                                                               
SETFLD40 DS    0H                                                               
         LA    R2,L'FDHDTBL(R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BNE   SETFLD10                                                         
*                                                                               
SETFLX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEND MAKEGOOD OK NOTICE                                                       
***********************************************************************         
MKGROK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RCONKAGY                                                
         MVC   RAGK2AOF,RCONKAOF                                                
         MVC   RAGK2REP,RCONKREP                                                
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         LA    R5,KEY                                                           
         USING RDARKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'51'                                                   
         MVC   RDARKREP,REPALPHA                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,=20C' '                                                 
         MVC   RDARKAGY(5),RAGY2DAR   EQUIVALENCY CODE                          
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RDARKORD,RCONDRLK   ORDER NUMBER                                 
         MVI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
         DROP  R6                                                               
                                                                                
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,4                COMBINATIONS WE NEED TO CHECK                
                                                                                
MKGROK10 DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    MKGROK20                                                         
         CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
         LA    R4,5(R4)                                                         
         MVC   RDARKAGY(5),0(R4)   EQUIVALENCY CODE                             
         BCT   R3,MKGROK10                                                      
*                                                                               
* TEMPORARY CODING FOR FOX TV                                                   
*                                                                               
         CLC   =C'FN',REPALPHA                                                  
         BNE   MKGROK12                                                         
         CLC   =C'KTTV',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WFLD',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WTXF',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WFXT',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WNYW',RCONKSTA                                                
         BNE   MKGROK25                                                         
         B     MKGROK13                                                         
*                                                                               
MKGROK12 DS    0H                                                               
         CLC   =C'AM',REPALPHA                                                  
         BNE   MKGROK15                                                         
         CLC   =C'WMAR',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WCCB',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WAPT',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WAWS',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WTEV',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'KASN',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WFTC',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WGNT',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'KNXV',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WPRI',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'KUSI',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WNAC',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'KSPR',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'WFTS',RCONKSTA                                                
         BE    MKGROK13                                                         
         CLC   =C'KLRT',RCONKSTA                                                
         BNE   MKGROK25                                                         
*                                                                               
MKGROK13 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'51'                                                   
         MVC   RDARKREP,=C'PV'     IF FOX, LOOK IN PETRY                        
         CLC   =C'AM',REPALPHA                                                  
         BNE   MGROK13A                                                         
         MVC   RDARKREP,=C'NK'     IF AMERICA, LOOK IN NATIONAL                 
*                                                                               
MGROK13A DS    0H                                                               
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,=20C' '                                                 
         MVC   RDARKAGY(5),RAGY2DAR   EQUIVALENCY CODE                          
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RDARKORD,RCONDRLK   ORDER NUMBER                                 
         MVI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
         DROP  R6                                                               
                                                                                
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,4                COMBINATIONS WE NEED TO CHECK                
                                                                                
MKGROK14 DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    MKGROK20                                                         
         CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
         LA    R4,5(R4)                                                         
         MVC   RDARKAGY(5),0(R4)   EQUIVALENCY CODE                             
         BCT   R3,MKGROK14                                                      
*                                                                               
MKGROK15 DS    0H                                                               
         CLC   =C'SZ',REPALPHA                                                  
         BNE   MKGROK25                                                         
         CLC   =C'1342  ',RAGK2AGY AND AGENCY 1342 ONLY                         
         BNE   MKGROK25                                                         
         MVC   RDARKAGY(5),=C'ED2DE'                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   MKGROK25                                                         
         DROP  R5                                                               
*                                                                               
MKGROK20 DS    0H                                                               
         GOTO1 VGETREC,DMCB,AIO4                                                
         L     R5,AIO4                                                          
         B     MKGROK28            USE USER SIGNON AS RECEIVER ID               
*                                                                               
* CHECK IF TAKEOVER CONTRACT/DARE                                               
*                                                                               
MKGROK25 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'                                                     
         BRAS  RE,GETEL                                                         
         BNE   MKGROK50                                                         
         USING RCONTKEL,R6                                                      
*                                                                               
         L     R5,AIO4                                                          
         USING RDARREC,R5                                                       
*                                                                               
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'                                                  
         MVC   RDARKAGY,RCONTKAR   AGENCY ROUTING (FOR JDS)                     
         MVC   RDARSNDR,RCONTKRC   SENDER ID                                    
         MVC   RDARRTS,RCONTKRT    AGENCY RETURN TO SENDER INFO                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R6                                                      
         MVC   RDARKORD,RCONDRLK                                                
         DROP  R6                                                               
*                                                                               
* GET RECEIVER ID                                                               
*                                                                               
MKGROK28 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),TWAUSRID                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                      
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2                                                          
         CLC   KEY(25),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,28(R6)                                                        
MKGROK30 CLI   0(R6),X'02'         GET REP SIGN-ON ID                           
         BE    MKGROK40                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   MKGROK30                                                         
         DC    H'0'                                                             
*                                                                               
MKGROK40 DS    0H                                                               
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RDARRCVR(0),2(R6)                                                
         OC    RDARRCVR,=20C' '                                                 
         DROP  R5                                                               
*                                                                               
MKGROK50 DS    0H                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
                                                                                
         LA    R4,P                                                             
         USING MOFRCFMD,R4                                                      
                                                                                
* RECORD                                                                        
         MVC   MOCFTID,=C'MKGROK'                                               
                                                                                
* ORDER NUMBER                                                                  
         OC    RDARKORD,RDARKORD                                                
         BNZ   *+6                                                              
         DC    H'0'                DID NOT FIND HEADER REC, DIE!!               
*                                                                               
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RDARKORD                                                
         EDIT  (P5,WORK2),(8,MOCFORDR),FILL=0                                   
                                                                                
* ID OF SENDER                                                                  
         MVC   MOCFFRID,RDARRCVR                                                
                                                                                
* ID OF RECEIVER                                                                
         MVC   MOCFTOID,RDARSNDR                                                
                                                                                
* ROUTING CODE                                                                  
         MVC   MOCFROUT,RDARKAGY                                                
                                                                                
* CURRENT DATE                                                                  
         GOTO1 DATCON,DMCB,(5,0),(X'20',MOCFDATE)                               
                                                                                
* CURRENT TIME                                                                  
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,WORK                                                        
         GOTO1 HEXOUT,DMCB,WORK,MOCFTIME,2,0                                    
*                                                                               
* STATION                                                                       
         MVC   MOCFQSTA,RDARKSTA                                                
         CLI   MOCFQSTA+4,C'L'                                                  
         BE    MKGROK60                                                         
         MVI   MOCFQSTA+5,C'V'     TV OR RADIO?                                 
         CLI   MOCFQSTA+4,C'T'                                                  
         BE    *+8                                                              
         MVI   MOCFQSTA+5,C'M'                                                  
                                                                                
* CONTRACT NUMBER                                                               
MKGROK60 DS    0H                                                               
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RCONKCON                                                
         EDIT  (P5,WORK2),(8,MOCFRPCN),FILL=0                                   
                                                                                
* AGENCY 'RETURN TO SENDER' DATA                                                
         MVC   MOCFRTNS,RDARRTS                                                 
         DROP  R6                                                               
*                                                                               
         MVC   KEY+28(4),TWAMKGDH  SET TO GET HEADER RECORD                     
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         USING RMKGREC,R6                                                       
                                                                                
* OFFER ID                                                                      
         MVC   MOCFOFRI(2),RMKGKGR1                                             
         DROP  R6                                                               
                                                                                
* VERSION NUMBER WITHIN ID                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGSDEM,R6                                                      
                                                                                
         EDIT  RMKGSCVR,(2,MOCFSEQN),FILL=0                                     
                                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVI   SPMODE,X'FF'        CLOSE PQ AND EXIT                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         XIT1                                                                   
         DROP  R4,R6,R9                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* AL2 = OFFSET OF FIELD HEADER FROM TWAD                                        
* AL1 = LENGTH OF FIELD HEADER + FIELD + EXTENSION                              
* AL1 = LENGTH OF FIELD                                                         
* AL1 = ATTRIBUTES: X'08' MARK NUMERIC                                          
*                 : X'04' COMMENT FIELD, CALCULATE LENGTH FROM END              
FDHDTBL  DS    0XL5                                                             
         DC    AL2(BUYDAYSH-TWAD),AL1(BUYTIMSH-BUYDAYSH),AL1(L'BUYDAYS)         
         DC    X'00'                                                            
         DC    AL2(BUYTIMSH-TWAD),AL1(BUYLENH-BUYTIMSH),AL1(L'BUYTIMS)          
         DC    X'00'                                                            
         DC    AL2(BUYLENH-TWAD),AL1(BUYDTESH-BUYLENH),AL1(L'BUYLEN)            
         DC    X'08'                                                            
         DC    AL2(BUYDTESH-TWAD),AL1(BUYDTE2H-BUYDTESH),AL1(L'BUYDTES)         
         DC    X'00'                                                            
         DC    AL2(BUYDTE2H-TWAD),AL1(BUYTYPH-BUYDTE2H),AL1(L'BUYDTE2)          
         DC    X'00'                                                            
         DC    AL2(BUYTYPH-TWAD),AL1(BUYDPTH-BUYTYPH),AL1(L'BUYTYP)             
         DC    X'00'                                                            
         DC    AL2(BUYDPTH-TWAD),AL1(BUYCLSH-BUYDPTH),AL1(L'BUYDPT)             
         DC    X'00'                                                            
         DC    AL2(BUYCLSH-TWAD),AL1(BUYSECH-BUYCLSH),AL1(L'BUYCLS)             
         DC    X'00'                                                            
         DC    AL2(BUYSECH-TWAD),AL1(BUYPLNH-BUYSECH),AL1(L'BUYSEC)             
         DC    X'00'                                                            
         DC    AL2(BUYPLNH-TWAD),AL1(BUYNPWH-BUYPLNH),AL1(L'BUYPLN)             
         DC    X'00'                                                            
         DC    AL2(BUYNPWH-TWAD),AL1(BUYRATEH-BUYNPWH),AL1(L'BUYNPW)            
         DC    X'08'                                                            
         DC    AL2(BUYRATEH-TWAD),AL1(BUYCOM1H-BUYRATEH),AL1(L'BUYRATE)         
         DC    X'08'                                                            
         DC    AL2(BUYCOM1H-TWAD),AL1(BUYCOM2H-BUYCOM1H),AL1(L'BUYCOM1)         
         DC    X'04'                                                            
         DC    AL2(BUYCOM2H-TWAD),AL1(BUYORDCH-BUYCOM2H),AL1(L'BUYCOM2)         
         DC    X'04'                                                            
         DC    AL2(BUYORDCH-TWAD),AL1(BUYORD2H-BUYORDCH),AL1(L'BUYORDC)         
         DC    X'00'                                                            
         DC    AL2(BUYORD2H-TWAD),AL1(BUYLAST-BUYORD2H),AL1(L'BUYORD2)          
         DC    X'00'                                                            
         DC    X'FF'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'213RECNT34   01/18/01'                                      
         END                                                                    
