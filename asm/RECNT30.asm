*          DATA SET RECNT30    AT LEVEL 122 AS OF 12/09/02                      
*PHASE T80230C,*                                                                
*INCLUDE REGENPLN                                                               
         TITLE 'T80230 - RECNT30 - REPPAK CONTRACT BUCKET UPDATE'               
*******************************************************************             
*                                                                 *             
*       RECNT30 - T80230 - CONTRACT BUCKET UPDATE                 *             
*                                                                 *             
* --------------------------------------------------------------- *             
* REFER TO RECNTHIST FOR PAST HISTORY                             *             
*                                                                 *             
*                                                                 *             
* 09DEC02 SKU RADIO EDI                                           *             
* 26APR01 BU  ADD 'DAILY PACING'                                  *             
* 12APR01 RHV TRADE BUY CAN/DEL BUCKET UPDATE BUG                 *             
* 22OCT00 RHV SPORTS BUYS                                         *             
* 28MAR00 SKU EXTENDED BUY SCREEN SUPPORT                         *             
*             INCREASE REP TO SPOT TRANSFER LIMIT TO 169 SPOTS    *             
* 22NOV99 SKU SET CONCNUM NUMERIC                                 *             
* 25NOV98 JRD FIX REGENPLN PARAMETERS                             *             
* 03NOV97 JRD ALTERNATE BUCKETS                                   *             
* 24JUL97 SKU 4K CONTRACT SUPPORT                                 *             
* 07MAY97 SKU ZERO BUCKET BUG FIX                                 *             
* 03JAN96 SKU 2K ADDRESSIBLITY BUG FIX                            *             
* 06OCT95 SKU 2K CONTRACT SUPPORT                                 *             
*                                                                 *             
* --------------------------------------------------------------- *             
*                                                                 *             
* ADDRESS BLOCK FOR REGENBUC @ WORK+0                             *             
* ADDRESS BLOCK FOR REGENPLN @ WORK+20                            *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80230   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80230,R9,RR=R5                                                
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         L     RC,0(R1)            WORK                                         
*                                                                               
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         MVI   TRUFLAG,C'N'        SET 'NEED EC/SAR FLAG' TO 'NO'               
*                                                                               
*********************                                                           
** VALIDATE ACTION **                                                           
*********************                                                           
         CLC   =C'DEL',BUYACT                                                   
         BNE   *+12                                                             
         BAS   RE,CHKBUY                                                        
         B     BUCK02                                                           
*                                                                               
         CLC   =C'BUY',BUYACT                                                   
         BE    BUCK02                                                           
         CLC   =C'CHA',BUYACT                                                   
         BE    BUCK02                                                           
         BAS   RE,SBACT            SPORTS BUY ACTION?                           
         BE    BUCK02                                                           
         DC    H'0'                                                             
*                                                                               
BUCK02   DS    0H                  ACTION OK                                    
         GOTOX (RFCHKALT,VREPFACS),DMCB,(0,RCONREC),ACOMFACS                    
         MVC   BUCKFLGS(1),0(R1)                                                
         TM    TWAFLAGS,X'08'      REP USING 'DAILY PACING'?                    
         BNO   BUCK03              YES                                          
         OI    BUCKFLGS,X'08'      SET DAILY PACING CALL                        
BUCK03   EQU   *                                                                
*                                                                               
         MVC   WORK(4),VGTBROAD    BUILD TOTAL SPOTS, WEEKS AND $ IN            
         MVC   WORK+4(4),GETDAY    BUYREC                                       
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),DATCON                                                
         MVC   WORK+16(4),DATAMGR                                               
*                                                                               
         MVC   WORK+20(16),WORK    SET REGENPLN PARMS                           
         MVC   WORK+36(4),VRECUP                                                
*                                                                               
         TM    TWAFLAGS,X'08'      REP USING 'DAILY PACING'?                    
         BO    BUCK04              YES                                          
         GOTOX (RFGENBUC,VREPFACS),DMCB,RBUYREC,WORK2,WORK                      
         B     BUCK06                                                           
BUCK04   EQU   *                                                                
         GOTOX (RFGENBUC,VREPFACS),DMCB,RBUYREC,WORK2,(X'80',WORK)              
BUCK06   EQU   *                                                                
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL            SPOTPAK INTERFACE ELEMENT?                   
         BNE   BUCK20              NO                                           
*                                                                               
         TM    RBUYCOMB,X'80'      IS THIS A 'NA' RATE BUY?                     
         BZ    BUCK15                                                           
*                                                                               
         SR    R0,R0               YES, CHECK FOR MAX 50 SPOTS                  
         NI    RBUYCOMB,X'FF'-X'80' CALCULATE TOTAL SPOTS SINCE                 
         ZIC   R1,RBUYCOMB         NA RATE LINES DO NOT HAVE TOTAL SPTS         
         OI    RBUYCOMB,X'80'      CLEAR AND RESTORE COMBO FLAG                 
*                                  RBUYCOMB LESS HOB IS #SPOT                   
         ZIC   RE,RBUYTWKS                                                      
         MR    R0,RE               MULTIPLY R1(#SPT) & RE(TOTAL WKS)            
         B     BUCK18              R1 HAS PRODUCT(TOTAL SPTS)                   
*                                                                               
BUCK15   DS    0H                                                               
         LH    R1,RBUYTSPT                                                      
         LTR   R1,R1               IF NEGATIVE SKIP CHECK                       
         BM    BUCK20                                                           
BUCK18   CH    R1,=H'169'          MAX OF 169 SPOTS                             
         BNH   BUCK20                                                           
         L     R2,ABUYFH                                                        
         LA    R3,266                                                           
         B     ERROR                                                            
*                                                                               
BUCK20   EQU   *                                                                
*                                                                               
         XC    WORK2,WORK2                                                      
         MVI   DMOUTBTS,0          FOR PROPER RECOVERY                          
*                                                                               
* UPDATE OLD AND NEW MISSED RECORDS FOR MAKE-GOODS                              
*                                                                               
*              RBUYREC        =    NEW BUYREC                                   
*              IO2            =    OLD BUYREC                                   
*              IO3            =    NEW MISSED RECORD, IF ANY (UPDATED)          
*              IO4            =    OLD MISSED RECORD, IF ANY (UPDATED)          
*              SPOOLAR        =    OLD CONTRACT (FOR BUCKET COMPARE)            
*              SPOOLAR+2000   =    OLD CONTRACT (FOR LATER PUTREC)              
*                                                                               
* SAVE OLD K REC FOR BUCKET COMPARE - WRITE TEST                                
         L     R7,ASPULAR                                                       
         GOTO1 VMOVEREC,DMCB,RCONREC,(R7)                                       
*                                                                               
* CHECK IF MAKE-GOOD STATUS CHANGED                                             
         CLC   =C'CH',BUYACT       CHANGE?                                      
         BNE   K2                                                               
* CHANGE                                                                        
K1       MVI   BYTE,X'FF'                                                       
         L     R7,AIO2                                                          
         GOTO1 VCHECKEL,DMCB,(5,RBUYREC),(R7),BYTE                              
*                                                                               
         CLI   BYTE,0              0=NO CHANGE                                  
         BE    K3                                                               
*                                                                               
K2       L     R7,AIO3                                                          
         OC    0(27,R7),0(R7)      NEW MISSED RECORD?                           
         BZ    K2B                                                              
*                                                                               
* GET COPY OF NEW MISSED REC                                                    
*                                                                               
         MVC   KEY,0(R7)                                                        
         GOTO1 VREAD                                                            
         BAS   RE,CHECK                                                         
*                                                                               
* TAKE OUT OF K BUCKET                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
*                                                                               
*   TEST: REMOVE                                                                
**       LA    RF,WORK                                                          
**       LA    RE,RCONREC                                                       
**       DC    H'0'                                                             
*   TEST: REMOVE                                                                
*                                                                               
         GOTO1 BUCKUP,DMCB,(X'FF',IOAREA)                                       
*                                                                               
* ADD IN NEW MISSED RECORD                                                      
         GOTO1 (RF),(R1),0(R7)                                                  
*                                                                               
* WRITE NEW MISSED REC                                                          
         GOTO1 VMOVEREC,DMCB,0(R7),IOAREA                                       
         BAS   RE,CHKBUY                                                        
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
*                                                                               
K2B      L     R7,AIO4             OLD MISSED RECORD?                           
         OC    0(27,R7),0(R7)      OLD MISSED RECORD?                           
         BZ    K3                                                               
*                                                                               
* CHECK IF OLD MISSED RECORD SAME AS NEW MISSED REC                             
         L     RF,AIO3                                                          
         CLC   0(27,R7),0(RF)                                                   
         BE    K3                                                               
*                                                                               
* OLD MISSED REC DIFFERENT                                                      
* GET COPY OF OLD MISSED REC                                                    
         MVC   KEY,0(R7)                                                        
         GOTO1 VREAD                                                            
         BAS   RE,CHECK                                                         
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
*                                                                               
* TAKE COPY OUT OF K BUCKETS                                                    
*                                                                               
*   TEST: REMOVE                                                                
**       LA    RF,WORK                                                          
**       LA    RE,RCONREC                                                       
**       DC    H'0'                                                             
*   TEST: REMOVE                                                                
*                                                                               
         GOTO1 BUCKUP,DMCB,(X'FF',IOAREA)                                       
*                                                                               
* ADD IN CHANGED OLD MISSED REC                                                 
*        GOTO1 (RF),(R1),3000(R7)                                               
         GOTO1 (RF),(R1),0(R7)                                                  
*                                                                               
* WRITE CHANGED OLD MISSED REC BACK                                             
*        GOTO1 VMOVEREC,(R1),3000(R7),IOAREA                                    
         GOTO1 VMOVEREC,(R1),0(R7),IOAREA                                       
         BAS   RE,CHKBUY                                                        
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
*                                                                               
* WRITE OR ADD BUYREC                                                           
K3       DS    0H                                                               
         BAS   RE,BUY              ADD OR CHANGE BUYREC                         
*                                                                               
         CLC   =C'BU',BUYACT       NEW BUY?                                     
         BE    K200                                                             
         BAS   RE,SBACT            SPORTS NEW BUY?                              
         BE    K200                                                             
*                                                                               
* CHANGE AND DELETE                                                             
         L     R6,AIO2             OLD PLAN                                     
         CLC   22(3,R6),=3X'FF'    OLD BUYREC A PLAN?                           
         BE    K95                                                              
*                                                                               
* OLDREC A PLAN - GET OLD PLNREC                                                
         MVC   KEY,0(R6)                                                        
         MVC   KEY+25(2),=2X'FF'                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   K100                OLD PLNREC STILL THERE? (ERROR)              
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
*                                                                               
* SUBTRACT OLD PLNREC FROM CONREC                                               
         GOTO1 VMOVEREC,DMCB,IOAREA,(R6)                                        
*                                                                               
*   TEST: REMOVE                                                                
**       LA    RF,WORK                                                          
**       LA    RE,RCONREC                                                       
**       DC    H'0'                                                             
*   TEST: REMOVE                                                                
*                                                                               
         GOTO1 BUCKUP,DMCB,(X'FF',(R6))                                         
*                                                                               
* SAVE DISK ADDR OF OLD PLAN REC                                                
         L     R7,AIO3                                                          
         MVC   SVDAIN30,KEY+28                                                  
         GOTO1 VMOVEREC,DMCB,(R6),(R7)                                          
*                                                                               
* BUILD NEW PLNREC BASED ON OLD BUYREC PLN IN R7 AREA                           
* DELETE ALL X'03' ELEMENTS                                                     
         GOTO1 VDELELEM,(R1),(3,(R7))                                           
         MVI   BYTE3,0             ACTIVITY                                     
         MVC   KEY(25),0(R6)                                                    
         XC    KEY+25(2),KEY+25                                                 
         GOTO1 VHIGH                                                            
         B     K6                                                               
         SPACE 1                                                                
K5       GOTO1 VSEQ                                                             
K6       CLC   KEY(25),KEYSAVE     SAME PLAN?                                   
         BNE   K10                                                              
         CLI   KEY+26,255          PLAN REC?                                    
         BE    K10                                                              
*                                                                               
* BUILD PLAN WEEK-DATE ELEMENTS                                                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
         GOTO1 =V(REGENPLN),DMCB,IOAREA,(R7),WORK+20,RR=YES                     
         MVI   BYTE3,1             ACTIVITY IND                                 
         B     K5                                                               
*                                                                               
* ADD CHANGED OLD PLANREC TO CONREC                                             
K10      EQU   *                                                                
*                                                                               
*   TEST: REMOVE                                                                
**       LA    RF,WORK                                                          
**       LA    RE,RCONREC                                                       
**       DC    H'0'                                                             
*   TEST: REMOVE                                                                
*                                                                               
         GOTO1 BUCKUP,DMCB,(R7)                                                 
*                                                                               
* BUCKUP ALSO UPDATES PLANREC SPOT AND $ TOTALS                                 
* CHECK IF OLD PLANREC NEEDS TO BE CHANGED                                      
         LA    RE,RBUYCOS-RBUYREC(R6)                                           
         LA    RF,RBUYCOS-RBUYREC(R7)                                           
         CLC   0(4,RE),0(RF)       SAME COST?                                   
         BNE   K12                                                              
         MVI   BYTE,X'FF'                                                       
         GOTO1 VCHECKEL,DMCB,(3,(R7)),(R6),BYTE                                 
         CLI   BYTE,0              0=NO CHANGE                                  
         BE    K100                                                             
         CLI   BYTE3,0             ACTIVITY?                                    
         BNE   K12                                                              
         OI    29(R7),X'80'        DELETE                                       
*                                                                               
         MVC   RBUYCHGI-RBUYREC(2,R7),=C'X '                                    
*                                                                               
         MVC   KEY,0(R7)                                                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VREAD                                                            
         BAS   RE,CHECK                                                         
         OI    KEY+27,X'80'                                                     
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
* UPDATE PASSIVES                                                               
         GOTO1 VLOAD,DMCB,(X'19',0),(R7)                                        
*                                                                               
K12      LA    RE,RBUYCHGD-RBUYREC(R7)                                          
         MVC   0(3,RE),TODAY                                                    
*                                                                               
         MVC   KEY+28(4),SVDAIN30  DISK ADDR                                    
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
         GOTO1 VMOVEREC,DMCB,(R7),IOAREA                                        
         BAS   RE,CHKBUY                                                        
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
*                                                                               
* ADD CHANGED OLD PLNREC TO CONREC                                              
         B     K100                                                             
         EJECT                                                                  
*                                                                               
* OLD BUYREC NOT A PLAN                                                         
K95      L     R6,AIO2                                                          
*                                                                               
* SUBTRACT OLD BUYREC FROM CONREC                                               
         GOTO1 BUCKUP,DMCB,(X'FF',(R6))                                         
*                                                                               
*   TEST: REMOVE                                                                
**       LA    RF,WORK                                                          
**       LA    RE,RCONREC                                                       
**       DC    H'0'                                                             
*   TEST: REMOVE                                                                
*                                                                               
*                                                                               
* TEST IF BUYREC DELETE                                                         
K100     CLC   =C'DEL',BUYACT                                                   
         BE    K350                                                             
*                                                                               
* CHECK IF NEW BUYREC A PLAN                                                    
K200     CLC   RBUYKPLN,=3X'FF'                                                 
         BNE   K205                                                             
*                                                                               
*   TEST: REMOVE                                                                
**       LA    RF,WORK                                                          
**       LA    RE,RCONREC                                                       
**       DC    H'0'                                                             
*   TEST: REMOVE                                                                
*                                                                               
         GOTO1 BUCKUP,DMCB,RBUYREC                                              
         B     K350                                                             
*                                                                               
* NEWREC A PLAN                                                                 
K205     CLC   =C'BU',BUYACT                                                    
         BE    K210                                                             
         BAS   RE,SBACT                                                         
         BE    K210                                                             
         L     R6,AIO2                                                          
         CLC   RBUYKPLN,22(R6)     PLAN CHANGE?                                 
         BE    K350                IF SAME NEW PLAN ALREADY CREATED             
*                                                                               
* NEW PLAN AFTER CHANGE OR NEW BUY                                              
* CHECK FOR PLNREC                                                              
K210     MVC   KEY,RBUYREC                                                      
         MVC   KEY+25(2),=2X'FF'                                                
         GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BNE   K275                                                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
*                                                                               
* PLAN REC EXISTS - SUBTRACT FROM CONREC                                        
*                                                                               
*   TEST: REMOVE                                                                
**       LA    RF,WORK                                                          
**       LA    RE,RCONREC                                                       
**       DC    H'0'                                                             
*   TEST: REMOVE                                                                
*                                                                               
         GOTO1 BUCKUP,DMCB,(X'FF',IOAREA)                                       
*                                                                               
* ADD TO PLAN REC                                                               
         GOTO1 =V(REGENPLN),(R1),RBUYREC,IOAREA,WORK+20,RR=YES                  
*                                                                               
* CHECK FOR CHANGE                                                              
         MVC   BYTE,DMCB+4         CHANGE IND                                   
*                                                                               
* ADD NEW PLANREC TO CONREC AND UPDATE PLANREC SPOTS AND $                      
*                                                                               
*   TEST: REMOVE                                                                
**       LA    RF,WORK                                                          
**       LA    RE,RCONREC                                                       
**       DC    H'0'                                                             
*   TEST: REMOVE                                                                
*                                                                               
         GOTO1 BUCKUP,(R1),IOAREA                                               
         CLI   BYTE,0                                                           
         BE    K350                                                             
*                                                                               
* WRITE PLAN BACK                                                               
         LA    R7,IOAREA                                                        
         LA    R7,RBUYCHGD-RBUYREC(R7)                                          
         MVC   0(3,R7),TODAY                                                    
         BAS   RE,CHKBUY                                                        
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
         B     K350                                                             
         EJECT                                                                  
*                                                                               
* BUILD NEW PLAN REC                                                            
K275     XC    IOAREA(255),IOAREA                                               
         MVC   IOAREA(25),RBUYREC                                               
         MVC   IOAREA+25(2),=2X'FF'                                             
         MVI   IOAREA+28,77        REC LEN                                      
         MVC   IOAREA+34(2),=X'012B'                                            
*                                                                               
* NPW                                                                           
         LA    R7,IOAREA                                                        
         LA    RF,RBUYNW-RBUYREC(R7)                                            
         MVI   0(RF),1                                                          
*                                                                               
* CREATION DATE                                                                 
         LA    RF,RBUYCREA-RBUYREC(R7)                                          
         MVC   0(3,RF),TODAY                                                    
         GOTO1 =V(REGENPLN),DMCB,RBUYREC,IOAREA,WORK+20,RR=YES                  
*                                                                               
* ADD NEW PLAN TO CONREC                                                        
*                                                                               
*   TEST: REMOVE                                                                
**       LA    RF,WORK                                                          
**       LA    RE,RCONREC                                                       
**       DC    H'0'                                                             
*   TEST: REMOVE                                                                
*                                                                               
         GOTO1 BUCKUP,(R1),IOAREA                                               
*                                                                               
* ADD PLAN REC                                                                  
* CHECK IF POINTER ALREADY THERE                                                
         MVC   KEY,IOAREA                                                       
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(27),KEYSAVE                                                  
         BE    K285                                                             
*                                                                               
* NO PREVIOUS POINTER FOR PLAN                                                  
         BAS   RE,CHKBUY                                                        
         GOTO1 VADDREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
* UPDATE BUY PASSIVE POINTER                                                    
         MVC   KEYSAVE+28(4),KEY                                                
         MVC   KEY,KEYSAVE                                                      
         GOTO1 VLOAD,DMCB,(X'19',0),IOAREA                                      
         B     K350                                                             
*                                                                               
* POINTER ALREADY EXISTS - USE OLD POINTER WITH NEW DISK ADDR                   
         BAS   RE,CHKBUY                                                        
K285     GOTO1 VADDREC,DMCB,IOAREA                                              
         MVC   KEYSAVE+28(4),KEY   NEW DISK ADDR                                
         MVC   KEY,KEYSAVE                                                      
         NI    KEY+27,X'3F'                                                     
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
* UPDATE BUY PASSIVE POINTER                                                    
         GOTO1 VLOAD,DMCB,(X'19',0),IOAREA                                      
         EJECT                                                                  
*                                                                               
* WRITE CONREC BACK                                                             
K350     DS    0H                                                               
         FOUT  CONMODH,MYSPACES,11                                              
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    K390                                                             
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED, SHOW MOD NUM.                     
         BO    K390                                                             
         SPACE 1                                                                
         ZIC   R3,1(R6)                                                         
         AR    R6,R3               GET NEXT ELEMENT                             
         SPACE 1                                                                
         CLI   0(R6),X'20'         SHOULD BE SEND ELEMENT                       
         BE    K355                                                             
         DC    H'0',C'MISSING X20 - SEND ELEM'                                  
         SPACE 1                                                                
         USING RCONSEND,R6                                                      
                                                                                
K355     DS    0H                                                               
         CLC   RCONSRV,RCONSSV     SHOW LATEST VERSION                          
         BH    K360                                                             
         EDIT  (1,RCONSSV),(3,CONMOD+8),ALIGN=LEFT                              
         B     K365                                                             
K360     EDIT  (1,RCONSRV),(3,CONMOD+8),ALIGN=LEFT                              
                                                                                
K365     DS    0H                                                               
         TM    RCONMODR+1,X'40'    GRAPHNET?                                    
         BZ    K370                                                             
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        RADIO EDI USES WIP                           
         BAS   RE,GETEL                                                         
         BNE   K368                                                             
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'                                                   
         BO    K370                                                             
         DROP  R6                                                               
*                                                                               
K368     DS    0H                                                               
         MVC   CONMOD(7),=C'ESL VER'                                            
         B     K400                                                             
                                                                                
K370     DS    0H                  DISPLAY WORK IN PROGRESS                     
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
*                                                                               
         MVC   CONMOD+4(3),=C'VER'                                              
         TM    RCONSENF,X'10'+X'20' SHOW IF WIP                                 
         BO    K400                                                             
         MVC   CONMOD(3),=C'WIP'                                                
         B     K400                                                             
         DROP  R6                                                               
                                                                                
K390     CLI   RCONMOD,0           K MOD NUM                                    
         BE    K400                                                             
         MVC   CONMOD(7),=C'MOD NUM'                                            
         EDIT  (1,RCONMOD),(3,CONMOD+8),ALIGN=LEFT                              
*                                                                               
* CHECK IF CONREC CHANGE                                                        
K400     DS    0H                                                               
*                                                                               
*   FIRST CHECK TO SEE IF ANY BUCKET CHANGE TO THE CONTRACT HAS                 
*      TAKEN PLACE:                                                             
*                                                                               
         MVI   BYTE,X'FF'                                                       
*        LA    R7,RBUYREC                                                       
*        LA    R7,4000(R7)         OLD CONREC                                   
         L     R7,ASPULAR          OLD CONREC                                   
         GOTO1 VCHECKEL,DMCB,(3,RCONREC),(R7),BYTE      CASH BUCKETS            
         CLI   BYTE,0              0=NO CHANGE                                  
         BNE   K405                NOT 0 = CHANGED:  UPDATE IT                  
*                                     ALONG WITH TRUE ACTIVITY DATE             
         GOTO1 VCHECKEL,DMCB,(X'63',RCONREC),(R7),BYTE  TRADE BUCKETS           
         CLI   BYTE,0              0=NO CHANGE                                  
         BNE   K405                NOT 0 = CHANGED:  UPDATE IT                  
*                                     ALONG WITH TRUE ACTIVITY DATE             
*                                                                               
*   NO BUCKET CHANGE HAS OCCURRED.  HOWEVER, CERTAIN CONDITIONS                 
*      REQUIRE THAT CONTRACT BE UPDATED ANYWAY.  TEST THEM NEXT.                
*      THEY REQUIRE NO TRUE ACTIVITY DATE UPDATE.                               
*                                                                               
         TM    TAREQ,1             T/A REQ IND?                                 
         BO    K410                                                             
         TM    BYTE4,X'02'         ACE/GRAPHNET CONTRACT UPDATED?               
         BO    K410                                                             
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    K410                NO - 'OTHER' UPDATE ALWAYS                   
*                                                                               
*   CONDITIONS NOT MET, AND NO BUCKET CHANGES:  DON'T REWRITE                   
*      CONTRACT RECORD.                                                         
*                                                                               
         B     EXXMOD                                                           
*                                                                               
*   ESTIMATE BUCKETS HAVE BEEN CHANGED.  IT IS THEREFORE NECESSARY              
*     TO UPDATE THE TRUE ACTIVITY DATE IN THE CONTRACT RECORD.                  
*                                                                               
K405     EQU   *                                                                
*                                                                               
         BAS   RE,TRUDATE          UPDATE TRUE ACTIVITY DATE                    
*                                                                               
* MOVE K REC TO IOAREA                                                          
K410     DS    0H                                                               
         MVC   KEY+28(4),TWAKADDR                                               
*                                                                               
* GET CONTRACT                                                                  
         CLI   TWACOMBO,0          IF COMBO ONLY THE FIRST COMBO K              
         BE    K420                NEEDS TO BE CHECKED                          
         CLI   TWACMBPT,1                                                       
         BNE   K430                                                             
*                                                                               
K420     LA    R2,CONCNUMH         CHECK THE CONTRACT NUMBER ON                 
         OI    CONCNUMH+4,X'08'    SET NUMERIC                                  
         GOTO1 (RFCONNUM,VREPFACS),DMCB,(4,CONCNUMH),(1,FULL)                   
***>     GOTO1 VPACK                                                            
***>     SRP   DUB+3(5),1,0        SCREEN VS. NUMBER ON RECORD                  
***>     CLC   RCONKCON,DUB+3      BEFORE WRITING BACK THE CONTRACT             
         CLC   RCONKCON,FULL       BEFORE WRITING BACK THE CONTRACT             
         BE    *+6                                                              
         DC    H'0'                                                             
K430     MVI   UPDATE,C'Y'                                                      
*                                                                               
         L     R7,ASPULAR          NEED TO PUT IT SOMEWHERE                     
         LA    R7,2000(R7)                                                      
*                                                                               
         GOTO1 VGETREC,DMCB,(R7)                                                
         BAS   RE,CHECK                                                         
         MVI   DMOUTBTS,0                                                       
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         BAS   RE,CHECK                                                         
         CLI   RCONKGRP,C'T'       IS CONTRACT FOR TV?                          
         BNE   EXXMOD              NO  -                                        
         CLI   TRUFLAG,C'Y'        YES - NEED 'EC/SAR' KEY?                     
         BNE   EXXMOD              NO  -                                        
         BAS   RE,GENECKEY                                                      
*                                  YES - GEN SAR KEY IF NOT PRESENT             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* CHECK FOR VALID SPORTS BUY TYPES                                              
*                                                                               
SBACT    NTR1                                                                   
         GOTO1 (RFGETTAB,VREPFACS),DMCB,('RTSPORTS',0)                          
         BE    *+6                                                              
         DC    H'0'                          MUST BE THERE                      
         L     R1,0(R1)                                                         
         MVC   HALF,0(R1)                    TABLE ENTRY LENGTH                 
         LA    R1,4(R1)                      START OF TABLE                     
SBACT1   DS    0H                                                               
         CLI   0(R1),X'FF'                   BUYACT NOT MATCHED                 
         BNE   *+10                                                             
         LTR   RB,RB                                                            
         B     XIT                           CC NOT =                           
         CLC   0(3,R1),BUYACT                                                   
         BE    XIT                           CC =                               
         AH    R1,HALF                                                          
         B     SBACT1                                                           
*                                                                               
GENECKEY NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'EC'           ESTABLISH SAR KEY                            
*                                                                               
         MVC   KEY+23(4),TWACNUM                                                
         MVC   KEY+21(2),REPALPHA                                               
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,0                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH               READ KEY                                     
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BE    XIT                 ALREADY THERE - DON'T READD                  
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),TWAKADDR                                               
*                                                                               
         GOTO1 VADD                                                             
         BAS   RE,CHECK                                                         
XIT      EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
BUY      NTR1                                                                   
         CLC   =C'DEL',BUYACT                                                   
         BE    BUYXIT                                                           
*                                                                               
         BAS   RE,SBACT                                                         
         BE    K450                                                             
         CLC   =C'BU',BUYACT                                                    
         BNE   K500                                                             
*                                                                               
* ADD BUYREC                                                                    
K450     GOTO1 VMOVEREC,DMCB,RBUYREC,IOAREA                                     
         BAS   RE,CHKBUY                                                        
*                                                                               
         CLI   TWACOMBO,0          IF COMBO ORDER, MOVE THE STATION             
         BE    K460                CALLS CURRENTLY IN RCONREC TO                
*                                                                               
         LA    R6,IOAREA           THE STATION FIELD IN SPOTPAK INTERF          
         MVI   ELCODE,X'08'        OTHERWISE, ALL BUYS IN THIS COMBO            
         BAS   RE,GETEL            WILL HAVE ONLY THE FIRST STATION             
         BNE   K460                COMBO CALLS !!!!!!                           
IOD      USING RBUYSPEL,R6         ELEMENT                                      
         MVC   IOD.RBUYSPST,RCONKSTA                                            
         DROP  IOD                                                              
*                                                                               
K460     DS    0H                                                               
         GOTO1 VADDREC,DMCB,IOAREA                                              
         MVC   TWABADDR,KEY        SAVE DISK ADDR                               
         BAS   RE,CHECK                                                         
*                                                                               
         GOTO1 =A(BUYCODE),DMCB,(R4),RR=Y                                       
*                                                                               
* UPDATE BUY PASSIVE POINTER                                                    
         MVC   KEYSAVE,IOAREA                                                   
         MVC   KEYSAVE+28(4),KEY                                                
         MVC   KEY,KEYSAVE                                                      
         GOTO1 VLOAD,DMCB,(X'19',0),IOAREA                                      
BUYXIT   DS    0H                                                               
         B     XIT                                                              
*                                                                               
CHECK    TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                FOR RECOVERY                                 
*                                  -TO MAKE SURE TRANSACTION IS                 
*                                  UNWOUND                                      
* CHANGE                                                                        
K500     L     R6,AIO2             OLD BUYREC                                   
*                                                                               
         CLC   RBUYKEY,0(R6)       KEY CHANGE (PLAN OR MAKE-GOOD)               
         BE    K600                                                             
*                                                                               
* DELETE OLD   RECORD AND ADD NEW POINTER AND NEW REC WITH NEW KEY              
         MVC   KEY,0(R6)           OLD KEY                                      
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VREAD                                                            
         BAS   RE,CHECK                                                         
         OI    KEY+27,X'C0'        DELETE + VOID INDICATORS                     
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
* UPDATE BUY PASSIVE  POINTERS                                                  
         GOTO1 VLOAD,DMCB,(X'19',0),(R6)                                        
*                                                                               
* DELETE OLD BUYREC                                                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         BAS  RE,CHECK                                                          
         GOTO1 VMOVEREC,DMCB,(R6),IOAREA                                        
         OI    IOAREA+29,X'C0'                                                  
*                                                                               
         BAS   RE,CHKBUY                                                        
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
*                                  ADD BUYREC WITH NEW KEY                      
* CHECK IF NEW POINTER ALREADY THERE                                            
         MVC   KEY,RBUYREC         NEW KEY                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(27),KEYSAVE                                                  
         BNE   K450                IF NOT GO TO ADDREC                          
*                                                                               
* POINTER ALREADY THERE                                                         
         GOTO1 VMOVEREC,DMCB,RBUYREC,IOAREA                                     
         BAS   RE,CHKBUY                                                        
*                                                                               
         GOTO1 VADDREC,DMCB,IOAREA                                              
         TM    DMCB+8,X'DF'        DUPLICATE KEY OK                             
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   TWABADDR,KEY                                                     
*                                                                               
* DUPLICATE KEY                                                                 
         MVC   KEYSAVE+28(4),KEY   NEW DISK ADDR                                
         MVC   KEY,KEYSAVE                                                      
         NI    KEY+27,X'3F'        TURN OFF DELETE, IF ANY                      
*                                                                               
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
*                                                                               
         GOTO1 =A(BUYCODE),DMCB,(R4),RR=Y                                       
*                                                                               
* UPDATE BUY PASSIVE POINTER                                                    
         GOTO1 VLOAD,DMCB,(X'19',0),IOAREA                                      
         B     BUYXIT                                                           
*                                                                               
* CHANGE BUYREC                                                                 
K600     DS    0H                                                               
         MVC   KEY+28(4),TWABADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,RBUYREC,IOAREA                                     
*                                                                               
         BAS   RE,CHKBUY                                                        
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         BAS   RE,CHECK                                                         
*                                                                               
         GOTO1 =A(BUYCODE),DMCB,(R4),RR=Y                                       
*                                                                               
         B     BUYXIT                                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
* ROUTINE TO ADD REC TO CONTRACT   P1=A(BUYREC OR PLNREC)                       
*                                     IF BYTE 0=X'FF'-SUBTRACTION               
*        IF 'DAILY PACING' IS REQUESTED, CALLS TO REPFACS BUCKUP                
*        ROUTINE REQUIRE THE INCLUSION OF P6 AS AN ADDRESS FOR A                
*        WORK AREA.                                                             
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
BUCKUP   NTR1                                                                   
         L     R2,0(R1)            A(BUYREC)                                    
         L     R0,VRECUP                                                        
BUUP0020 EQU   *                                                                
         LA    R5,TWABYTOT         CALCULATE A(WORKSPACE)                       
         AH    R5,=Y(TWABYTRD-TWABYTOT)                                         
*                                                                               
*   NOTE:  THIS IS IN 'TRADE BUCKET TOTAL' AREA IN THE TWA, WHICH               
*        IS NOT USED WITHIN THIS MODULE.                                        
*                                                                               
         GOTOX (RFBUCKUP,VREPFACS),DMCB,(R2),(BUCKFLGS,RCONREC),       +        
               ACOMFACS,VGTBROAD,(R0),(R5)                                      
         BE    BUUP0040                                                         
*                                                                               
         L     R2,ABUYFH                                                        
*                                                                               
         L     R3,0(R1)                                                         
         GOTO1 GETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                          
         DC    H'0',C'$ABEND'                                                   
BUUP0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------*         
*  TRUDATE:  FOR CONTRACTS WHERE THE BUYLINES HAVE RESULTED IN BUCKET           
*     CHANGES, FIND (ADD IF NOT PRESENT) THE X'08' ELEMENT, AND                 
*     UPDATE THE TRUE ACTIVITY DATE FOR SAR REPORTING                           
*---------------------------------------------------------------------*         
TRUDATE  NTR1                                                                   
         LA    R2,RCONELEM         A(1ST ELEMENT)                               
TDAT0010 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    TDAT0030            YES - NO X'08' FOUND - ADD IT                
         CLI   0(R2),X'08'         X'08'?                                       
         BNE   TDAT0010            NO  - GO BACK FOR NEXT                       
TDAT0020 EQU   *                   YES - ADD TODAYS DATE                        
         USING RCONACEL,R2                                                      
         GOTO1 DATCON,DMCB,(5,RCONACTA),(3,RCONACTA)                            
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         DROP  R2                                                               
*                                                                               
         B     TDAT0040            FINISHED                                     
TDAT0030 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,RCONDATE),(3,TDATELT+5)                           
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         GOTO1 VADDELEM,DMCB,RCONREC,TDATELT                                    
TDAT0040 EQU   *                                                                
         MVI   TRUFLAG,C'Y'        SET 'NEED EC KEY' FLAG                       
         B     XIT                                                              
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
TDATELT  DC    XL12'080C00000000000000000000'                                   
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO VERIFY BUY CONTRACT NUMBER AGAINST CONTRACT IN                 
* CORE BEFORE UPDATING THE FILE                                                 
CHKBUY   ST    RE,DMCB2                                                         
         LA    RE,IOAREA                                                        
*                                                                               
         USING RBUYKEY,RE                                                       
         ZAP   WORK3(5),=P'99999999'                                            
         PACK  WORK3+10(1),RBUYKCON+3(1)     INVERT THE SEQUENCE                
         PACK  WORK3+11(1),RBUYKCON+2(1)     OF THE DIGITS                      
         PACK  WORK3+12(1),RBUYKCON+1(1)                                        
         PACK  WORK3+13(1),RBUYKCON(1)       MOVE IN SIGN AND                   
         DROP  RE                                                               
*                                                                               
         MVI   WORK3+14,X'0C'                SHIFT ONE DECIMAL PLACE            
         SRP   WORK3+10(5),64-1,0            TO RIGHT                           
         SP    WORK3(5),WORK3+10(5)          BEFORE SUBTRACTING FROM            
         SRP   WORK3(5),1,0                  NINES AND SHIFTING 1 TO            
         CLC   RCONKCON,WORK3                LEFT AND DUMPING IF                
         BE    *+6                           RESULT NEQ TO K NUMBER             
         DC    H'0'                                                             
         L     RE,DMCB2                                                         
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
NEWCSECT CSECT                                                                  
BUYCODE  NTR1  LABEL=*,BASE=*                                                   
         L     R4,0(R1)            RESET A(TWA)                                 
         TM    PROFILES+CNTFOXSB,CNTFOXSA                                       
*                                  BUYLINE CODE FIELD TURNED ON?                
         BZ    BCOD0240            NO  - EXIT                                   
         CLC   ORIGBYCD,=C'   '    NEED TO DELETE OLD CODE?                     
         BNH   BCOD0020            NO  - TREAT NEW CODE ONLY                    
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(2),=X'9B01'     SET PASSIVE TYPE                             
         MVC   KEY+11(2),REPALPHA  INSERT REP CODE                              
         L     R7,ASPULAR          RETRIEVE CONTRACT NUMBER                     
         USING RCONREC,R7                                                       
         MVC   KEY+13(3),ORIGBYCD  INSERT ORIGINAL BUYLINE CODE                 
         MVC   KEY+16(4),RCONKCON  INSERT CONTRACT NUMBER                       
         DROP  R7                                                               
         MVC   KEY+20(1),RBUYKLIN  INSERT BUYLINE NUMBER                        
         GOTO1 VHIGH               READ FOR KEY                                 
         CLC   KEY(21),KEYSAVE     KEY FOUND?                                   
         BNE   BCOD0020            NO  - TREAT AS IF NON-EXISTANT               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VREAD               READ THE KEY FOR UPDATE                      
         OI    KEY+27,X'80'        SET KEY FOR DELETION                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
BCOD0020 EQU   *                                                                
*                                                                               
*   AT THIS POINT ADD OR REWRITE NEW CODES AS FOUND                             
*                                                                               
         LA    R6,RBUYELEM                                                      
BCOD0040 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    BCOD0240            YES - NO X'5F' ELEMENT                       
         CLI   0(R6),X'5F'         BUYLINE CODE ELEMENT?                        
         BE    BCOD0060            YES - ADJUST PASSIVE KEYS                    
         ZIC   RF,1(R6)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R6,RF                                                            
         B     BCOD0040                                                         
BCOD0060 EQU   *                                                                
*                                                                               
         XC    KEY,KEY             CLEAR KEY FOR REBUILD                        
         MVC   KEY(2),=X'9B01'                                                  
         MVC   KEY+11(2),RCONKREP                                               
*                                  INSERT REP CODE                              
         MVC   KEY+13(3),RBYSCDBC-RBYSCDEL(R6)                                  
*                                  INSERT BUYLINE CODE                          
         L     R7,ASPULAR          RETRIEVE CONTRACT NUMBER                     
         USING RCONREC,R7                                                       
         MVC   KEY+16(4),RCONKCON  INSERT CONTRACT NUMBER                       
         DROP  R7                                                               
         LA    RF,RBUYREC                                                       
         USING RBUYREC,RF                                                       
         MVC   KEY+20(1),RBUYKLIN  INSERT BUYLINE NUMBER                        
         MVC   KEY+21(3),=X'FFFFFF'                                             
*                                  INITIALIZE EARLIEST DATE                     
         XC    KEY+24(3),KEY+24    INITIALIZE LATEST DATE                       
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    R6,RBUYELEM                                                      
BCOD0080 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    BCOD0180            YES - NO (MORE) X'03' ELEMENT(S)             
         CLI   0(R6),X'03'         EFFECTIVE DATE ELEMENT?                      
         BE    BCOD0120            YES - ADJUST PASSIVE KEYS                    
BCOD0100 EQU   *                                                                
         ZIC   RF,1(R6)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R6,RF                                                            
         B     BCOD0080                                                         
BCOD0120 EQU   *                                                                
         USING RBUYDTEL,R6                                                      
         CLC   RBUYDTST,KEY+21     EFF START DATE EARLIER?                      
         BH    BCOD0140            NO                                           
         MVC   KEY+21(3),RBUYDTST  YES - USE IT                                 
BCOD0140 EQU   *                                                                
         CLC   RBUYDTED,KEY+21     EFF END   DATE LATER?                        
         BNH   BCOD0160            NO                                           
         MVC   KEY+24(3),RBUYDTED  YES - USE IT                                 
BCOD0160 EQU   *                                                                
         B     BCOD0100            GO BACK FOR NEXT X'03' ELEMENT               
BCOD0180 DS    0H                                                               
*                                                                               
*   NEED TO DELETE ANY PRIOR KEY WHICH MAY HAVE DIFFERENT FLIGHT                
*                                                                               
         MVC   BYCDKEY,KEY         SAVE NEWLY BUILT KEY                         
         XC    KEY+21(6),KEY+21    CLEAR OUT FLIGHT DATES                       
         GOTO1 VHIGH               READ FOR NON-DELETED KEYS ONLY               
         CLC   KEY(21),KEYSAVE     KEY THROUGH BUYLINE# FOUND?                  
         BNE   BCOD0200            NO                                           
         MVI   UPDATE,C'Y'         READ WHOLE KEY FOR UPDATE                    
         GOTO1 VREAD                                                            
         OI    KEY+27,X'80'                                                     
*                                  SET KEY FOR DELETION                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
BCOD0200 EQU   *                                                                
         MVC   KEY(27),BYCDKEY     RESTORE NEWLY BUILT KEY                      
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS ALSO               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    BCOD0220            YES                                          
         MVC   KEY,BYCDKEY         NO  - ADD AS NEW KEY                         
         MVC   KEY+28(4),TWABADDR                                               
*                                  INSERT CORRECT DA AND CONTROL BYTE           
         GOTO1 VADD                                                             
         B     BCOD0240                                                         
*                                                                               
BCOD0220 DS    0H                                                               
         MVI   UPDATE,C'Y'         READ KEY FOR UPDATE                          
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS ALSO               
         GOTO1 VREAD                                                            
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),TWABADDR                                               
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VWRITE              UPDATE KEY                                   
         B     BCOD0240                                                         
BCOD0240 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122RECNT30   12/09/02'                                      
         END                                                                    
