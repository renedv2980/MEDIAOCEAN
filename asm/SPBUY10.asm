*          DATA SET SPBUY10    AT LEVEL 023 AS OF 02/26/21                      
*PHASE T21110C                                                                  
         TITLE 'T21110 - SPOTPAK BUY - BUY DESC EDITS'                          
T21110   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21110                                                         
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21110+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         ZIC   RF,EDTVAL           GET BRANCH VALUE                             
         B     BUEDTTAB(RF)                                                     
         EJECT                                                                  
BUEDTTAB B     PER                 X'00'                                        
         B     NPW                 X'04'                                        
         B     TIME                X'08'                                        
         B     DPT                 X'0C'                                        
         B     SLN                 X'10'                                        
         DC    AL4(0)    PGM       X'14'  IN OVLY 11                            
         DC    AL4(0)    ADJ       X'18'  IN OVLY 11                            
         B     COST                X'1C'                                        
         B     DEM                 X'20'                                        
         B     PRTNR               X'24'                                        
         DC    AL4(0)    REF       X'28' IN OVLY 11                             
         B     ALLOC               X'2C'                                        
         B     DAY                 X'30'                                        
         B     ORB                 X'34'                                        
         B     CFMD                X'38'                                        
         B     BOOK                X'3C'                                        
         B     ELDTVAL             X'40'                                        
         B     COM                 X'44'                                        
         B     REP                 X'48'                                        
         B     TAX                 X'4C'                                        
         DC    AL4(0)    PCD       X'50'  IN OVLY 11                            
         DC    AL4(0)    SKED      X'54' (DOES NOT EXIST)                       
         B     COST      INTG      X'58'                                        
         DC    AL4(0)    STAT      X'5C' (DOES NOT EXIST)                       
         B     COST      CUTIN     X'60'                                        
         DC    AL4(0)              X'64'                                        
         DC    AL4(0)              X'68'                                        
         DC    AL4(0)              X'6C'                                        
         DC    AL4(0)              X'70'                                        
         DC    AL4(0)              X'74'                                        
         DC    AL4(0)              X'78'                                        
         DC    AL4(0)              X'7C'                                        
         DC    AL4(0)              X'80'                                        
         DC    AL4(0)              X'84'                                        
         DC    AL4(0)              X'88'                                        
         B     UP                  X'8C'                                        
         DC    AL4(0)              X'90'                                        
         B     CTYPE               X'94'                                        
         B     STYPE               X'98'                                        
         B     C58TAX              X'9C'                                        
         B     EXCHANGE            X'A0'                                        
         DC    AL4(0)              X'A4'                                        
         DC    AL4(0)              X'A8'                                        
         DC    AL4(0)              X'AC'                                        
         DC    AL4(0)              X'B0'                                        
         DC    AL4(0)              X'B4'                                        
         B     COST                X'B8'                                        
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1  REGS=(R2)                                                        
*                                                                               
BUYERR   GOTO1 ERROR                                                            
         EJECT                                                                  
* EDIT PERIOD AND DAYS. FORMAT IS MMMDD-MMMDD,-NNW,OR -E.                       
*                                                                               
PER      MVI   UPNDX,SBUYROTQ                                                   
         MVI   ERRCD,PERERR                                                     
         MVI   FSTOPS,C'-'                                                      
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         GOTO1 VDATVAL,DMCB,(1,(R4)),BUSTART                                    
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         BZ    BUYERR                                                           
         MVI   BUWKIND,C'O'        SET DEFAULT ALT WK IND                       
*                                                                               
         MVC   BUSTART(2),SVSTART  MOVE EST START YEAR                          
         CLC   SVSTART(2),SVEND    EST ALL IN ONE YEAR                          
         BE    PER1                YES                                          
         CLC   BUSTART+2(4),SVSTART+2   INPUT MMDD TO ES START MMDD             
         BNL   *+10                     IF INPUT HI OR EQ USE START YR          
         MVC   BUSTART(2),SVEND                                                 
*                                                                               
PER1     MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
*                                                                               
         CLI   FLEN+1,1                                                         
         BNE   PER2                                                             
         CLI   0(R4),C'E'                                                       
         BNE   BUYERR                                                           
         MVC   BUEND,SVEND         SET EST END DATE AS PER END                  
         MVI   BUPERIND,3          INDICATE -E                                  
         B     PER10                                                            
*                                                                               
* TEST FOR ANOTHER VALID DATE                                                   
*                                                                               
PER2     GOTO1 VDATVAL,DMCB,(1,(R4)),BUEND                                      
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         BZ    PER4                                                             
         MVI   BUPERIND,2          INDICATE END DATE                            
*                                                                               
         MVC   BUEND(2),BUSTART    MOVE PERIOD START YEAR                       
         CLC   BUEND+2(4),SVEND+2  UNLESS EXACT END DAY OF EST                  
         BNE   *+10                                                             
         MVC   BUEND(2),SVEND      THEN START WITH EST END YEAR                 
*                                                                               
         CLC   SVSTART(2),SVEND    EST ALL IN ONE YEAR                          
         BE    PER10               YES                                          
         CLC   BUEND+2(4),BUSTART+2   INPUT MMDD TO ES START MMDD               
         BNL   *+10                   IF INPUT HI OR EQ USE START YR            
         MVC   BUEND(2),SVEND                                                   
         B     PER10                                                            
*                                                                               
* NOT A VALID DATE - RE-EDIT FOR WEEKS                                          
*                                                                               
PER4     MVI   BUPERIND,1          INDICATE WEEKS                               
         MVI   FSTOPS,C'W'                                                      
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         GOTO1 FLDVAL                                                           
*                                                                               
         CLI   FSTOP,C'W'                                                       
         BNE   BUYERR                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    BUYERR                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         CVB   R0,DUB              SAVE NUMBER OF WEEKS                         
         STC   R0,BUWKS                                                         
* CHECK FOR ALT WEEK IND                                                        
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL                                                           
         LA    R1,7                                                             
         LTR   R5,R5                                                            
         BZ    PER6                                                             
         CLI   FLEN+1,1                                                         
         BNE   BUYERR                                                           
         MVC   BUWKIND,0(R4)       SAVE WEEK IND                                
         LA    R1,14                                                            
         CLI   0(R4),C'A'                                                       
         BE    PER6                                                             
         LA    R1,21                                                            
         CLI   0(R4),C'T'                                                       
         BE    PER6                                                             
         LA    R1,28                                                            
         CLI   0(R4),C'F'                                                       
         BE    PER6                                                             
         B     BUYERR                                                           
*                                                                               
PER6     ZIC   R0,BUWKS                                                         
         BCTR  R0,0                GET NUMBER OF INTERVALS                      
         CLI   BUWKIND,C'O'                                                     
         BE    *+10                                                             
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
         MR    R0,R0                                                            
         LR    R0,R1                                                            
         GOTO1 VADDAY,DMCB,BUSTART,BUEND,(R0)  AND SET END DATE                 
         EJECT                                                                  
* EDIT DAYS                                                                     
*                                                                               
PER10    DS    0H                                                               
         LA    R4,1(R4,R5)         POINT TO DAY ENTRY                           
         ST    R4,FADDR            AND UPDATE FLDVAL TABLE                      
         XC    FLEN,FLEN                                                        
         TM    BUSTAT,X'01'        TEST NETPAK                                  
         BO    PER10X                                                           
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,DAYERR                                                     
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         BNZ   PER10A                                                           
         LA    R4,1(R4)            UPDATE INPUT POINTER                         
         ST    R4,FADDR                                                         
         CLI   BUTRCODE,C'B'       MUST ENTER DAY FOR CHANGES                   
         BNE   BUYERR                                                           
         TM    BUSTAT,X'01'        TEST NETPAK                                  
         BO    PER10X                                                           
         B     BUYERR                                                           
PER10A   DS    0H                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A03'  GET ADDRESS OF DAYPAK                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),((R5),(R4)),BUDAYS,BUDAYNUM                            
*                                                                               
         CLI   BUDAYS,0                                                         
         BE    BUYERR                                                           
*                                                                               
         MVI   ERRCD,ROTATERR                                                   
         ZIC   R0,BUDAYNUM                                                      
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         STC   R0,BUDAY1IN                                                      
         STC   R1,BUDAYXIN                                                      
*                                                                               
         CLI   SVEOWSDY,0          TEST OUT-OF-WEEK DATA ALLOWED                
         BNE   PER10B              YES                                          
         CR    R0,R1               NO OUT-OF-WEEK ROTATORS                      
         BH    BUYERR                                                           
         B     PER10X                                                           
*                                                                               
PER10B   XC    WORK,WORK                                                        
         LHI   R0,7                                                             
         LA    R1,WORK                                                          
         LHI   RE,X'40'                                                         
         LHI   RF,1                DAY NUMBER                                   
*                                                                               
PER10C   EX    RE,*+8                                                           
         B     *+8                                                              
         TM    BUDAYS,0                                                         
         BZ    *+8                                                              
         STC   RF,0(R1)            SET FLAG FOR DAY ACTIVE                      
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         SRL   RE,1                                                             
         BCT   R0,PER10C                                                        
         MVC   WORK+7(7),WORK      COPY THE FLAGS                               
*                                                                               
         SR    RE,RE                                                            
         IC    RE,SVEOWSDY                                                      
         LA    RE,WORK-1(RE)       POINT TO FIRST POSSIBLE DAY                  
         LA    RF,6(RE)            POINT TO LAST POSSIBLE DAY                   
*                                                                               
PER10D   CLI   0(RE),0             TEST THIS DAY ACTIVE                         
         BNE   PER10E                                                           
         LA    RE,1(RE)                                                         
         B     PER10D                                                           
*                                                                               
PER10E   SR    R0,R0                                                            
         IC    R0,0(RE)            GET START DAY NUMBER                         
         SLL   R0,4                                                             
*                                                                               
         CLI   0(RF),0             TEST LAST DAY ACTIVE                         
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         OR    R0,RE               AT LAST WE HAVE BUDAYNUM                     
         STC   R0,BUDAYNUM                                                      
*                                                                               
         STC   R0,BUDAYXIN                                                      
         NI    BUDAYXIN,X'0F'                                                   
         SRL   R0,4                                                             
         STC   R0,BUDAY1IN                                                      
*                                                                               
PER10X   MVI   ERRCD,STENDERR                                                   
         CLC   BUSTART,BUEND       BUY START BEFORE BUY END                     
         BH    BUYERR                                                           
*                                                                               
         MVI   ERRCD,ESPERERR                                                   
         CLC   BUSTART,SVSTART     BUY START BEFORE EST START                   
         BL    BUYERR                                                           
         CLC   BUEND,SVEND         BUY END AFTER EST END                        
         BH    BUYERR                                                           
         CLI   BDTIME,0            TEST P/B                                     
         BE    PER12               NO                                           
* FIND PBELEM                                                                   
         LA    R6,BDELEM                                                        
PER11    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),4                                                          
         BNE   PER11                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY                                                   
         MVC   KEY+4(3),6(R6)                                                   
         MVC   KEY+7(1),3(R6)      EST                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,CHKPTNR                                                       
         BNE   BUYERR                                                           
* GET DAYS FOR PERIOD START AND END DATES                                       
PER12    DS    0H                                                               
         GOTO1 VGETDAY,DMCB,BUSTART,DUB                                         
         MVC   BUDAY1,0(R1)                                                     
         MVC   WORK(3),DUB                                                      
*                                                                               
         GOTO1 (RF),(R1),BUEND                                                  
         MVC   BUDAYX,0(R1)                                                     
         TM    BUSTAT,X'01'        TEST NETPAK                                  
         BO    PER50                                                            
* CHECK START DATE DAY VS INPUT DAY                                             
         MVI   ERRCD,STDTERR                                                    
         CLC   BUDAY1IN,BUDAY1                                                  
         BE    PER30                                                            
*                                                                               
         CLC   SVSTART(2),SVEND    ESTIMATE ALL IN ONE YEAR                     
         BE    PER25               DON'T DO ANYTHING                            
         CLC   BUSTART(2),SVSTART  START DATE IN START YEAR OF EST?             
         BNE   PER25               NO - DONE                                    
         MVC   BUSTART(2),SVEND    YES - THEN TRY END YEAR                      
         MVC   BUEND(2),SVEND                                                   
*                                                                               
*IF CHANGING THE YEAR TO ESTIMATE END YEAR MAKES THE BUY START DATE             
*AFTER THE ESTIMATE END DATE THEN STICK WITH REGULAR WRONG DAY OR               
*OOWR ERROR MESSAGE - IF END DATE OUT OF ESTIMATE THEN ESTIMATE ERROR           
*                                                                               
         CLC   BUSTART,SVEND       BUY START BEFORE EST START                   
         BNH   PER14                                                            
         MVC   BUSTART(2),SVSTART  SET BACK TO START YEAR FOR ERROR             
         B     PER25                                                            
*                                                                               
PER14    MVI   ERRCD,ESPERERR                                                   
         CLC   BUEND,BUSTART       IF END IS NOW BEFORE START                   
         BL    BUYERR              REALLY THE END IS OUT OF ESTIMATE            
         B     PER10X              BACK UP TO VALIDATE A BIT MORE               
*                                                                               
PER25    XC    BUYMSG,BUYMSG                                                    
         MVC   BUYMSG(39),=C'EN/0726 - XXXXXXXX IS A XXX OR BAD OOWR'           
         MVC   BUYMSG+24(3),WORK                                                
         GOTO1 VDATCON,DMCB,BUSTART,(5,BUYMSG+10)                               
         MVI   ERRAREA,X'FF'       INDICATE MESSAGE PRESENT                     
         B     BUYERR                                                           
*                                                                               
PER30    BAS   RE,PERADJ                                                        
         B     PER50                                                            
         EJECT                                                                  
*                                                                               
PERADJ   NTR1                                                                   
*                                                                               
         CLI   BUPERIND,2          TEST INPUT METHOD                            
         BL    PERADJ20            WEEK                                         
         BE    PERADJ30            END DATE                                     
         B     PERADJ40            -E                                           
*                                                                               
PERADJ20 DS    0H                  NUMBER OF WEEKS                              
         CLC   BUDAY1IN,BUDAYXIN  TEST ROTATOR                                  
         BE    EXIT                NO - BUEND IS DAY IN LAST WEEK               
* SET END DATE TO LAST DAY OF ROTATOR                                           
         ZIC   R0,BUDAYXIN                                                      
         ZIC   RE,BUDAYX                                                        
PERADJ22 SR    R0,RE                                                            
         BP    *+8                                                              
         AH    R0,=H'7'            MUST BE OUT-OF-WEEK ROT                      
PERADJ24 GOTO1 VADDAY,DMCB,BUEND,DUB,(R0)                                       
         MVC   BUEND,DUB                                                        
         CLC   BUEND,SVEND         TEST PAST ESTIMATE END DATE                  
         BNH   *+10                                                             
         MVC   BUEND,SVEND                                                      
         B     EXIT                                                             
         SPACE 2                                                                
* END DATE MUST BE FIRST OR  LAST DAY OF ROTATOR                                
*                                                                               
PERADJ30 DS    0H                  END DATE                                     
         MVI   ERRCD,ENDDTERR                                                   
         CLC   BUDAYXIN,BUDAYX     TEST END DAY = END DATE DAY                  
         BE    EXIT                                                             
*                                                                               
         CLC   BUDAYX,BUDAY1       TEST END DATE DAY = ST DATE DAY              
         BNE   BUYERR                                                           
* ADJUST END DATE TO LAST DAY OF ROTATOR (BUDAYXIN)                             
         ZIC   R0,BUDAYXIN                                                      
         ZIC   RE,BUDAYX                                                        
         B     PERADJ22                                                         
         SPACE 2                                                                
PERADJ40 DS    0H                  -E INPUT                                     
         ZIC   RE,BUDAYXIN         INPUT END DAY                                
         ZIC   R0,BUDAYX           END DATE DAY                                 
         SR    R0,RE                                                            
         BP    *+8                                                              
         AHI   R0,7                                                             
         LNR   R0,R0                                                            
         B     PERADJ24            GO MOVE END DATE BACK                        
         EJECT                                                                  
PER50    CLI   BUWKS,0                                                          
         BNE   PER54                                                            
* CALCULATE NUMBER OF WEEKS                                                     
         MVC   WORK(6),BUSTART                                                  
         LA    R1,DMCB                                                          
         LA    RE,WORK                                                          
         ST    RE,0(R1)                                                         
         ST    RE,4(R1)                                                         
         LA    RE,7                                                             
         CLI   BUWKIND,C'O'                                                     
         BE    PER51                                                            
         LA    RE,14                                                            
         CLI   BUWKIND,C'A'                                                     
         BE    PER51                                                            
         LA    RE,21                                                            
         CLI   BUWKIND,C'T'                                                     
         BE    PER51                                                            
         LA    RE,28                                                            
PER51    ST    RE,8(R1)                                                         
         L     RF,VADDAY                                                        
         SR    R0,R0               RESET COUNTER                                
         BCTR  R0,0                SET TO 1                                     
PER52    GOTO1 (RF),(R1)                                                        
         CLC   WORK(6),BUEND                                                    
         BH    *+8                                                              
         BCT   R0,PER52            ADD 1                                        
         LPR   R0,R0                                                            
         STC   R0,BUWKS                                                         
*                                                                               
PER54    DS    0H                                                               
         TM    SVCOPT1,X'40'       TEST INFOMERCIAL CLIENT                      
         BZ    PER55               NO                                           
         MVI   ERRCD,PERERR                                                     
         CLI   BUWKS,1                                                          
         BNE   BUYERR                                                           
         CLC   BUDAY1IN,BUDAYXIN   TEST ROTATOR                                 
         BNE   BUYERR              YES - NOT ALLOWED                            
*                                                                               
PER55    DS    0H                                                               
         GOTO1 VDATCON,DMCB,BUSTART,(3,BUSTARTB)                                
         GOTO1 (RF),(R1),BUEND,(3,BUENDB)                                       
         GOTO1 (RF),(R1),BUSTART,(2,BUSTARTP)                                   
         GOTO1 (RF),(R1),BUEND,(2,BUENDP)                                       
*                                                                               
         CLI   SVCXTRA+4,C'E'      TEST FLIGHTS BY EST                          
         BNE   EXIT                                                             
         LA    RE,SVFLTDTS                                                      
         OC    0(4,RE),0(RE)       IF NO FLIGHT DATES SAVED                     
         BZ    EXIT                IT'S A TRANSITIONAL PROBLEM                  
PER56    CLC   BUENDP,2(RE)        BUY END TO FLT END                           
         BH    *+14                HIGH - SKIP                                  
         CLC   BUSTARTP,0(RE)      BUY START TO FLT START                       
         BNL   EXIT                HIGH OR EQUAL - DATES OK                     
         LA    RE,4(RE)            NEXT ENTRY                                   
         CLI   0(RE),0             TEST E-O-L                                   
         BNE   PER56                                                            
         MVI   ERRCD,NOTINFLT                                                   
         B     BUYERR                                                           
         B     EXIT                                                             
         EJECT                                                                  
* EDIT NPW                                                                      
*                                                                               
NPW      BRAS  RE,ANPW                                                          
         B     EXIT                                                             
                                                                                
COST     BRAS  RE,ACOST                                                         
         B     EXIT                                                             
                                                                                
* EDIT TIME                                                                     
*                                                                               
TIME     MVI   UPNDX,SBUYSTMQ                                                   
         MVI   ERRCD,TIMERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A0E'  GET ADDRESS OF TIMVAL                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),((R5),(R4)),BUTIME                                     
         CLI   0(R1),X'FF'                                                      
         BE    BUYERR                                                           
*                                                                               
         LA    R0,500              SET RADIO START TIME                         
         CLI   BUYMD,C'R'                                                       
         BE    TIM2                                                             
         CLI   BUYMD,C'X'          X IS NETWORK RADIO, STUPID                   
         BE    TIM2                                                             
*-------------------------------------------------------------------*           
         CLI   BUYMD,C'T'          IGNORE THIS TEST FOR TELEVISION              
         BE    TIM10               MHER 11MAR97                                 
         CLI   BUYMD,C'N'          FOR GCOO                                     
         BNE   TIM1                                                             
**       CLC   =C'T1',AGYALPHA     ALLOW SAME AS FOR TV                         
**       BE    TIM10                                                            
*-------------------------------------------------------------------*           
TIM1     LA    R0,600              OTHER MEDIA START AT 6A                      
*                                                                               
TIM2     SR    R4,R4                                                            
         ICM   R4,3,BUTIME         START TIME                                   
         CR    R4,R0               TEST PRIOR TO START OF DAY                   
         BNL   *+8                                                              
         AH    R4,=H'2400'                                                      
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,BUTIME+2       END TIME                                     
         BZ    TIM10                                                            
         CR    R5,R0               TEST PRIOR TO START OF DAY                   
         BNL   *+8                                                              
         AH    R5,=H'2400'                                                      
*                                                                               
         CR    R4,R5               START OF DAY TO START TIME                   
         BH    BUYERR              THIS SHOULD NOT BE !                         
*                                                                               
TIM10    CLC   =C'CC',BUTIME+2                                                  
         BE    BUYERR                                                           
         CLI   0(R1),X'FF'                                                      
         BE    BUYERR                                                           
         B     EXIT                                                             
         SPACE 2                                                                
* EDIT DAY (FOR LI=1+N)                                                         
*                                                                               
DAY      MVI   ERRCD,DAYERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         L     RF,VCALLOV                                                       
         GOTO1 (RF),DMCB,0,X'D9000A03'  GET ADDRESS OF DAYPAK                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),((R5),(R4)),BUDAYS,BUDAYNUM                            
         CLI   BUDAYS,0                                                         
         BE    BUYERR                                                           
         B     EXIT                                                             
         EJECT                                                                  
DPT      MVI   UPNDX,SBUYDPTQ                                                   
         MVI   ERRCD,DPTERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BNZ   DPT1                                                             
*                                                                               
         CLI   BUTRCODE,C'B'                                                    
         BNE   BUYERR                                                           
         TM    BUSTAT,X'03'        TEST NETPAK BUY (NOT PKG)                    
         BM    DEM10               YES-ALLOW NO DPT                             
         B     BUYERR                                                           
*                                                                               
DPT1     CLI   FLEN+1,1                                                         
         BNE   BUYERR                                                           
         CLI   0(R4),C'Z'                                                       
         BE    BUYERR                                                           
*                                                                               
         LA    R0,L'SVMENU-1       R0=MAX N'DAYPARTS                            
         LA    R1,SVMENU                                                        
*                                                                               
DPT2     CLC   0(1,R4),0(R1)                                                    
         BE    DPTX                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,DPT2                                                          
         B     BUYERR                                                           
*                                                                               
DPTX     MVC   BUDPT,0(R4)                                                      
         B     EXIT                                                             
         EJECT                                                                  
SLN      MVI   UPNDX,SBUYSLNQ                                                   
         MVI   ERRCD,SLNERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BNZ   SLN2                                                             
         MVI   BUSLN,30                                                         
         B     DEM10                                                            
*                                                                               
SLN2     CLI   FLEN+1,3                                                         
         BH    BUYERR                                                           
         TM    FVAL,X'08'                                                       
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         STC   R0,BUSLN                                                         
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
         CH    R0,=H'255'                                                       
         BH    BUYERR                                                           
*                                                                               
         CLI   SVESLN,0            ONE SLN LIMIT                                
         BE    SLN10                                                            
         CLC   SVESLN,BUSLN                                                     
         BE    SLN10                                                            
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(ONLY1SLN)                                              
         B     BUYERR                                                           
*                                                                               
SLN10    MVC   BYTE,BUSLN                                                       
         GOTO1 VCHKSLN             VALIDATE SLN                                 
         B     EXIT                                                             
         EJECT                                                                  
* EDIT DEMOGRAPHIC VALUE                                                        
*                                                                               
DEM      MVI   ERRCD,DEMERR                                                     
         MVI   FSTOPS+1,C'/'                                                    
         GOTO1 FLDVAL                                                           
         XC    BUDEM,BUDEM                                                      
         XC    BUNEWDEM,BUNEWDEM                                                
         LTR   R5,R5               TEST NO INPUT                                
         BZ    DEM10               YES - IGNORE                                 
         CLI   BUTRCODE,C'C'                                                    
         BNE   DEM2                                                             
         CLC   =C'NOV',0(R4)       SPECIAL CODE FOR NO OVRD                     
         BNE   DEM2                                                             
         MVC   BUDEM(2),=X'FFFF'                                                
         B     DEMX                                                             
*                                                                               
DEM2     LA    R7,0(R5,R4)         TEST LAST CHAR IS A *                        
         BCTR  R7,0                                                             
         CLI   0(R7),C'*'                                                       
         BNE   *+6                                                              
         BCTR  R5,0                                                             
*                                                                               
         CLI   0(R4),C'L'          "LOOKED UP" VALUE PASSED?                    
         BNE   DEM3                                                             
         CLI   0(R7),C'*'          YES, AND AN OVERRIDE?                        
         BE    BUYERR                   YES, NO WAY                             
         LR    R7,R4               SAVE OFF WHERE WE WERE                       
         LA    R4,1(R4)            AND POINT TO THE VALUE                       
         BCTR  R5,0                 WITH ONE LESS CHARACTER                     
*                                                                               
DEM3     GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
         CLI   0(R1),X'FF'                                                      
         BE    BUYERR                                                           
*                                                                               
         CLI   0(R7),C'*'          OVERRIDE FROM BEFORE?                        
         BE    DEM3A                                                            
         CLI   0(R7),C'L'           OR  "LOOKED UP" VALUE FROM SENDER?          
         BNE   DEM3C                                                            
         LR    R4,R7                 RESTORE A(INPUT) TO R4                     
DEM3A    LA    R5,1(R5)            YES, RESTORE LENGTH TO R5                    
*                                                                               
DEM3C    L     R0,4(R1)                                                         
         SRDA  R0,32                                                            
************************                                                        
* NON-TRAD DEMO TEST FOR LT 20 IS NOT THE BEST SOLUTION, BUT                    
* TAKING INTO CONSIDERATION DEADLINES, THIS WILL HAVE TO DO FOR NOW             
* NOTE: THE BEST CORRECT SOLUTION IS TO CHANGE ALL PROGRAM THAT CALL            
*  DEMEDT TO CORRECTLY SET SPDEMTYP                                             
************************                                                        
         CLI   SPDEMTYP,20         TEST NONT DEMO?                              
         JH    DEM3M               NO                                           
*                                                                               
         LLC   RE,SPDEMTYP          YES, GET NONT DEMO SEQNUM                   
         BCTR  RE,0                                                             
         SLL   RE,3                X 8                                          
         AHI   RE,SVNTDMS-BUYSAVE                                               
         AR    RE,RA               RE=A(NON-TRAD SHORT NAME)                    
         MVC   SPDEMTYP,0(RE)      SAVE NON-TRAD DEMO TYPE                      
*                                                                               
DEM3M    LAY   RF,SV00APRF+6       TEST 2 DECIMAL IMPRESSIONS                   
         CLI   SPDEMTYP,C'X'       COMSCORE IMPRESSION?                         
         JE    DEM3DEC2             YES                                         
         CLI   SPDEMTYP,C'I'       IMPRESSIONS?                                 
         JE    DEM3DEC2             YES                                         
*                                                                               
DEM3R    LAY   RF,SV00PROF+9       TEST 2 DECIMAL RATINGS                       
         CLI   SPDEMTYP,C'R'       RATING DEMO TYPE?                            
         BE    DEM3R10              YES                                         
         CLI   SPDEMTYP,C'E'       OR EXTENDED DEMO TYPE?                       
         BNE   DEM4                 NEITHER RATS/IMPS, RND&ADJ TO 1DEC          
*                                                                               
DEM3R10  CLI   BUYMD,C'X'          US MEDIA X ALWAYS USE 2-DEC RATINGS          
         BNE   DEM3DEC2             NO                                          
         CLI   SVAPROF+7,C'C'      CANADIAN AGY?                                
         BNE   DEM3DEC4             NO                                          
*                                                                               
DEM3DEC2 CLI   0(RF),C'Y'          TEST 2 DECIMAL IMPS/RATS?                    
         BNE   DEM4                 NO, ROUND & ADJUST                          
         CLI   BUYMD,C'T'           YES, PROFILE ON APPLIC TO TV                
         JE    DEM3DEC4                                                         
         CLI   BUYMD,C'R'          CANADIAN RADIO?                              
         JNE   DEM4                                                             
         CLI   SVAPROF+7,C'C'                                                   
         JNE   DEM4                NO, THEN 1 DECIMAL                           
*                                                                               
DEM3DEC4 ST    R1,BUNEWDEM                                                      
         OI    BUNEWDEM,X'40'      SET 2 DECIMAL FLAG                           
         B     DEM6                                                             
*                                                                               
DEM4     AHI   R1,5                ROUND                                        
         D     R0,=F'10'           ADJUST TO 1 DECIMAL                          
         ST    R1,BUNEWDEM                                                      
*                                                                               
DEM6     L     R0,=F'10000000'                                                  
         CLR   R1,R0               TEST EXCEEDS MAX                             
         BNL   BUYERR                                                           
*                                                                               
DEMX     B     EXIT                                                             
*                                                                               
DEM10    LA    R4,1(R4)            RESET INPUT POINTER FOR NEXT TIME            
         ST    R4,FADDR                                                         
         B     EXIT                                                             
         EJECT                                                                  
* EDIT PARTNER. FORMAT IS PA='PRD'-'EST'-('SLN')                                
*                                                                               
PRTNR    MVI   UPNDX,SBUYP1SQ                                                   
         XC    BUPSSV,BUPSSV                                                    
         MVI   ERRCD,INVPTNR                                                    
         MVI   FSTOPS+1,C'-'                                                    
         GOTO1 FLDVAL                                                           
         MVC   BUPSVPRD,0(R4)                                                   
         CLI   FLEN+1,3                                                         
         BE    *+16                                                             
         MVI   BUPSVPRD+2,C' '                                                  
         CLI   FLEN+1,2                                                         
         BNE   BUYERR                                                           
         CLC   BUPSVPRD,QPRD       TEST DIFFERENT PRD                           
         BE    BUYERR                                                           
         MVC   BUPSVEST,SVEST      SET DEFAULT PARTNER EST                      
         CLI   FSTOP,C'-'                                                       
         BNE   PRTNR2                                                           
* PARTNER ESTIMATE                                                              
         MVC   FSTOPS(2),=C',-'                                                 
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         CLI   FLEN+1,3                                                         
         BH    BUYERR                                                           
         TM    FVAL,X'08'          NUMERIC                                      
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
         CH    R0,=H'255'                                                       
         BH    BUYERR                                                           
         STC   R0,BUPSVEST                                                      
* VALIDATE PRD/EST ON FILE                                                      
PRTNR2   L     R1,ASVCLIST                                                      
*                                                                               
PRTNR4   CLC   BUPSVPRD,0(R1)                                                   
         BE    PRTNR6                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   PRTNR4                                                           
         B     BUYERR                                                           
PRTNR6   MVC   BUPSVCD,3(R1)       BRAND CODE                                   
         EJECT                                                                  
* READ FOR ESTHDR                                                               
*                                                                               
         XC    KEY,KEY                                                          
         L     RE,ADRSVKEY                                                      
         MVC   KEY+1(3),0(RE)      A-M/CLT                                      
         MVC   KEY+4(3),BUPSVPRD                                                
         MVC   KEY+7(1),BUPSVEST                                                
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOPRDEST                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
         BAS   RE,CHKPTNR                                                       
         BNE   BUYERR                                                           
*                                                                               
         ZIC   R0,BUSLN            GET TOTAL LEN                                
         SRL   R0,1                DIVIDE BY 2                                  
         CLI   FSTOP,C'-'          TEST PRTNR LEN ENTERED                       
         BE    PRTNR8                                                           
         MVI   ERRCD,ODDSECS                                                    
         TM    BUSLN,X'01'                                                      
         BO    BUYERR                                                           
         B     PRTNR10                                                          
* PARTNER LENGTH                                                                
PRTNR8   DS    0H                                                               
         MVI   FSTOPS+1,0                                                       
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         CLI   FLEN+1,3                                                         
         BH    BUYERR                                                           
         TM    FVAL,X'08'          NUMERIC                                      
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         CH    R0,=H'255'                                                       
         BH    BUYERR                                                           
         EJECT                                                                  
PRTNR10  STC   R0,BUPSVTIM         SET AS TIME SHARE                            
         STC   R0,BUPSVCOS          AND COST SHARE                              
*                                                                               
         ZIC   RE,BUSLN            TOTAL LEN                                    
         SR    RE,R0                LESS PRTNR GIVES ACTIVE SECS                
         MVI   ERRCD,PTNRMAX                                                    
         BNP   BUYERR                                                           
         STC   RE,BUACTTIM                                                      
         STC   RE,BUACTCOS                                                      
* TEST VALID SLNS                                                               
         MVI   ERRCD,PTNRLEN                                                    
         MVC   BYTE,BUPSVTIM                                                    
         GOTO1 VCHKSLN                                                          
         MVC   BYTE,BUACTTIM                                                    
         GOTO1 VCHKSLN                                                          
         B     EXIT                                                             
         EJECT                                                                  
* EDIT ALLOCATION PRDS. FORMAT IS AA-BB/45-15. ( ) DENOTES MKGD PNDING          
*                                                                               
ALLOC    MVI   UPNDX,SBUYMASQ                                                   
         XC    BUELPRD(4),BUELPRD                                               
         MVI   BUXTFLAG,0                                                       
         MVI   ERRCD,PRDERR                                                     
         MVI   FSTOPS+1,C'-'       FOR P/B                                      
ALLOC2   DS    0H                                                               
         GOTO1 FLDVAL                                                           
         CLI   0(R4),C'*'          TEST DO NOT REALLOCATE                       
         BNE   ALLOC4                                                           
         TM    BUELPRSW,X'05'      TEST FOUND PREVIOUSLY OR HIAT                
         BNZ   BUYERR                                                           
         OI    BUELPRSW,X'01'                                                   
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         B     ALLOC2                                                           
*                                                                               
ALLOC4   CLI   0(R4),C'$'          TEST COST OVERRIDE                           
         BE    ALLOC30             YES - EDIT COST                              
         TM    BUELPRSW,X'01'      TEST DO NOT REALLOCATE                       
         BO    ALLOC10             YES - CAN'T BE HIAT/UNAL                     
*                                                                               
         CLC   SVCCOST2,=F'0'      TEST COST2 IS A FACTOR                       
         BNE   *+14                                                             
         OC    SVC2CONV,SVC2CONV                                                
         BZ    ALLOC6              NO                                           
* COST2 FACTOR HAS CAN INDICATE OVERRIDE FOR ONE COST ONLY                      
         CLI   0(R4),C'<'          TEST COST 1 ONLY                             
         BNE   *+12                                                             
         OI    BUXTFLAG,SXTFLAG_C1ONLY                                          
         B     ALLOC30                                                          
         CLI   0(R4),C'>'          TEST COST 2 ONLY                             
         BNE   *+12                                                             
         OI    BUXTFLAG,SXTFLAG_C2ONLY                                          
         B     ALLOC30                                                          
*                                                                               
*                                                                               
ALLOC6   CLI   FLEN+1,4                                                         
         BNE   ALLOC8                                                           
         CLC   =C'HIAT',0(R4)                                                   
         BNE   ALLOC8                                                           
         OI    BUELPRSW,X'04'                                                   
         B     ALLOC20                                                          
ALLOC8   CLI   FLEN+1,4                                                         
         BL    ALLOC10                                                          
         CLC   =C'UNAL',0(R4)                                                   
         BE    ALLOC20                                                          
*                                                                               
ALLOC10  BAS   RE,ALLOCPRD                                                      
         MVC   BUELSEC,BDSEC       SET DEFAULT SPTLEN                           
*                                                                               
         CLI   FSTOP,C','                                                       
         BE    ALLOC15                                                          
         CLI   FSTOP,C'-'          TEST FOR PRD-PRD SEPARATOR                   
         BNE   ALLOC15                                                          
*                                                                               
         TM    SVCOPT3,COP3SPOD    TEST SPODS ALLOWED                           
         BZ    ALLOC13                                                          
* VALIDATE SPOD SLN                                                             
         GOTO1 FLDVAL              GET SLN                                      
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(SPODBAD)                                               
         BAS   RE,ALLOCSEC                                                      
         MVC   BUELSEC,BYTE        SET RESULT IN BUY OUTPUT AREA                
         CLC   BUELSEC,BDSEC       MAKE SURE NOT > SLN                          
         BH    BUYERR                                                           
         B     EXIT                                                             
*                                                                               
* READ FOR SECOND PRD                                                           
*                                                                               
ALLOC13  MVI   UPNDX,SBUYMA2Q                                                   
         MVI   FSTOPS+2,C'/'       UNEQ SPLIT DELIMITER                         
         GOTO1 FLDVAL                                                           
*                                                                               
         BAS   RE,ALLOCPRD                                                      
         MVI   ERRCD,PRDERR                                                     
         CLC   BUELPRD(1),BUELPRD+1  TEST SAME PRD                              
         BE    BUYERR                                                           
*                                                                               
         CLI   FSTOP,C'/'          TEST UNEQ SPLIT                              
         BE    ALLOC14                                                          
* EQUAL SPLIT                                                                   
         CLI   BUTRCODE,C'B'                                                    
         BNE   *+10                                                             
         MVC   BDSEC,BUSLN         FIX PG 45 SEC P/B BUG                        
         MVI   ERRCD,ODDSECS                                                    
         TM    BDSEC,X'01'         TEST ODD SECONDS                             
         BO    BUYERR                                                           
*                                                                               
         ZIC   R0,BDSEC                                                         
         SRL   R0,1                                                             
         STC   R0,BYTE                                                          
         GOTO1 VCHKSLN             MAKE SURE SLN IS VALID                       
         STC   R0,BUELSEC                                                       
         STC   R0,BUELSEC2                                                      
*                                                                               
         B     ALLOC15                                                          
*                                                                               
* UNEQUAL SPLIT                                                                 
*                                                                               
ALLOC14  MVI   UPNDX,SBUYP1SQ                                                   
         MVI   FSTOPS+2,C'-'       SECONDS SEPARATOR                            
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,C'-'                                                       
         BNE   ALLOC15                                                          
         BAS   RE,ALLOCSEC                                                      
         MVC   BUELSEC,BYTE                                                     
*                                                                               
         MVI   FSTOPS+2,0                                                       
         GOTO1 FLDVAL                                                           
         BAS   RE,ALLOCSEC                                                      
         MVC   BUELSEC2,BYTE                                                    
*                                                                               
         MVI   ERRCD,ALLOCTOT                                                   
         ZIC   RE,BUELSEC                                                       
         ZIC   R0,BUELSEC2                                                      
         AR    RE,R0               TOTAL OF SPLITS                              
         ICM   R0,1,BDSEC          SPTLEN                                       
         BNZ   *+8                                                              
         IC    R0,BUSLN                                                         
         CR    RE,R0                                                            
         BNE   BUYERR                                                           
*                                                                               
ALLOC15  XC    BUDEM,BUDEM         CLEAR COST SHARE                             
         TM    BUELPRSW,X'08'      TEST BRAND A PAYS ALL                        
         BZ    ALLOC16                                                          
         CLI   BUELPRD+1,0         TEST PIGGYBACK                               
         BNE   ALLOC16                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(BADFRRDR)                                            
         B     BUYERR                                                           
*                                                                               
ALLOC16  TM    BUSTAT,X'01'        TEST NETPAK                                  
         BZ    EXIT                                                             
         CLI   BUELPR2,0           TEST P/B                                     
         BE    EXIT                NO                                           
         CLI   BUELSEC2,0          TEST SPLIT ENTERED AS SECONDS                
         BNE   EXIT                YES - DONE                                   
*                                                                               
         IC    R0,BDSEC                                                         
         STC   R0,BUELSEC          SET LEN1=LEN2=SLN                            
         STC   R0,BUELSEC2                                                      
*                                                                               
         MVI   ERRCD,BADPCTG                                                    
         GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
         ICM   R0,15,4(R1)                                                      
         BZ    BUYERR                                                           
         CH    R0,=H'9999'                                                      
         BH    BUYERR                                                           
         STH   R0,BUDEM                                                         
         B     EXIT                                                             
*                                                                               
* FOR HIATUS OR UNALL, MAKE SURE NO MORE INPUT                                  
*                                                                               
ALLOC20  CLI   FSTOP,C'-'                                                       
         BE    BUYERR                                                           
         CLI   FSTOP,C'$'                                                       
         BE    BUYERR                                                           
         B     EXIT                                                             
         EJECT                                                                  
* EDIT COST OVERRIDE AMOUNT                                                     
*                                                                               
ALLOC30  MVI   FSTOPS+1,0                                                       
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,BADCOST                                                    
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
* LOOK FOR CHARACTER THAT ENDS AMOUNT                                           
         LA    R0,4                                                             
         CLC   =C'FREE',0(R4)                                                   
         BE    ALLOC36A                                                         
*                                                                               
         SR    R0,R0                                                            
ALLOC32  CLI   0(R4),C'.'                                                       
         BE    ALLOC34                                                          
         CLI   0(R4),C'0'                                                       
         BL    ALLOC36                                                          
         CLI   0(R4),C'9'                                                       
         BH    ALLOC36                                                          
ALLOC34  LA    R4,1(R4)                                                         
         BCT   R0,ALLOC32                                                       
ALLOC36  LPR   R0,R0                                                            
*                                                                               
ALLOC36A L     R4,FADDR                                                         
*                                                                               
         GOTO1 VCASHVAL,DMCB,(2,(R4)),(R0)                                      
         CLI   0(R1),X'FF'                                                      
         BE    BUYERR                                                           
         L     RE,4(R1)                                                         
         LTR   RE,RE                                                            
         BM    BUYERR                                                           
         ST    RE,FULL                                                          
         BAS   RE,TSTCOST2                                                      
         BNE   BUYERR                                                           
*                                                                               
         CLI   FULL,0              AMOUNT MUST FIT IN 3 BYTES                   
         BNZ   BUYERR                                                           
         MVC   BUELCOS,FULL+1                                                   
         OI    BUELPRSW,X'20'      SET COST OVRD IND                            
* SET FLDVAL FOR NEXT EDIT                                                      
         AR    R4,R0                                                            
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         CLI   0(R4),C'A'          IF NEXT CHAR ALPHA, MUST BE PRD              
         BNL   ALLOC                                                            
         LA    R4,1(R4)            BUMP PAST STOP CHAR                          
         ST    R4,FADDR                                                         
         B     EXIT                                                             
         EJECT                                                                  
TSTCOST2 NTR1                                                                   
         L     RE,FULL             SEE IF AMOUNT = 0                            
         LTR   RE,RE                                                            
         BZ    EQXIT                                                            
         SR    R1,R1                                                            
         ICM   R1,7,BDCOST         OR IF AMOUNT = BDCOST                        
         CR    R1,RE                                                            
         BE    EQXIT                                                            
         LA    R6,BDELEM           SEE IF THERE IS A 2ND COST                   
*                                                                               
TST10    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EQXIT                                                            
         CLI   0(R6),X'71'         IF THERE IS - ERROR                          
         BNE   TST10                                                            
         B     NEQXIT                                                           
         EJECT                                                                  
** VALIDATE SLN                                                                 
*                                                                               
ALLOCSEC NTR1                                                                   
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         CLI   FLEN+1,3                                                         
         BH    BUYERR                                                           
         TM    FVAL,X'08'                                                       
         BZ    BUYERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4) *EXECUTED*                                           
         CVB   R0,DUB                                                           
         STC   R0,BYTE                                                          
*                                                                               
         GOTO1 VCHKSLN                                                          
         J     EXIT                                                             
         EJECT                                                                  
* EDIT PRD CODE AND VALIDATE EST ON FILE                                        
*                                                                               
ALLOCPRD NTR1                                                                   
*                                                                               
         MVI   ERRCD,PRDERR                                                     
         CLI   T211FFD+1,C'*'      DDS TERMINAL?                                
         BE    *+12                                                             
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     TEST DARE MAKEGOOD                  
         BZ    ALLPRD10            NO                                           
         CLI   0(R4),C'('          TEST MAKEGOOD PENDING                        
         BNE   ALLPRD10            NO                                           
         OI    BUELPRSW,X'10'      SET MAKEGOOD PENDING                         
*                                                                               
         LA    RE,0(R4,R5)         POINT TO END OF FIELD+1                      
         BCTR  RE,0                BACK UP TO LAST CHAR                         
         CLI   0(RE),C')'                                                       
         BNE   BUYERR                                                           
*                                                                               
         LA    R4,1(R4)            POINT TO PRD                                 
         BCTR  R5,0                                                             
         BCTR  R5,0                ADJUST LEN FOR ( )                           
*                                                                               
ALLPRD10 CLI   0(R4),C'@'          TEST FREE RIDER                              
         BNE   ALLPRD11                                                         
         OI    BUELPRSW,X'08'      SET BRAND A PAYS ALL                         
         LA    R4,1(R4)            POINT TO PRD                                 
         BCTR  R5,0                AND ADJUST LENGTH                            
*                                                                               
ALLPRD11 MVC   WORK(3),0(R4)                                                    
         CH    R5,=H'3'                                                         
         BE    ALLPRD12                                                         
         MVI   WORK+2,C' '                                                      
         CH    R5,=H'2'                                                         
         BNE   BUYERR                                                           
*                                                                               
ALLPRD12 L     R6,ASVCLIST                                                      
*                                                                               
ALLPRD14 CLC   0(3,R6),WORK                                                     
         BE    ALLPRD16                                                         
         LA    R6,4(R6)                                                         
         CLI   0(R6),C'A'                                                       
         BNL   ALLPRD14                                                         
         B     BUYERR                                                           
*                                                                               
ALLPRD16 LA    RE,BUELPRD                                                       
         CLI   0(RE),0                                                          
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         MVC   0(1,RE),3(R6)       SET PRD CODE                                 
*                                                                               
         CLI   3(R6),X'FF'         POL IS INVALID ALLOC                         
         BE    BUYERR                                                           
         CLC   =C'AAA',0(R6)       SO IS AAA                                    
         BE    BUYERR                                                           
* BUILD KEY FOR ESTHDR                                                          
         XC    KEY,KEY                                                          
         L     RE,ADRSVKEY                                                      
         MVC   KEY+1(3),0(RE)      A-M/CLT                                      
         MVC   KEY+4(3),0(R6)      PRD                                          
         MVC   KEY+7(1),9(RE)      EST                                          
* BUT CHECK TABLE FIRST (1 BIT PER BRAND)                                       
         ZIC   R4,3(R6)            PRD NUM                                      
         BCTR  R4,0                SUB 1                                        
         SRDL  R4,3                DIVIDE BY 8                                  
         LA    R4,SVPRDEST(R4)     POINT TO TABLE BYTE (BYTE 0 = 1-8)           
         LA    R6,X'0100'                                                       
         SRL   R5,29               SHIFT REMAINDER FOR BCT                      
         LA    R5,1(R5)            SET FOR BCT                                  
         SRL   R6,1                                                             
         BCT   R5,*-4                                                           
         EX    R6,*+8              TEST BIT ON                                  
         B     *+8                                                              
         TM    0(R4),0 *EXECUTED*                                               
         BO    EXIT                                                             
*                                                                               
* READ FOR ESTHDR                                                               
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOPRDEST                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
* SET BIT IN PRDEST TABLE                                                       
         STC   R6,BYTE                                                          
         OC    0(1,R4),BYTE                                                     
         MVI   TWA1FLAG,C'Y'       SET FLAG TO WRITE TWA1                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* ORBIT DESCRIPTION FIELDS -  DAY,TIME,PROGRAM,DEMO                             
*                                                                               
ORB      XC    BUORB,BUORB                                                      
         MVI   ERRCD,DAYERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         GOTO1 VCALLOV,DMCB,0,X'D9000A0E'  GET ADDRESS OF DAYPAK                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),((R5),(R4)),BUORBDAY,WORK                              
         CLI   BUORBDAY,0                                                       
         BE    BUYERR                                                           
*                                                                               
         MVI   ERRCD,TIMERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         GOTO1 VCALLOV,DMCB,0,X'D9000A0E'  GET ADDRESS OF TIMVAL                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),((R5),(R4)),BUORBTIM                                   
         CLI   0(R1),X'FF'                                                      
         BE    BUYERR                                                           
*                                                                               
         MVI   ERRCD,PGMERR                                                     
         GOTO1 FLDVAL                                                           
         CLI   FLEN+1,7                                                         
         BH    BUYERR                                                           
         LTR   R5,R5                                                            
         BZ    ORB10                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BUORBDES(0),0(R4)                                                
*                                                                               
ORB10    MVI   ERRCD,DEMERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    ORB20                                                            
         TM    FVAL,X'08'                                                       
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         STH   R0,BUORBDEM                                                      
*                                                                               
ORB20    B     EXIT                                                             
         EJECT                                                                  
* CONFIRMATION                                                                  
*                                                                               
CFMD     DS    0H                                                               
         MVI   ERRCD,INVERR                                                     
         GOTO1 FLDVAL                                                           
         CLI   FLEN+1,2                                                         
         BNE   BUYERR                                                           
*                                                                               
         LA    R5,CFDTAB                                                        
         LA    R6,3                                                             
         LA    R7,CFDTABX                                                       
         CLC   0(2,R4),0(R5)                                                    
         BE    *+12                                                             
         BXLE  R5,R6,*-10                                                       
         B     BUYERR                                                           
*                                                                               
         MVC   BUCFD,BDCFD                                                      
         IC    RE,2(R5)                                                         
         CLI   0(R5),C'U'                                                       
         BE    *+12                                                             
         EX    RE,CFDON                                                         
         B     *+8                                                              
         EX    RE,CFDOFF                                                        
         B     EXIT                                                             
*                                                                               
CFDON    OI    BUCFD,0                                                          
CFDOFF   NI    BUCFD,0                                                          
*                                                                               
CFDTAB   DC    C'CS',X'02'         CONFIRMED - START                            
         DC    C'CC',X'01'                   - CANCEL                           
         DC    C'CB',X'03'                   - BOTH                             
         DC    C'US',X'01'         UNCONFIRMED - START                          
         DC    C'UC',X'02'                     - CANCEL                         
         DC    C'UB',X'00'                     - BOTH                           
CFDTABX  EQU   *-1                                                              
         B     EXIT                                                             
         EJECT                                                                  
* RATING BOOK                                                                   
*                                                                               
BOOK     MVI   ERRCD,BOOKERR                                                    
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C'-'                                                      
         GOTO1 FLDVAL                                                           
         SPACE 1                                                                
* CREATE FLDHDR FOR BOOKVAL *                                                   
         SPACE 1                                                                
         XC    ELEM,ELEM                                                        
         STC   R5,ELEM+5                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),0(R4) *EXECUTED*                                       
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A00' GET BOOKVAL ADDRESS                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(C'N',ELEM),(1,DUB),VSCANNER                           
         CLI   4(R1),0                                                          
         BE    BUYERR                                                           
         TM    DUB,X'BF'           TEST ANY GARBAGE INPUT                       
         BNZ   BUYERR                                                           
         MVC   BUBOOK,DUB+1        SAVE BOOK                                    
*                                                                               
         CLI   FSTOP,C'-'                                                       
         BNE   EXIT                                                             
* EDIT HUT MONTH OVERRIDE                                                       
         MVI   ERRCD,HUTERR                                                     
         MVI   FSTOPS,0                                                         
         GOTO1 FLDVAL                                                           
         CLI   FLEN+1,3                                                         
         BNE   BUYERR                                                           
         MVC   DUB(3),0(R4)                                                     
         MVC   DUB+3(2),=C'15'                                                  
         GOTO1 VDATVAL,DMCB,(1,DUB),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    BUYERR                                                           
         PACK  DUB,WORK+2(2)                                                    
         CVB   R0,DUB                                                           
         SLL   R0,4                                                             
         STC   R0,BUHUTADJ                                                      
         B     EXIT                                                             
         EJECT                                                                  
* EDIT ELEMENT DATES. FORMAT IS  JAN06                                          
*                            OR  JAN06-01 FOR SPOT NUMBER 1                     
*                            OR  JAN06*03 FOR NUMBER OF SPOTS                   
*                            OR  JAN06-02*16 FOR POL NPW DATA                   
*                                                                               
ELDTVAL  DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BUELDT         SAVE PREVIOUS ELEMENT DATE!                  
         XC    BUELDATA,BUELDATA                                                
         MVI   ERRCD,INVDATE                                                    
         GOTO1 FLDVAL              UPDATE R4 TO THIS FIELD                      
*                                                                               
         CLI   0(R4),C'0'          IF IT'S A NUMBER                             
         BL    ELDTV1                                                           
         STCM  R0,3,BUELDT         RESTORE SPOT DATE                            
         LA    R6,BUELNUM                                                       
         B     ELDTV11             AND EDIT SPOT NUMBER ONLY                    
*                                                                               
* CHECK FOR 53 WEEK ESTIMATE                                                    
*                                                                               
ELDTV1   CLC   SVSTART(2),SVEND    TEST EST ALL IN ONE YEAR                     
         BE    ELDTV2                                                           
         CLC   SVEND+2(4),SVSTART+2 EST END MMDD > START MMDD                   
         BL    ELDTV2              NO                                           
         GOTO1 VDATVAL,DMCB,(0,(R4)),WORK  VALIDATE FOR M/D/Y                   
         ICM   R0,15,0(R1)         GET DATA LENGTH                              
         BZ    ELDTV2              IF NOT M/D/Y TRY FOR M/D                     
         AR    R4,R0               POINT TO REMAINING DATA                      
         B     ELDTV6              AND WE'VE GOT THE DATE!                      
                                                                                
* VALIDATE MONTH/DAY                                                            
ELDTV2   GOTO1 VDATVAL,DMCB,(1,(R4)),WORK                                       
         L     R0,0(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
         AR    R4,R0               POINT TO REMAINING DATA                      
* DETERMINE YEAR                                                                
         MVC   WORK(2),SVSTART                                                  
         CLC   SVSTART(2),SVEND    EST ALL IN ONE YEAR                          
         BE    ELDTV6              YES                                          
*                                                                               
         CLC   BDSTART(1),BDEND    BUY PERIOD IN ONE YEAR                       
         BNE   ELDTV4                                                           
         GOTO1 VDATCON,DMCB,(3,BDEND),WORK+10                                   
         MVC   WORK(2),WORK+10     USE BUY PERIOD YEAR THEN!                    
         B     ELDTV6                                                           
*                                                                               
ELDTV4   CLC   WORK+2(4),SVSTART+2  INPUT MMDD TO ES ST MMDD                    
         BNL   *+10                                                             
         MVC   WORK(2),SVEND                                                    
*                                                                               
ELDTV6   MVI   ERRCD,ESPERERR                                                   
         CLC   WORK(6),SVEND                                                    
         BH    BUYERR                                                           
         CLC   WORK(6),SVSTART                                                  
         BL    BUYERR                                                           
* MAKE SURE FEB 29 VALID FOR THIS YEAR                                          
         CLC   WORK+2(4),=C'0229'  TEST FEB29                                   
         BNE   ELDTV8                                                           
         MVI   ERRCD,INVDATE                                                    
         GOTO1 VGETDAY,DMCB,WORK,WORK+12                                        
         CLI   0(R1),0                                                          
         BE    BUYERR                                                           
*                                                                               
ELDTV8   DS    0H                                                               
         GOTO1 VDATCON,DMCB,WORK,(2,BUELDT)                                     
*                                                                               
         LA    R6,BUELNUM                                                       
         CLI   0(R4),C'-'                                                       
         BE    ELDTV10                                                          
         LA    R6,BUELNPW                                                       
         CLI   0(R4),C'*'                                                       
         BE    ELDTV10                                                          
         B     ELDTV24                                                          
         EJECT                                                                  
*                                                                               
* LOOK FOR NON-NUMERIC CHAR THAT TERMINATES NUMBER                              
*                                                                               
ELDTV10  LA    R4,1(R4)            BUMP PAST STOP CHAR                          
*                                                                               
ELDTV11  ST    R4,FADDR            SAVE START ADDRESS                           
         SR    R5,R5                                                            
*                                                                               
ELDTV12  CLI   0(R4),C'0'                                                       
         BL    ELDTV14                                                          
         CLI   0(R4),C'9'                                                       
         BH    ELDTV14                                                          
         LA    R4,1(R4)                                                         
         BCT   R5,ELDTV12                                                       
*                                                                               
ELDTV14  MVI   ERRCD,BADSPOT                                                    
         LPR   R5,R5                                                            
         BZ    BUYERR                                                           
         L     R4,FADDR            PICK UP START ADDRESS                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)   *EXECUTED*                                         
*                                                                               
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
*                                                                               
         STC   R0,0(R6)            SET EL NUM OR EL NPW                         
*                                                                               
         LA    R4,1(R4,R5)         POINT TO STOP CHAR                           
         TM    BUSTAT,X'80'        TEST POL NPW DATA                            
         BZ    ELDTV24                                                          
         CLI   0(R4),C'*'          TEST NPW FOLLOWS SPOT NUM                    
         BNE   ELDTV24                                                          
         CLI   BUELNPW,0           MAKE SURE NPW NOT EDITED YET                 
         BNE   ELDTV24             IF SO, PASS NORMAL ERROR                     
         LA    R6,BUELNPW          ELSE SET UP FOR EDIT                         
         B     ELDTV10              AND GO BACK                                 
*                                                                               
* ADJUST FLDVAL FOR NEXT EDIT                                                   
*                                                                               
ELDTV24  ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   ERRCD,BADOREG                                                    
         CLI   BUYKEY+3,X'FF'                                                   
         BE    EXIT                                                             
         CLI   0(R4),C','                                                       
         BE    EXIT                                                             
         CLI   BUTRCODE,C'C'       TEST CHANGE                                  
         BNE   *+12                                                             
         CLI   0(R4),C'='          ALLOW = STOP CHAR FOR SKED=                  
         BE    EXIT                                                             
         CLI   0(R4),C' '                                                       
         BH    BUYERR                                                           
         B     EXIT                                                             
         EJECT                                                                  
* COMMENTS                                                                      
*                                                                               
COM      MVI   ERRCD,CMREFERR                                                   
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'66'                                                       
         MVI   FSTOPS,C'-'                                                      
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,C'-'                                                       
         BNE   COM2                                                             
         CLI   FLEN+1,1                                                         
         BNE   COM2                                                             
         CLI   FSTOP,C'-'                                                       
         BNE   COM2                                                             
         B     COM3                                                             
COM2     C     R2,FLAST            ARE WE IN DSPLY AREA                         
         B     COM3                ***** NOP *****                              
         BNH   COM3                NO - MUST HAVE VALID REF NUM                 
         XC    FLEN,FLEN           ELSE SET TO RE-EDIT (NO REF NUM)             
         B     COM4                                                             
*                                                                               
COM3     LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         TM    FVAL,X'08'                                                       
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         STC   R0,ELEM+2                                                        
         CLI   ELEM+2,5                                                         
         BH    BUYERR                                                           
*                                                                               
COM4     MVI   ERRCD,1                                                          
         XC    FSTOPS,FSTOPS                                                    
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+3(0),0(R4) *EXECUTED*                                       
         LA    RE,4(R5)                                                         
         STC   RE,ELEM+1           SET ELEM LENGTH                              
         B     EXIT                                                             
         EJECT                                                                  
* REP                                                                           
*                                                                               
REP      MVI   ERRCD,REPERR                                                     
         MVI   FSTOPS,0                                                         
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         CHI   R5,3                                                             
         BH    BUYERR                                                           
         MVC   FULL(3),0(R4)                                                    
         OC    FULL,SPACES                                                      
         LHI   RE,VRCPACK-BUYSAVE                                               
         AR    RE,RA                                                            
         L     RF,0(RE)                                                         
         GOTO1 (RF),DMCB,(C'P',FULL),BUREP                                      
         BNZ   BUYERR                                                           
         GOTO1 (RF),(R1),(C'U',BUREP),DUB                                       
*                                                                               
         OC    BUREP,BUREP         TEST INPUT=000                               
         BNZ   REP2                                                             
         CLC   =C'LI',BUTRCODE     TEST RECALL ACTION                           
         BNE   EXIT                                                             
         OI    BUREP,X'80'         SET ZERO REP INPUT FLAG                      
         B     EXIT                                                             
* CHECK REP ON FILE                                                             
REP2     DS    0H                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PREFILL KEY WITH C'0'                        
                                                                                
         MVI   KEY,C'R'            READ REP RECORD                              
         MVC   KEY+1(1),BUYMD                                                   
         MVC   KEY+2(3),DUB                                                     
         MVC   KEY+5(2),AGYALPHA                                                
         L     R0,AREC                                                          
         L     RE,AREC2                                                         
         ST    RE,AREC                                                          
         GOTO1 RDSTA                                                            
         ST    R0,AREC                                                          
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE BUY RD SEQUENCE                      
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
* TAX                                                                           
*                                                                               
TAX      MVI   ERRCD,TAXERR                                                     
         MVI   FSTOPS,0                                                         
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   COS6                YES - EDIT AS PENNIES                        
* NEW TAX IS 5 DECIMAL PLACES                                                   
         GOTO1 VCASHVAL,DMCB,(3,(R4)),(R5)                                      
         CLI   0(R1),X'FF'                                                      
         BE    BUYERR                                                           
         L     R0,4(R1)                                                         
         STCM  R0,3,BUNTAX                                                      
         C     R0,=F'32767'                                                     
         BH    BUYERR                                                           
         B     EXIT                                                             
         EJECT                                                                  
*==================================================================             
* EDIT INPUT PARAMETERS FOR DEMO UPGRADES *                                     
* UPP= (OR UPT=), BK=,  DT=               *                                     
*                                                                               
* 07/19/01 IT TURNS OUT THAT A LOT OF THE INPUT TEXT IS NOT SAVED               
*          SO CREATE A NEW SAVE AREA SVUPTXT AND SAVE IT ALL !                  
*==================================================================             
         SPACE 1                                                                
UP       L     R8,ADBLOCK                                                       
         USING SPDEMUPD,R8                                                      
         XC    0(SPDEMUPL,R8),0(R8)                                             
         MVC   SPUPAREC,AREC2                                                   
         MVC   SPUPAFAC,VCOMFACS                                                
         MVC   SPUPAGY,AGYALPHA                                                 
         MVC   SPUPMED,BUYMD                                                    
         MVC   SPUPCLI,QCLT                                                     
         MVI   SPUPSRC,C'N'                                                     
         TM    SVCPROF+3,X'01'                                                  
         BZ    *+8                                                              
         MVI   SPUPSRC,C'A'                                                     
         MVC   SPUPFBK,SVBOOK                                                   
         OC    SVOPTBK,SVOPTBK                                                  
         BZ    *+10                                                             
         MVC   SPUPFBK,SVOPTBK                                                  
         MVC   SPUPSTA,QSTA                                                     
         MVC   SPUPDAY,BDDAY                                                    
         MVC   SPUPTIM,BDPROG                                                   
         CLI   BUTRCODE,C'B'                                                    
         BNE   *+16                                                             
         MVC   SPUPDAY,BUDAYS                                                   
         MVC   SPUPTIM,BUTIME                                                   
*                                                                               
         CLI   ADBLOCK,C'X'        TEST FORMAT DBLOCK REQUEST                   
         BE    *+12                YES                                          
         CLI   FLEN+1,2            TEST EDIT FOR DT= OR BK=                     
         BNE   UP2                 NO                                           
         MVI   ERRCD,NOUPOPTS                                                   
         CLI   SVUPFIL,0           TEST DEFAULT UPGRADE OPTS ENTERED            
         BE    BUYERR              NO                                           
         MVC   SPUPFIL,SVUPFIL                                                  
         MVC   SPUPTYPE(8),SVUPOPTS                                             
         LAY   RF,BUUPGD                                                        
         MVC   0(L'BUUPGD,RF),SVUPIN  MOVE FOR ELEM BUILD ROUTINE               
         B     UP8                                                              
*                                                                               
UP2      ST    R4,ELEM+252         SAVE INPUT START ADDRESS                     
         MVC   SPUPFIL,2(R4)       ON ENTRY R4 POINTS TO UPP= OR UPT=           
         CLI   2(R4),C'P'                                                       
         BE    *+14                                                             
         CLI   2(R4),C'T'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
* READ INPUT STRING FOR UPGRADE EXPRESSION AND BUILD FLDHDR *                   
         SPACE 1                                                                
         XC    ELEM(128),ELEM                                                   
         MVI   ERRCD,BADUPGD                                                    
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL              READ 'PUT,M82'                               
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         LA    R0,8(R5)                                                         
         STC   R0,ELEM                                                          
         STC   R5,ELEM+5                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),0(R4)                                                  
         LAY   RF,BUUPGD                                                        
         MVC   0(L'BUUPGD,RF),ELEM+8   SAVE INPUT EXPRESSION FOR LATER          
* GET UPVAL ADDRESS *                                                           
         SPACE 1                                                                
         GOTO1 VCALLOV,DMCB,0,X'D9000A13'                                       
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),ELEM,WORK,(C'/',VCOMFACS)                              
         CLI   0(R1),0             TEST VALID EXPRESSION                        
         BE    BUYERR                                                           
         MVC   SPUPTYPE(8),WORK+4                                               
*                                                                               
UP8      CLI   ADBLOCK,C'X'        TEST BUILD DBLOCK CALL                       
         MVI   ADBLOCK,C'U'        SET FLAG UPGRADE BLOCK BUILT                 
         BE    EXIT                AND EXIT IF BUILD CALL                       
         CLI   FSTOP,C'='                                                       
         BE    UP8X                                                             
         MVC   FSTOPS(2),=C',='                                                 
         GOTO1 FLDVAL                                                           
UP8X     CLC   =C'BK=',0(R4)                                                    
         BE    UP10                                                             
         CLC   =C'DT=',0(R4)                                                    
         BE    UP20                                                             
         MVI   BYTE,0                                                           
         CLC   =C'PUT=',0(R4)                                                   
         BE    UP30                                                             
         CLC   =C'SHR=',0(R4)                                                   
         BE    UP32                                                             
         CLC   =C'RTG=',0(R4)                                                   
         BE    UP32                                                             
         CLC   =C'RP=',0(R4)                                                    
         BE    UP30                                                             
         XC    FLEN,FLEN           SET TO ALLOW RE-EDIT                         
*                                                                               
         LHI   R1,SVUPTXT-BUYSAVE  POINT TO TEXT INPUT SAVE AREA                
         AR    R1,RA                                                            
         XC    0(L'SVUPTXT,R1),0(R1)                                            
         L     RE,FADDR            START OF NEXT FIELD                          
         L     RF,ELEM+252         START OF FIRST FIELD                         
***NOP   CLI   0(RE),C' '          TEST MORE INPUT FOLLOWS                      
***NOP   BNH   *+8                 NO                                           
***NOP   AHI   RE,-2               ADJUST INPUT LENGTH                          
         SR    RE,RF               LESS START OF FIRST FIELD                    
         BCTR  RE,0                ADJUST FOR EX                                
         MVC   0(0,R1),0(RF)                                                    
         EX    RE,*-6                                                           
         B     EXIT                                                             
         EJECT                                                                  
* EDIT OVERRIDE FOR SHARE BOOK *                                                
         SPACE 1                                                                
UP10     MVI   ERRCD,BOOKERR                                                    
         MVI   FSTOPS+1,C'('        BOOKTYPE FOLLOWS BOOK IN PARENS             
         GOTO1 FLDVAL                                                           
* CREATE FLDHDR FOR BOOKVAL *                                                   
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         XC    ELEM(128),ELEM                                                   
         LA    R0,8(R5)                                                         
         STC   R0,ELEM                                                          
         STC   R5,ELEM+5                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),0(R4)                                                  
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A00'                                       
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(C'N',ELEM),(1,DUB),VSCANNER                           
         CLI   4(R1),0                                                          
         BE    BUYERR                                                           
         TM    DUB,X'BF'           TEST ANY GARBAGE OPTIONS SPECIFIED           
         BNZ   BUYERR                                                           
         MVC   SPUPFBK,DUB+1                                                    
*                                                                               
         CLI   FSTOP,C'('          TEST FOR BOOKTYPE                            
         BNE   UP8                                                              
         LA    R4,1(R4,R5)         POINT TO (X) + ADJUST FOR BCT                
*                                                                               
****************************                                                    
*????  WHAT ABOUT 2 CHARACTER BOOKTYPES???                                      
****************************                                                    
         CLI   2(R4),C')'                                                       
         BNE   UP15                                                             
         CLI   1(R4),C'A'                                                       
         BL    BUYERR                                                           
         CLI   1(R4),C'9'                                                       
         BH    BUYERR                                                           
         MVC   SPUPBTYP,1(R4)                                                   
         MVC   BUBKTYPE,SPUPBTYP   AND SET IT HERE FOR BLDDEM ROUTINE           
*                                                                               
         LH    R0,FLEN             ADJUST FLEN FOR                              
         AHI   R0,2                1 CHAR BKTP AND )                            
         STH   R0,FLEN                                                          
         B     UP8                                                              
****************************                                                    
* SUPPORT 2 CHARACTER BOOKTYPES IN UPGRADES      -HWON 8/16/2016                
*                                      RELEASED  -HWON 9/15/2016                
****************************                                                    
UP15     CLI   3(R4),C')'          HAVE 2 CHAR BOOKTYPE?                        
         BNE   BUYERR                                                           
         CLI   2(R4),C'A'          MAKE SURE VALID ALPHANUMERIC                 
         BL    BUYERR                                                           
         CLI   2(R4),C'9'                                                       
         BH    BUYERR                                                           
         BRAS  RE,VAL2BKTP                                                      
*                                                                               
         LH    R0,FLEN             ADJUST FLEN FOR                              
         AHI   R0,3                2 CHAR BKTP AND )                            
         STH   R0,FLEN                                                          
         B     UP8                                                              
*                                                                               
         SPACE 1                                                                
* EDIT OVERRIDE DAY/TIME *                                                      
         SPACE 1                                                                
UP20     MVI   FSTOPS+1,C'/'                                                    
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,DAYERR                                                     
         CLI   FSTOP,C'/'                                                       
         BNE   BUYERR                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         GOTO1 VCALLOV,DMCB,0,X'D9000A03'  GET ADDRESS OF DAYPAK                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),((R5),(R4)),WORK,WORK+1                                
         CLI   WORK,0                                                           
         BE    BUYERR                                                           
         MVC   SPUPUDAY,WORK                                                    
* EDIT TIME *                                                                   
         MVI   ERRCD,TIMERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         GOTO1 VCALLOV,DMCB,0,X'D9000A0E'  GET ADDRESS OF TIMVAL                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),((R5),(R4)),WORK                                       
         CLI   0(R1),X'FF'                                                      
         BE    BUYERR                                                           
         MVC   SPUPUTIM,WORK                                                    
*                                                                               
         CLI   SPUPSTYP,C'P'       TEST PUT UPGRADE                             
         BE    *+10                YES                                          
         MVC   SPUPDAY(5),SPUPUDAY NO - OVRD REPLACES ACTUAL DAY/TIME           
*                                                                               
         CLI   FSTOP,C'/'                                                       
         BNE   UP8                                                              
* VALIDATE STATION OVERRIDE *                                                   
         MVI   FSTOPS+1,0                                                       
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,STAERR                                                     
         CLI   FLEN+1,3                                                         
         BL    BUYERR                                                           
         CLI   FLEN+1,4                                                         
         BH    BUYERR                                                           
         MVC   SPUPSTA,SPACES                                                   
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   SPUPSTA(0),0(R4)    *EXECUTED*                                   
         MVI   SPUPSTA+4,C'T'      FORCE MEDIA=T                                
         B     UP8                                                              
         SPACE 1                                                                
* EDIT PUT/SHR 2 YEAR AVERAGE EXPRESSIONS *                                     
         SPACE 1                                                                
UP30     OI    BYTE,X'80'                                                       
         CLC   =C'RP=',0(R4)                                                    
         BNE   UP34                                                             
*                                                                               
UP32     OI    BYTE,X'01'                                                       
*                                                                               
UP34     MVI   ERRCD,INVERR                                                     
         MVI   FSTOPS+1,0                                                       
         GOTO1 FLDVAL                                                           
         CLI   FLEN+1,1                                                         
         BNE   BUYERR                                                           
         CLI   0(R4),C'1'                                                       
         BE    UP8                                                              
         CLI   0(R4),C'2'                                                       
         BNE   BUYERR                                                           
         TM    BYTE,X'80'                                                       
         BZ    *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         TM    BYTE,X'01'                                                       
         BZ    *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
         B     UP8                                                              
         EJECT                                                                  
                                                                                
VAL2BKTP NTR1                                                                   
         ICM   RF,15,VCOMFACS                                                   
         BZ    BUYERR                                                           
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
VAL2BKT2 CLI   0(RF),X'FF'                                                      
         BE    BUYERR              INVALID BOOK TYPE                            
         CLC   SPBKTYPA,1(R4)                                                   
         BE    *+10                                                             
         AR    RF,RE                                                            
         B     VAL2BKT2                                                         
         MVC   SPUPBTYP,SPBKTYPN                                                
         MVC   BUBKTYPE,SPUPBTYP   AND SET IT HERE FOR BLDDEM ROUTINE           
         B     EXIT                                                             
         DROP  RF                                                               
                                                                                
* SUBR READS PARTNER EST TO TEST DATES IN EST PERIOD                            
*                                                                               
CHKPTNR  NTR1                                                                   
*                                                                               
         L     R0,AREC             SAVE I/O ADDRESS                             
         L     R8,AREC2                                                         
         USING ESTHDRD,R8                                                       
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R0,AREC             RESTORE I/O ADDRESS                          
* TEST DATES WITHIN PARTNER EST PERIOD                                          
         MVI   ERRCD,PTNRDTS                                                    
         CLC   BUSTART,ESTART                                                   
         BL    BUYERR                                                           
         CLC   BUEND,EEND                                                       
         BH    BUYERR                                                           
         B     EQXIT                                                            
         DROP  R8                                                               
* EDIT COUNTRY CODES                                                            
*                                                                               
CTYPE    BAS   RE,COUNTRY          CLIENT COUNTRY                               
         MVC   BUCTYPE,0(R4)                                                    
         B     EXIT                                                             
*                                                                               
STYPE    BAS   RE,COUNTRY          STATION COUNTRY                              
         MVC   BUSTYPE,0(R4)                                                    
         B     EXIT                                                             
*                                                                               
COUNTRY  LR    R0,RE               VALIDATE COUNTRY CODE                        
         MVI   ERRCD,INVERR                                                     
         GOTO1 FLDVAL                                                           
         CLI   FLEN+1,1                                                         
         BNE   BUYERR                                                           
         CLI   0(R4),C'C'                                                       
         BE    *+12                                                             
         CLI   0(R4),C'U'                                                       
         BNE   BUYERR                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* EDIT CANADIAN C58 TAX                                                         
*                                                                               
C58TAX   MVI   ERRCD,INVERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
         CLI   0(R1),0                                                          
         BNE   BUYERR                                                           
         L     RE,4(R1)                                                         
         LTR   RE,RE                                                            
         BM    BUYERR                                                           
         C     RE,=F'10000'                                                     
         BH    BUYERR                                                           
         STCM  RE,3,BUC58TAX                                                    
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE EXCHANGE RATE                                                        
*                                                                               
EXCHANGE MVI   ERRCD,INVERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         CLI   FLEN+1,6                                                         
         BH    BUYERR                                                           
         MVI   BYTE,0                                                           
         MVC   WORK(5),=C'00000'   RATE FORMAT = N.NNNN                         
*                                                                               
EXCH2    CLI   0(R4),C'.'          TEST DECIMAL POINT                           
         BNE   EXCH4                                                            
         CLI   BYTE,0              YES-TEST ALREADY ENCOUNTERED                 
         BNE   BUYERR                                                           
         MVI   BYTE,1                                                           
         LA    RE,WORK+1                                                        
         LA    RF,5                UP TO 4 DECIMAL PLACES                       
         B     EXCH8                                                            
*                                                                               
EXCH4    CLI   0(R4),C'0'          TEST NUMERIC                                 
         BL    BUYERR                                                           
         CLI   0(R4),C'9'                                                       
         BH    BUYERR                                                           
         CLI   BYTE,0              TEST BEFORE DEIMAL POINT                     
         BNE   EXCH6                                                            
         CLI   WORK,C'0'           YES-ONLY 1 NON-ZERO DIGIT BEFORE             
         BNE   BUYERR                  DECIMAL POINT                            
         MVC   WORK(1),0(R4)                                                    
         B     EXCH8                                                            
*                                                                               
EXCH6    MVC   0(1,RE),0(R4)                                                    
         LA    RE,1(RE)                                                         
         BCT   RF,EXCH8            UP TO 4 DECIMAL PLACES                       
         B     BUYERR                                                           
*                                                                               
EXCH8    LA    R4,1(R4)            NEXT CHARACTER                               
         BCT   R5,EXCH2                                                         
*                                                                               
         PACK  DUB,WORK(5)                                                      
         CVB   RE,DUB                                                           
         LTR   RE,RE                                                            
         BZ    BUYERR                                                           
         SRDL  RE,16                                                            
         LTR   RE,RE               TEST VALUE NOT TOO BIG                       
         BNZ   BUYERR                                                           
         STCM  RF,12,BUXRATE       STORE RATE                                   
*                                                                               
EXCHX    B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  EDIT NPW                                                                     
*                                                                               
ANPW     NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRCD,NPWERR                                                     
         MVI   FSTOPS+1,C'T'                                                    
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BNZ   NPW2                                                             
         CLI   BUTRCODE,C'B'       TEST NEW BUY                                 
         JNE   BUYERR              NO                                           
         MVI   BUNPW,1             SET DEFAULT NPW ON NEW BUY ONLY              
         LA    R4,1(R4)            POINT BEYOND STOP CHAR                       
         ST    R4,FADDR                                                         
         B     NPWX                                                             
NPW2     TM    FVAL,X'08'          TEST NUMERIC                                 
         JZ    BUYERR                                                           
         CLI   FLEN+1,2                                                         
         JH    BUYERR                                                           
         CVB   R0,DUB                                                           
         STC   R0,BUNPW                                                         
         LTR   R0,R0                                                            
         JZ    BUYERR                                                           
         TM    SVCOPT1,X'01'       TEST GMI CLIENT                              
         BO    NPW6                YES - MAX IS 2 DIGITS                        
*                                                                               
NPW4     CLI   BUNPW,75                                                         
         JH    BUYERR                                                           
*                                                                               
         TM    SVCOPT1,X'40'       TEST INFOMERCIAL CLIENT                      
         BZ    NPW6                                                             
         CLI   BUNPW,1                                                          
         JNE   BUYERR                                                           
*                                                                               
NPW6     CLI   BUTRCODE,C'C'       TEST CHANGE                                  
         BE    *+10                YES - DO NOT CHANGE TO POL NPW               
         OC    BUSTAT,SVPOLNPW                                                  
         TM    BUSTAT,X'80'                                                     
         BZ    *+12                                                             
         CLI   BUNPW,63            MAX SPOTS FOR POL NPW IS 63                  
         JH    BUYERR                                                           
*                                                                               
         CLI   FSTOP,C'T'          TEST NETPAK AVG IND                          
         BNE   NPWX                                                             
         TM    BUSTAT,X'01'        TEST NETPAK                                  
         JZ    BUYERR              NO                                           
         OI    BUSTAT,X'60'        SET SKED= DATA/AVG IND                       
* UPDATE INPUT POINTER                                                          
         LA    R4,2(R4,R5)         POINT BEYOND COMMA                           
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
NPWX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* EDIT COST                                                                     
*                                                                               
ACOST    NTR1  BASE=*,LABEL=*                                                   
         XC    BUCOST2,BUCOST2                                                  
         MVI   BUCOS2IND,0                                                      
         CLI   EDTVAL,COS2EDT                                                   
         BE    COST2                                                            
         TM    SVCOPT1,COP1COSQ    TEST 2 COSTS VALUES ALLOWED                  
         BO    COS0                                                             
         TM    SVCOPT3,COP3COSQ    OR OPTIONAL                                  
         BO    COS0                NO                                           
         TM    SVCOPT4,COP4TRD     OR TRADE                                     
         BO    COS0                                                             
         TM    SVCOPT4,COP4MIDS    TEST MIDAS                                   
         BZ    *+8                  NO                                          
COS0     MVI   FSTOPS+1,C'/'       SET FIELD SEPARATOR                          
*                                                                               
         MVI   UPNDX,SBUYCSTQ                                                   
         MVI   ERRCD,COSTERR                                                    
         MVI   BUCOSTYP,0                                                       
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         JZ    BUYERR                                                           
         CLI   FLEN+1,2                                                         
         BNE   COS1                                                             
         CLC   =C'-0',0(R4)                                                     
         BNE   COS1                                                             
         XC    BUCOST,BUCOST                                                    
         MVI   BUCIND,X'21'                                                     
         MVI   BUCIND2,0                                                        
         B     COSX                                                             
*                                                                               
COS1     CLC   =C'FREE',0(R4)                                                   
         BNE   COS2                                                             
         XC    BUCOST,BUCOST                                                    
         MVI   BUCIND,X'20'                                                     
         MVI   BUCIND2,0                                                        
         B     COSX                                                             
*                                                                               
COS2     MVI   BUCIND2,0                                                        
         MVI   BUCIND,X'80'                                                     
         CLI   0(R4),C'F'                                                       
         BE    COS4                                                             
         MVI   BUCIND,X'40'                                                     
         CLI   0(R4),C'Q'                                                       
         BE    COS4                                                             
         MVI   BUCIND,X'10'                                                     
         CLI   0(R4),C'N'                                                       
         BE    COS4                                                             
         MVI   BUCIND,X'08'                                                     
         CLI   0(R4),C'V'                                                       
         BE    COS4                                                             
         MVI   BUCIND,X'04'                                                     
         CLI   0(R4),C'S'                                                       
         BE    COS4                                                             
         MVI   BUCIND,X'02'                                                     
         CLI   0(R4),C'X'                                                       
         BE    COS4                                                             
         MVI   BUCIND,X'00'                                                     
         CLI   0(R4),C'P'                                                       
         BE    COS4                                                             
* TRADE                                                                         
         MVI   BUCIND,X'20'                                                     
         MVI   BUCIND2,X'02'                                                    
         TM    SVAFLAG1,X'20'      TEST CTA ACTIVE                              
         BZ    COS2X               IF NOT, T RATE NOT VALID                     
         CLI   0(R4),C'T'          TEST TRADE DOLLARS SPECIFIED                 
         BNE   COS2B               NO                                           
* T RATE MUST BE INPUT AGAINST T PRD ONLY                                       
         CLI   QPRD+2,C'T'         YES - PRD MUST END IN 'T'                    
         BE    COS4                NO                                           
         CLC   =C'NOTRADE',BUYBU   SPECIAL EXCEPTION FOR 'T' RULES              
         BE    COS4                NO                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOTTRADE)                                            
         J     BUYERR                                                           
* DID NOT INPUT T RATE, BUT CTA IS ACTIVE                                       
COS2B    CLC   =C'NOTRADE',BUYBU   SPECIAL EXCEPTION MAKES IT                   
         BE    COS3                NORMAL                                       
         CLI   QPRD+2,C'T'         TEST TRADE PRD                               
         BE    COS6                YES - LEAVE TRADE BITS ON                    
*                                                                               
COS2X    MVI   BUCIND,X'20'                                                     
         MVI   BUCIND2,X'80'                                                    
         CLI   0(R4),C'C'          TEST COMMISSION ONLY                         
         BE    COS4                                                             
* MUST BE GROSS OR INVALID                                                      
COS3     MVI   BUCIND,X'20'                                                     
         MVI   BUCIND2,0                                                        
         B     COS6                                                             
*                                                                               
COS4     LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         JZ    BUYERR                                                           
*                                                                               
COS6     GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
*                                                                               
         CLI   0(R1),X'FF'                                                      
         JE    BUYERR                                                           
*                                                                               
         L     RE,4(R1)                                                         
         LTR   RE,RE                                                            
         BNM   COS7                                                             
         LPR   RE,RE               SET RATE POSITIVE                            
         OI    BUCIND,X'01'        SET NEGATIVE RATE FLAG                       
         CLI   SVAPROF+10,C'0'                                                  
         BNH   COS7                                                             
         CLC   =C'FUND',BUYST      NO LIMIT ON THIS STATION (JWT)               
         BE    COS7                                                             
         TM    SVCOPT4,COP4MIDS    TEST MIDAS CLIENT                            
         BZ    COS6A               NO                                           
         CLI   SVMIDAS,C'C'        TEST MIDAS CREDIT STATION                    
         BE    COS7                YES - NO LIMIT ON CREDIT BUYS                
*                                                                               
COS6A    IC    R0,SVAPROF+10       GET CREDIT BUY LIMIT IN $100                 
         LA    R1,15                                                            
         NR    R1,R0               DROP HOB                                     
         M     R0,=F'10000'        CONVERT TO PENNIES                           
         CR    RE,R1                                                            
         JH    BUYERR                                                           
COS7     DS    0H                                                               
         ST    RE,FULL                                                          
         STCM  RE,7,BUCOST                                                      
*                                                                               
         CLI   EDTVAL,CUTEDT                                                    
         BE    COS8                                                             
         CLI   EDTVAL,TAXEDT       CANAD NTWK TAX IS IN PENNIES                 
         BE    COS8                                                             
*                                                                               
         CLI   FULL,0              DOES COST FIT IN 3 BYTES                     
         BNE   COS7A               NO                                           
         TM    BUSTAT,X'20'        TEST CANADIAN NETWORK                        
         BZ    *+8                 NO                                           
         OI    BUCIND2,X'01'       YES-INDICATE COST IN PENNIES                 
         B     COS8                                                             
*                                                                               
COS7A    SRDA  RE,32               CONVERT COST TO DOLLARS                      
         D     RE,=F'100'          DIVIDE BY 100                                
         C     RF,=F'2000000'      MAX IS 2 MILLION DOLLARS                     
         BNH   COS7B                                                            
         MVI   ERRCD,MAXCOST       COST IS INVALID                              
         J     BUYERR                                                           
*                                                                               
COS7B    STCM  RF,7,BUCOST                                                      
         ST    RF,FULL                                                          
         TM    BUSTAT,X'20'        TEST CANADIAN NETWORK                        
         BO    *+8                 YES                                          
         OI    BUCIND2,X'10'       INDICATE COST IN DOLLARS                     
*                                                                               
COS8     MVI   ERRCD,MAXCOST                                                    
         CLI   FULL,0              TEST ANY HOB'S ON                            
         JNE   BUYERR              YES - TOO BIG                                
*                                                                               
         TM    BUSTAT,X'20'        TEST CANADIAN NETWORK                        
         BZ    COS8A               NOPE                                         
         TM    BUCIND2,X'01'       COST IN PENNIES?                             
         BO    COS8A               YES, OK                                      
         CLC   FULL,=F'999999'     > 999,999?                                   
         JH    BUYERR              YES, ERROR                                   
*                                                                               
COS8A    CLI   EDTVAL,TAXEDT       NO SPECIAL RATES FOR TAX                     
         BE    COSX                                                             
*                                                                               
         LA    R1,SVERATE                                                       
         CLI   0(R1),C'*'          TEST OVERRIDE TO NORMAL RATES                
         BE    COSX                                                             
         CLI   0(R1),C'0'          TEST MANDATORY EST RATE TYPE                 
         BH    COS9A                                                            
         LA    R1,SVPRATE          MANDATORY RATE FOR PRODUCT                   
         CLI   0(R1),C'0'                                                       
         BH    COS9A                                                            
         LA    R1,SVCPROF+14                                                    
         CLI   SVCPROF+14,C'0'     TEST MANDATORY CLT RATE TYPE                 
         BE    COSX                NO                                           
*                                                                               
COS9A    CLI   SVCPROF+14,C'*'     TEST NO SPECIAL RATES ALLOWED                
         BNE   COS9B                                                            
         TM    BUCIND2,BDCCOMOQ    X'80'- C RATE(COMMISSION ONLY)?              
         JO    BUYERR               YES, SEND ERROR                             
         TM    BUCIND,BDCGROSQ     X'02' - TYPE MUST BE GROSS X'20'             
         BO    COSX                                                             
         J     BUYERR               O/W, SEND AN ERROR                          
*                                                                               
COS9B    DS    0H                                                               
         MVC   DUB(1),0(R1)        EXTRACT COST TYPE                            
         MVI   ERRCD,CPCOSERR                                                   
         CLI   DUB,C'8'                                                         
         JH    BUYERR                                                           
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    *+12                                                             
         CLI   DUB,C'8'                                                         
         JH    BUYERR                                                           
         NI    DUB,X'0F'                                                        
         ZIC   RE,DUB                                                           
         AR    RE,RE                                                            
         ZIC   RF,COSTAB-2(RE)                                                  
         TM    BUCIND,BDCMINSQ     X'01 - MINUS SPOT?                           
         BZ    *+8                  NO                                          
         LA    RF,1(RF)                                                         
         STC   RF,BUCIND                                                        
         IC    RF,COSTAB-1(RE)     GET SECOND BYTE                              
         STC   RF,DUB                                                           
         OC    BUCIND2,DUB                                                      
         B     COSX                                                             
*                                                                               
COSTAB   DC    X'0400',X'8000',X'1000',X'4000'   S/F/N/Q                        
         DC    X'0800',X'0200',X'0000',X'2080'   V/X/P/C                        
*                                                                               
COSX     DS    0H                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   *+8                                                              
         OI    BUCIND2,X'20'       SET CANADIAN FLAG                            
*                                                                               
         CLI   FSTOP,C'/'          TEST SECOND COST ENTERED                     
         JNE   EXIT                                                             
                                                                                
* EDIT SECOND COST FIELD                                                        
                                                                                
COST2    MVI   FSTOPS+1,0          RESET FIELD SEP                              
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOC2CHG)                                               
         TM    SVPWFLG,X'01'       STOP WIFM FROM CHANGING FACTOR               
         JO    BUYERR                                                           
         MVI   ERRCD,COST2ERR                                                   
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         JZ    BUYERR                                                           
         OI    BUCOS2IND,X'80'     SET FLAG FOR COST2 INPUT                     
         CLI   FLEN+1,3                                                         
         BNE   COST2A                                                           
         CLC   =C'DEL',0(R4)                                                    
         BNE   COST2A                                                           
         MVI   BUCOST2,X'FF'       SET DELETE COST 2 FLAG                       
         J     EXIT                                                             
*                                                                               
COST2A   DS    0H                                                               
         TM    SVCOPT1,COP1COSQ                                                 
         BO    COST2B                                                           
         TM    SVCOPT3,COP3COSQ                                                 
         BO    COST2B                                                           
         TM    SVCOPT4,COP4MIDS    TEST MIDAS                                   
         BO    COST2B                                                           
         TM    SVCOPT4,COP4TRD     OR TRADE                                     
         BZ    COST2T                                                           
*                                                                               
COST2B   GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
*                                                                               
         CLI   0(R1),X'FF'                                                      
         JE    BUYERR                                                           
*                                                                               
         MVI   ERRCD,MAXCOST                                                    
         ICM   RF,15,4(R1)                                                      
         BNM   *+8                                                              
         OI    BUCOS2IND,X'01'     SET COST2 NEGATIVE                           
         LPR   RF,RF                                                            
         STCM  RF,15,BUCOST2                                                    
         ST    RF,FULL                                                          
*                                                                               
         CLI   FULL,0              DOES COST FIT IN 3 BYTES                     
         BNE   COST2D              NO                                           
         TM    BUSTAT,X'20'        TEST CANADIAN NETWORK                        
         BZ    *+8                 NO                                           
         OI    BUCIND2,X'01'       YES-INDICATE COST IN PENNIES                 
         B     COST2X                                                           
*                                                                               
COST2D   SR    RE,RE               CONVERT COST TO DOLLARS                      
         D     RE,=F'100'          DIVIDE BY 100                                
         C     RF,=F'2000000'      MAX IS 2 MILLION DOLLARS                     
         BNH   COST2E                                                           
         MVI   ERRCD,MAXCOST       IT'S NOT VALID                               
         J     BUYERR                                                           
*                                                                               
COST2E   STCM  RF,15,BUCOST2                                                    
         ST    RF,FULL                                                          
         OI    BUCOS2IND,X'10'     SET FLAG FOR COST2 IN DOLLARS!               
*                                                                               
COST2F   MVI   ERRCD,MAXCOST                                                    
         CLI   FULL,0              TEST ANY HOB'S ON                            
         JNE   BUYERR              YES - TOO BIG                                
*                                                                               
         TM    BUSTAT,X'20'        TEST CANADIAN NETWORK                        
         BZ    COST2X              NO                                           
         TM    BUCIND2,X'01'       COST IN PENNIES?                             
         BO    COST2X              YES, OK                                      
         CLC   FULL,=F'999999'     > 999,999?                                   
         JH    BUYERR              YES, ERROR                                   
         B     COST2X                                                           
*                                                                               
COST2T   GOTO1 VCASHVAL,DMCB,(6,(R4)),(R5)   <=== TRADE HAS 6 DEC'S             
         CLI   0(R1),X'FF'                                                      
         JE    BUYERR                                                           
         L     RE,4(R1)                                                         
         STCM  RE,15,BUCOST2                                                    
         ST    RE,FULL                                                          
         OI    BUCOS2IND,X'80'     SET FLAG FOR COST2 INPUT                     
*                                                                               
COST2X   J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*SPDEMUPD                                                                       
       ++INCLUDE SPDEMUPD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
*SPGENEST                                                                       
       ++INCLUDE SPGENEST                                                       
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
       ++INCLUDE DEDEMTABD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPBUY10   02/26/21'                                      
         END                                                                    
