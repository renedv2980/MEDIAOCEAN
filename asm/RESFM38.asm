*          DATA SET RESFM38    AT LEVEL 048 AS OF 11/14/07                      
*PHASE T81838A                                                                  
*        TITLE 'T81838 - RESFM38 - NEW BUSINESS ACTIVITY REPORT REQ'            
*                                                                               
*********************************************************************           
*                                                                   *           
*        RESFM38 (T81838) --- BUS. ACT. REPORT, REQUEST SCREEN      *           
*                                                                   *           
*  JUN10/98 (AST) - BIRTH DATE                                      *           
*                                                                   *           
*  JAN03/00 (MLB) - ADDING SET FILTERING FOR OFFICE AND SALESPERSON *           
*                                                                   *           
*  AUG28/00 (BU ) - 'PENDING START DATE' FIELD                      *           
*                                                                   *           
*  JUN26/01 (BU ) - SET FILTERING:  STATION. PAGE BREAK: S/P        *           
*                                                                   *           
*  JAN06/03 (JRD) - FIX GROUP/SUBGROUP FILTERING                    *           
*                                                                   *           
*  MAR28/05 (BU ) - ADD DEMFILV TO LIST                             *           
*                                                                   *           
*  OCT24/07 (DE ) --- USE SOFT DEMO FILE OPEN LISTS                 *           
*                                                                   *           
*                                                                   *           
*  * NOTE R5 - REPIOBLK                                             *           
* ----------------------------------------------------------------- *           
*******************************************************************             
*                                                                               
T81838   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1838**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
*                                                                               
         B     XIT                                                              
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     DS    0H                                                               
         LA    RE,MYWORK           CLEAR COMMON WORK AREA                       
         LA    RF,MYWORKLQ                                                      
         XCEF                                                                   
                                                                                
* SET UP REPIO BLOCK                                                            
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
*                                                                               
         L     R1,AIO              IOAREA                                       
         ST    R1,RIPIOARE                                                      
         L     R1,DATAMGR          DATAMGR                                      
         ST    R1,RIPDTMGR                                                      
         MVC   RIPREP,AGENCY                                                    
*                                                                               
***  VALIDATE REPORT NAME/TYPE ***                                              
*                                                                               
         LA    R2,ARRNAMH                                                       
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RACTKEY,R4                                                       
         MVI   RACTTYP,RACTTYPQ                                                 
         MVC   RACTREP,AGENCY                                                   
         MVC   RACTNAME,ARRNAM                                                  
         OC    RACTNAME,SPACES     SPACE PAD THE LABEL NAME                     
*                                                                               
         GOTO1 READ                                                             
         BAS   RE,GETNMP           GET NUMBER OF PERIODS IN RECORD              
         MVC   BACTKEY,KEY         SAVE FOR REPORT PRG                          
*                                                                               
         B     FSTFLD                                                           
                                                                                
         EJECT                                                                  
*                                                                               
FSTFLD   DS    0H                                                               
         XC    SETFLG,SETFLG                                                    
*                                                                               
****************************************************************                
*    VALIDATE PENDING PERIOD - REQUIRED                                         
****************************************************************                
         LA    R2,ARRPERH                                                       
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
*                                                                               
         BAS   RE,VALPER           VALIDATE PERIOD                              
*                                                                               
* NEED TO SAVE FOR PERIOD COMPARISONS                                           
*                                                                               
NXFLD01  DS    0H                                                               
**>>>>>                                                                         
*                                                                               
****************************************************************                
*   VALIDATE PENDING START DATE                                                 
****************************************************************                
         SPACE 1                                                                
*                                                                               
         XC    WORK,WORK                                                        
         XC    WORK2,WORK2                                                      
*                                                                               
         LA    R2,ARRPSTAH         ANY PENDING START DATE?                      
         CLI   5(R2),0             NO                                           
         BE    NXFLD01A                                                         
         GOTO1 DATVAL,DMCB,(0,ARRPSTA),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,ACTPSTA)                                 
NXFLD01A DS    0H                                                               
**>>>>>                                                                         
****************************************************************                
*    VALIDATE PERIODS                                                           
****************************************************************                
         XC    PERFLG1(5),PERFLG1  CLEAR FLAGS TO NO DATA                       
*                                                                               
         LA    R2,ARRONEH                                                       
         LA    R3,PERFLG1          FIRST PERIOD INPUT FLAG                      
         LA    R4,YMDST1           BEGINNING OF PERIOD STORAGE                  
*                                                                               
VPERS00  CLI   5(R2),0             ANY INPUT?                                   
         BE    VPX                 NO                                           
*                                                                               
         LR    R1,R3               GET ADDRESS OF CURRENT PERIOD FLAG           
         LA    R0,PERFLG1          GET A(FIRST PER FLAG)                        
         SR    R1,R0               GET DIFFERENCE                               
         LA    R1,1(R1)            EQUALS NUMBER OF PER'S INPUT SO FAR          
         ZIC   R0,PERNUM           GET NUM PER'S ALLOWED                        
         CR    R1,R0                                                            
         BH    MYEND6              TOO MANY PERIODS INPUT                       
*                                                                               
         BAS   RE,VALPERS          VALIDATE THE PERIOD                          
* CHECK PERIODS WITHIN PENDING PERIOD                                           
         CLC   TYMDST,YMDST        IS PERIOD WITHIN PENDING PERIOD?             
         BL    MYEND5                                                           
         CLC   TYMDND,YMDND        IS PERIOD WITHIN PENDING PERIOD?             
         BH    MYEND5                                                           
*                                                                               
         MVC   0(3,R4),TYMDST                                                   
         MVC   3(3,R4),TYMDND                                                   
         MVI   0(R3),1             SET TO DATA RECEIVED                         
*                                                                               
VPERS50  ZIC   R1,0(R2)            BUMP TO NEXT # FIELD                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            BUMP TO NEXT INPUT FIELD                     
         AR    R2,R1                                                            
         LA    R4,6(R4)            POINT TO NEXT PERIOD IN STORAGE              
         LA    R3,1(R3)            CHECK NEXT PERIOD FLAG                       
         LA    R1,PERFLG5          LAST PERIOD FLAG                             
         CR    R3,R1               ARE WE PAST THE LAST FLAG?                   
         BNH   VPERS00             NO, CHECK NEXT                               
*        BCT   R3,VPERS00                                                       
*                                                                               
VPX      DS    0H                                                               
*                                                                               
****************************************************************                
*   VALIDATE ACTIVITY  -  DEFAULT=TODAY'S DATE                                  
****************************************************************                
         SPACE 1                                                                
*                                                                               
         XC    WORK,WORK                                                        
         XC    WORK2,WORK2                                                      
*                                                                               
         LA    R2,ARRADTEH                                                      
         CLI   5(R2),0                                                          
         BE    NXFLD05                                                          
         OC    ARFPBLK,ARFPBLK     IS IT RFP ?                                  
         BZ    PND52                                                            
         CLC   =Y(RE#RFPRD),9(R2)  ALREADY VALIDATED                            
         BE    NXFLD05             YES                                          
         CLC   =C'STEND',8(R2)     SYMBOLIC NAME ?                              
         BNE   PND52               NO - SEE IF DATE ENTERED                     
         BAS   RE,VALRFP           YES/LET RFP DO ITS THING                     
         B     NXFLD05                                                          
PND52    GOTO1 DATVAL,DMCB,(0,ARRADTE),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,ACTSTR)                                  
         MVC   ACTEND,ACTSTR                                                    
* - ARE THERE TWO DATES ?                                                       
         LA    R3,8(R2)                                                         
         LA    R1,9                LOOK FOR DASH                                
         CLI   0(R3),C'-'                                                       
         BE    PND53                                                            
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
* IF REACH HERE SHOULD ONLY BE ONE DATE                                         
         CLI   5(R2),8             MORE THAN ONE DATE?                          
         BH    ERREND              YES, ERROR                                   
*                                                                               
* CONVERT ONE INPUT DATE TO M-S ACTIVITY DATES OF THAT WEEK                     
* GET MONDAY                                                                    
         GOTO1 GETDAY,DMCB,WORK,NMDAY                                           
         ZIC   R6,0(R1)            GET NUMERIC VALUE OF DAY                     
         BCTR  R6,0                SUBTRACT ONE FROM DAY                        
         LNR   R6,R6               CONVERT TO NEGATIVE NUMBER                   
         GOTO1 ADDAY,DMCB,WORK,WORK2,(R6)                                       
         GOTO1 DATCON,DMCB,(0,WORK2),(3,ACTSTR)                                 
*                                                                               
* GET SUNDAY                                                                    
         GOTO1 GETDAY,DMCB,WORK,NMDAY                                           
         LA    R6,7                                                             
         ZIC   R0,0(R1)            GET NUMERIC VALUE OF DAY                     
         SR    R6,R0                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK2,(R6)                                       
         GOTO1 DATCON,DMCB,(0,WORK2),(3,ACTEND)                                 
*                                                                               
         B     PND55               DONE                                         
*                                                                               
* SUPPOSED TO BE TWO DATES                                                      
PND53    GOTO1 DATVAL,DMCB,(0,1(R3)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,ACTEND)                                  
*                                                                               
* CHECK ACTIVITY WITHIN PENDING PERIOD                                          
PND55    CLC   ACTSTR,YMDST        IS PERIOD WITHIN PENDING PERIOD?             
         BL    MYEND7                                                           
         CLC   ACTEND,YMDND        IS PERIOD WITHIN PENDING PERIOD?             
         BH    MYEND7                                                           
         CLC   ACTSTR,ACTEND       IS END BEFORE START DATE?                    
         BH    ERREND              YES, ERROR                                   
NXFLD05  DS    0H                                                               
*                                                                               
**********************************************************************          
*    VALIDATE OFFICE                                                            
**********************************************************************          
         XC    SETOFFS,SETOFFS                                                  
         XC    SETSALS,SETSALS                                                  
         XC    SETSTAS,SETSTAS                                                  
         XC    NEXTSET,NEXTSET                                                  
         LA    RE,BUFF                                                          
         ST    RE,NEXTSET          INDICATE NEXT OPEN SLOT                      
*                                                                               
         LA    R2,ARROFFH                                                       
         CLI   5(R2),0                                                          
         BE    NXFLD10                                                          
*                                                                               
         CLI   8(R2),C'*'          SET REQUEST?                                 
         BE    OFF05               YES                                          
         CLI   8(R2),C'?'          SET REQUEST (TSO INPUT)?                     
         BNE   OFF10               NO, CONTINUE                                 
OFF05    EQU   *                                                                
         MVC   SETFLG,=C'OF'       OFFICE FLAG                                  
*                                                                               
         BAS   RE,VALSET                                                        
         B     NXFLD10             CHECK NEXT, DON'T FILL IN RIPOFF             
*                                                                               
OFF10    DS    0H                                                               
         GOTO1 VALIOFF,DMCB                                                     
         MVC   RIPOFF,ARROFF                                                    
         B     NXFLD10                                                          
*                                                                               
NXFLD10  DS    0H                                                               
*                                                                               
********************************************************************            
*    VALIDATE STATION                                                           
********************************************************************            
         SPACE 1                                                                
         LA    R2,ARRSTAH                                                       
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BNE   STA02                                                            
         CLI   TWAWHEN,2           ,,IF IT'S SOON                               
         BNE   NXFLD15             NO                                           
         CLI   ARROFFH+5,0         ,,NEED STATION OR OFFICE                     
         BE    MYEND               ,,DO MY OWN ERROR MESSAGE                    
* IF STATION SIGN-ON, MUST HAVE INPUT                                           
         CLI   TWAACCS,C'$'                                                     
         BNE   NXFLD15             CHECK NEXT SCREEN FIELD                      
         B     ERREND                                                           
STA02    EQU   *                                                                
*                                                                               
         CLI   8(R2),C'*'          SET REQUEST?                                 
         BE    STA05               YES                                          
         CLI   8(R2),C'?'          SET REQUEST (TSO REQUEST)                    
         BNE   STA10               NO, CONTINUE                                 
STA05    EQU   *                                                                
*                                                                               
* IF STATION SIGN-ON, CAN'T BE A SET:  ONLY ALLOWED SINGLE STATION              
*                                                                               
         CLI   TWAACCS,C'$'                                                     
         BE    ERREND                                                           
         MVC   SETFLG,=C'ST'       STATION FLAG                                 
*                                                                               
*   TEST                                                                        
****     MVC   DIE(2),=X'0000'                                                  
*   TEST DUMP END                                                               
*                                                                               
         BAS   RE,VALSET                                                        
         B     NXFLD15             CHECK NEXT, DON'T FILL IN RIPOFF             
*                                                                               
STA10    DS    0H                                                               
*                                                                               
         GOTO1 VALISTA,DMCB                                                     
         MVC   RIPSTA,WORK                                                      
                                                                                
* IF STATION SIGN-ON, CHECK IF IT'S A VALID SIGN ON ID                          
         CLI   TWAACCS,C'$'                                                     
         BNE   NXFLD15                                                          
         L     R6,AIO                                                           
         USING RSTASOEL,R6                                                      
         MVI   ELCODE,6            GET VALID SIGN ON ID ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   NXFLD15                                                          
STA15    DS    0H                                                               
         CLC   RSTASID,TWAORIG     VALID SIGN-ON?                               
         BE    NXFLD15             YES, PROCEED                                 
         BAS   RE,NEXTEL           NOPE, CHECK NEXT ELEMENT                     
         BE    STA15                                                            
         B     ERREND              ALL DONE, NO MATCH, NOT VALID                
         DROP  R6                                                               
                                                                                
* WORK=CALL LETTER                                                              
* WORK+4  A-AM F-FM C-CM T=BLANK                                                
* WORK+10 MARKET NAME                                                           
* WORK+40  1 OR 2 IF SATELLITE STATION                                          
* WORK+41 GROUP/SUB GROUP CODE                                                  
*                                                                               
         SPACE 1                                                                
*                                                                               
NXFLD15  DS    0H                                                               
*                                                                               
****************************************************************                
*    VALIDATE GRP+SUB                                                           
****************************************************************                
         SPACE 1                                                                
         MVI   ERROR,INVALID                                                    
         LA    R2,ARRGRPH                                                       
         CLI   5(R2),0                                                          
         BE    NXFLD20                                                          
         GOTO1 VALIGRP,DMCB                                                     
         MVC   RIPGRP(2),ARRGRP                                                 
*                                                                               
         CLI   RIPSBGP,C' '        CLEAR SUBGROUP                               
         BH    *+8                                                              
         MVI   RIPSBGP,0                                                        
*                                                                               
         B     NXFLD20                                                          
                                                                                
* GROUP NAME WORK+10(10) SUBNAME WORK+20(10)                                    
                                                                                
*                                                                               
NXFLD20  DS    0H                                                               
*                                                                               
****************************************************************                
*    VALIDATE REGION                                                            
****************************************************************                
         SPACE 1                                                                
PND65    MVI   ERROR,INVALID                                                    
                                                                                
         LA    R2,ARRREGH                                                       
         CLI   5(R2),0                                                          
         BE    PND70                                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RREGKEY,R4                                                       
         MVI   RREGKTYP,X'03'                                                   
         MVC   RREGKREP,AGENCY                                                  
         MVC   RREGKREG,8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   RREGKEY,KEYSAVE                                                  
         BNE   ERREND                                                           
         MVC   FLTREG,8(R2)                                                     
         B     NXFLD25                                                          
         DROP  R4                                                               
*                                                                               
NXFLD25  DS    0H                                                               
*                                                                               
****************************************************************                
*    VALIDATE DIV+TEAM                                                          
****************************************************************                
         SPACE 1                                                                
PND70    LA    R2,ARRTEMH                                                       
         CLI   5(R2),0                                                          
         BE    NXFLD30                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RTEMKEY,R4                                                       
         MVI   RTEMKTYP,X'05'                                                   
         MVC   RTEMKREP,AGENCY                                                  
         MVC   RTEMKTEM,8(R2)                                                   
         GOTO1 HIGH                                                             
         CLI   5(R2),1             WAS ONLY GROUP ENTERED                       
         BE    PND72                                                            
         CLC   RTEMKEY,KEYSAVE                                                  
         BNE   ERREND                                                           
         MVC   RIPTEAM,8(R2)                                                    
         B     NXFLD30                                                          
         SPACE                                                                  
PND72    CLC   RTEMKEY(26),KEYSAVE    ONLY NEED DIVISION                        
         BNE   ERREND                                                           
         MVC   RIPTEAM(1),8(R2)                                                 
         B     NXFLD30                                                          
         DROP  R4                                                               
         SPACE 1                                                                
*                                                                               
NXFLD30  DS    0H                                                               
*                                                                               
****************************************************************                
*    VALIDATE SALESPERSON                                      *                
****************************************************************                
         SPACE 1                                                                
PND60    LA    R2,ARRSALH                                                       
         CLI   5(R2),0                                                          
         BE    NXFLD34                                                          
*                                                                               
         CLI   8(R2),C'*'          SET REQUEST?                                 
         BE    PND62               YES                                          
         CLI   8(R2),C'?'          SET REQUEST (TSO REQUEST)                    
         BNE   PND64               NO, CONTINUE                                 
PND62    EQU   *                                                                
         MVC   SETFLG,=C'SP'       SALESP FLAG                                  
         BAS   RE,VALSET                                                        
*                                                                               
         B     NXFLD35             CHECK NEXT, DON'T FILL IN RIPSAL             
*                                                                               
PND64    CLC   ARRSAL(2),=C'D-'    TEST TEAM OPTION                             
         BE    VALTEAM                                                          
         CLI   5(R2),3             TEST LENGTH ERROR                            
         BH    ERREND                                                           
*                                                                               
         MVI   KEY,6                                                            
         XC    KEY+1(21),KEY+1                                                  
         MVC   KEY+22(2),AGENCY                                                 
         MVC   KEY+24(3),ARRSAL                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         MVC   RIPSAL,ARRSAL                                                    
         OC    RIPOFF,RIPOFF               IF OFFICE                            
         BZ    NXFLD35                                                          
         GOTO1 GETREC                   GET REC AND MATCH ON OFFICE             
         L     R6,AIO                                                           
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSALELEM,R6                                                      
         CLC   RSALOFF,RIPOFF                                                   
         BNE   MYEND2                                                           
         B     NXFLD34                                                          
         DROP  R6                                                               
         EJECT                                                                  
VALTEAM  DS    0H                                                               
         CLI   5(R2),6                                                          
         BH    ERREND                                                           
         OC    ARRSAL,SPACES                                                    
*                                  SET KEY                                      
         MVI   KEY,5                                                            
         XC    KEY+1(22),KEY+1                                                  
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),ARRSAL+2                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         LA    R3,KEY                                                           
         USING RTEMREC,R3                                                       
*                                                                               
         MVC   RIPTEAM,RTEMKTEM                                                 
         B     NXFLD34                                                          
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
NXFLD34  XC    SETSALS,SETSALS                                                  
*                                                                               
NXFLD35  DS    0H                                                               
*                                                                               
*   TEST                                                                        
***      L     RE,SETOFFS                                                       
***      L     RF,SETSALS                                                       
***      DC    H'0'                                                             
*   TEST                                                                        
*                                                                               
****************************************************************                
*    VALIDATE ADVERTISER                                                        
****************************************************************                
         SPACE 1                                                                
PND80    LA    R2,ARRADVH                                                       
         CLI   5(R2),0                                                          
         BE    NXFLD40                                                          
         GOTO1 VALIADV                                                          
         MVC   RIPADV,WORK                                                      
         B     NXFLD40                                                          
                                                                                
* WORK= ADV CODE WORK+10=ADV NAME                                               
         SPACE 2                                                                
*                                                                               
NXFLD40  DS    0H                                                               
*                                                                               
******************************************************************              
*  VALIDATE AGENCY                                                              
******************************************************************              
         SPACE                                                                  
         LA    R2,ARRAGYH                                                       
         CLI   5(R2),0                                                          
         BE    NXFLD45                                                          
         OC    RIPAGY,SPACES       FILL WITH SPACES                             
         LA    R0,5                                                             
         LA    R1,8(,R2)                                                        
         LA    RE,RIPAGY                                                        
         SR    RF,RF                                                            
VAGY10   CLI   0(R1),C'-'                                                       
         BE    VAGY14                                                           
         CLI   0(R1),C' '                                                       
         BNH   VAGY14                                                           
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,VAGY10                                                        
         B     ERREND                                                           
         SPACE                                                                  
VAGY14   CLM   RE,1,5(R2)          THIS ALL THERE IS                            
         BE    VAGY20                                                           
         LA    R0,2                                                             
         LA    R1,1(,R1)                                                        
         LA    RE,RIPAGOFF                                                      
         SR    RF,RF                                                            
VAGY16   CLI   0(R1),C' '                                                       
         BNH   VAGY18                                                           
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,VAGY16                                                        
VAGY18   CH    RF,=H'2'                                                         
         BH    ERREND                                                           
         SPACE                                                                  
VAGY20   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RAGYKEY,R4                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY,RIPAGY                                                  
         OC    RAGYKAGY,SPACES                                                  
         MVC   RAGYKAOF,RIPAGOFF                                                
         OC    RAGYKAOF,SPACES                                                  
         MVC   RAGYKREP,RIPREP                                                  
         GOTO1 HIGH                                                             
         CLC   RAGYKEY,KEYSAVE                                                  
         BNE   ERREND                                                           
         SPACE                                                                  
         CLI   RIPAGOFF,X'40'                                                   
         BNE   NXFLD45                                                          
         XC    RIPAGOFF,RIPAGOFF                                                
         B     NXFLD45                                                          
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
NXFLD45  DS    0H                                                               
*                                                                               
*******************************************************************             
*  VALIDATE CLASS                                                               
*******************************************************************             
         LA    R2,ARRCLSH                                                       
         CLI   5(R2),0                                                          
         BE    NXFLD50                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCLSKEY,R4                                                       
         MVI   RCLSKTYP,X'0D'                                                   
         MVC   RCLSKREP,AGENCY                                                  
         MVC   RCLSKCLS,8(R2)                                                   
         OC    RCLSKCLS,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         SPACE                                                                  
         MVC   FLTCLS,8(R2)                                                     
         OC    FLTCLS,SPACES                                                    
         B     NXFLD50                                                          
         DROP  R4                                                               
         SPACE 2                                                                
*                                                                               
NXFLD50  DS    0H                                                               
*                                                                               
*******************************************************************             
* VALIDATE CATEGORY                                                             
*****************************************************************               
         LA    R2,ARRCATH                                                       
         CLI   5(R2),0                                                          
         BE    NXFLD55                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCTGKEY,R4                                                       
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,AGENCY                                                  
         MVC   RCTGKCTG,8(R2)                                                   
         OC    RCTGKCTG,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         MVC   RIPCAT,8(R2)                                                     
         B     NXFLD55                                                          
         DROP  R4                                                               
         EJECT                                                                  
NXFLD55  DS    0H                                                               
*******************************************************                         
* VALIDATE CONTRACT TYPE                                                        
*******************************************************                         
         MVI   REPORT,0                                                         
         LA    R2,ARRCTPH                                                       
         XC    CONTYPES(255),CONTYPES                                           
         CLI   5(R2),0                                                          
         BE    VCTYPX                                                           
*                                                                               
         MVI   BYTE,C'N'                                                        
         LA    R3,8(R2)                                                         
         CLI   0(R3),C'*'          SET REQUEST?                                 
         BE    VCTYP03             YES                                          
         CLI   0(R3),C'?'          SET REQUEST (TSO REQUEST)                    
         BNE   VCTYP05             NO, CONTINUE                                 
VCTYP03  EQU   *                                                                
         LA    R3,1(R3)                                                         
         MVI   BYTE,C'Y'           INDICATE MIGHT BE EXCLUDE                    
VCTYP05  EQU   *                                                                
*                                                                               
         LR    RE,R3                                                            
         LA    R1,8(R2)                                                         
         SR    RE,R1                                                            
         ZIC   R1,5(R2)                                                         
         SR    R1,RE               LENGTH OF SET NAME                           
         CH    R1,=H'4'            CHECK LENGTH                                 
         BH    ERREND                                                           
         CH    R1,=H'1'                                                         
         BL    ERREND                                                           
         BH    VCTYP10             SET RECORD                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'           ACCESS CONTRACT TYPE RECORD                  
         MVC   KEY+24(2),RIPREP    REP ALPHA CODE                               
         MVC   KEY+26(1),0(R3)     INSERT CONTRACT TYPE                         
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
*                                                                               
         MVC   CONTYPES,0(R3)                                                   
*                                                                               
         CLI   BYTE,C'Y'           EXCLUDE TYPE?                                
         BNE   *+8                                                              
         OI    REPORT,RPTQNCT      YES                                          
         B     VCTYPX                                                           
*                                                                               
VCTYP10  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'           ACCESS SET RECORD                            
         MVC   KEY+19(2),RIPREP    REP ALPHA CODE                               
         MVC   KEY+21(2),=C'CT'    INSERT SET TYPE                              
         MVC   KEY+23(4),0(R3)     INSERT SET NAME                              
         OC    KEY+23(4),SPACES                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        NEW DESCRIPTION ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   VCTYP12             MUST BE OLD SET RECORD                       
*                                                                               
         USING RSET1DES,R6                                                      
         TM    RSET1FLG,X'08'      EXCLUSION SET?                               
         BZ    *+8                                                              
         OI    REPORT,RPTQNCT      YES                                          
*                                                                               
         TM    RSET1FLG,X'80'      SET OF SETS?                                 
         BNZ   VCTYP20             YES                                          
         DROP  R6                                                               
*                                                                               
VCTYP12  DS    0H                  PROCESS NORMAL SET RECORD                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                SUBTRACT CONTROL                             
         SR    RF,RE                                                            
         LA    R3,CONTYPES                                                      
         LA    R0,255(R3)                                                       
         LA    R6,3(R6)                                                         
*                                                                               
VCTYP14  DS    0H                                                               
         MVC   0(1,R3),0(R6)                                                    
         LA    R3,1(R3)                                                         
         LA    R6,1(R6)                                                         
         CR    R3,R0                                                            
         BNL   MYEND3                                                           
         BCT   RF,VCTYP14                                                       
         B     VCTYPX                                                           
*                                                                               
VCTYP20  DS    0H                  PROCESS SET OF SETS                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
         L     RE,AIO2                                                          
         XC    0(255,RE),0(RE)                                                  
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    R0,3                SUBTRACT CONTROL                             
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),3(R6)       KEEP IDENTIFIERS HERE                        
         ST    RE,FULL             KEEP ADDRESS                                 
*                                                                               
VCTYP22  DS    0H                                                               
         L     RE,FULL                                                          
         CLC   0(4,RE),SPACES      ANY SET NAME?                                
         BNH   VCTYPX              NO                                           
*                                                                               
         LA    RF,4(RE)            SETUP NEXT SET NAME                          
         ST    RF,FULL                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'           ACCESS SET RECORD                            
         MVC   KEY+19(2),RIPREP    REP ALPHA CODE                               
         MVC   KEY+21(2),=C'CT'    INSERT SET TYPE                              
         MVC   KEY+23(4),0(RE)     INSERT SET NAME                              
         OC    KEY+23(4),SPACES                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        NEW DESCRIPTION ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   VCTYP24             MUST BE OLD SET RECORD                       
*                                                                               
         USING RSET1DES,R6                                                      
         TM    RSET1FLG,X'80'      SET OF SETS?                                 
         BNZ   VCTYP22             YES SKIP IT                                  
         DROP  R6                                                               
*                                                                               
VCTYP24  DS    0H                  PROCESS NORMAL SET RECORD                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                SUBTRACT CONTROL                             
         SR    RF,RE                                                            
         LA    R0,CONTYPES+255                                                  
         LA    R6,3(R6)                                                         
*                                                                               
VCTYP30  DS    0H                  CHECK FOR REPEATS                            
         LA    R3,CONTYPES                                                      
*                                                                               
VCTYP32  DS    0H                  CHECK FOR REPEATS                            
         CLI   0(R3),0             END OF TABLE                                 
         BE    VCTYP34             YES - INSERT CONTRACT TYPE HERE              
         CLC   0(1,R3),0(R6)       CONTRACT TYPE MATCH?                         
         BE    VCTYP36             YES - SKIP THIS ONE                          
         LA    R3,1(R3)                                                         
         CR    R3,R0                                                            
         BL    VCTYP32                                                          
         B     MYEND3                                                           
*                                                                               
VCTYP34  DS    0H                                                               
         MVC   0(1,R3),0(R6)                                                    
VCTYP36  DS    0H                                                               
         LA    R6,1(R6)                                                         
         BCT   RF,VCTYP30                                                       
         B     VCTYP22             NEXT SET NAME                                
*                                                                               
VCTYPX   DS    0H                                                               
         SPACE 2                                                                
*******************************************************                         
* VALIDATE STATUS FILTERS                                                       
********************************************************                        
         LA    R2,ARRFL1H                                                       
         LA    R3,4                                                             
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+12                YES                                          
         OI    REPORT,RPTQALL      SET FOR ALL REPORT                           
         B     VFILX               NO                                           
*                                                                               
VFIL00   CLI   8(R2),C'P'         PENDING                                       
         BNE   VFIL02                                                           
         OI    REPORT,RPTQPND                                                   
         B     VFIL50                                                           
VFIL02   CLI   8(R2),C'I'         INCOMPLETE                                    
         BNE   VFIL04                                                           
         OI    REPORT,RPTQINC                                                   
         B     VFIL50                                                           
VFIL04   CLI   8(R2),C'C'         COMPLETE                                      
         BNE   VFIL06                                                           
         OI    REPORT,RPTQCPL                                                   
         B     VFIL50                                                           
VFIL06   CLI   8(R2),C'L'         LOSSES                                        
         BNE   VFIL08                                                           
         OI    REPORT,RPTQLOS                                                   
         B     VFIL50                                                           
VFIL08   DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VFILX                                                            
         B     ERREND                                                           
*                                                                               
VFIL50   ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         BCT   R3,VFIL00                                                        
*                                                                               
VFILX    DS    0H                                                               
*******************************************************                         
* VALIDATE READY TO BOOK                                                        
*******************************************************                         
         MVI   RTBFLTR,C'B'        DEFAULT = BOTH                               
         LA    R2,ARRRTBH                                                       
         CLI   5(R2),0                                                          
         BE    VRTBX                                                            
*                                                                               
         CLI   8(R2),C'B'                                                       
         BE    VRTBX                                                            
*                                                                               
         MVI   RTBFLTR,C'Y'                                                     
         CLI   8(R2),C'Y'                                                       
         BE    VRTBX                                                            
*                                                                               
         MVI   RTBFLTR,C'N'                                                     
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
*                                                                               
VRTBX    DS    0H                                                               
         SPACE 2                                                                
*******************************************************                         
* SKIP SPACE BETWEEN CONTRACTS                                                  
********************************************************                        
         LA    R2,ARRSPCH                                                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   KSPACING,C'N'        DEFAULT TO NO                               
         B     SPCX                                                             
         CLI   8(R2),C'Y'                                                       
         BE    SPC10                                                            
         CLI   8(R2),C'N'                                                       
         BE    SPC10                                                            
         B     ERREND                                                           
*                                                                               
SPC10    DS    0H                                                               
         MVC   KSPACING,8(R2)                                                   
SPCX     DS    0H                                                               
*                                                                               
********************************************************************            
* DOLLARS - ROUND DOLLARS TO THOUSANDS OR PRINT EVERY DOLLAR                    
********************************************************************            
*                                                                               
         LA    R2,ARRRNDH                                                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   RDOLLAR,C'N'        DEFAULT TO NO                                
         B     DOLX                                                             
         CLI   8(R2),C'Y'                                                       
         BE    DOL10                                                            
         CLI   8(R2),C'N'                                                       
         BE    DOL10                                                            
         B     ERREND                                                           
*                                                                               
DOL10    DS    0H                                                               
         MVC   RDOLLAR,8(R2)                                                    
DOLX     DS    0H                                                               
********************************************************************            
*  VALIDATE NATIONAL/LOCAL OFFICE FLAG                                          
********************************************************************            
         SPACE 1                                                                
         LA    R2,ARRNLFH                                                       
         MVI   ERROR,INVALID                                                    
         XC    NLFLAG,NLFLAG                                                    
         CLI   5(R2),0                                                          
         BE    VCOMP                                                            
*                                                                               
         CLI   8(R2),C'N'                                                       
         BNE   *+14                                                             
         MVC   NLFLAG,8(R2)                                                     
         B     VCOMP                                                            
         CLI   8(R2),C'L'                                                       
         BNE   ERREND                                                           
         MVC   NLFLAG,8(R2)                                                     
         SPACE 1                                                                
*                                                                               
********************************************************************            
*  VALIDATE COMPETITIVE SHARE OPTION                                            
********************************************************************            
VCOMP    DS    0H                                                               
         LA    R2,ARRCOMPH                                                      
         MVI   ERROR,INVALID                                                    
         MVI   COMPOPT,C'N'                                                     
         CLI   5(R2),0                                                          
         BE    NXFLD11                                                          
*                                                                               
         CLI   8(R2),C'N'                                                       
         BE    VCOMP10                                                          
         CLI   8(R2),C'P'                                                       
         BE    VCOMP10                                                          
         CLI   8(R2),C'D'                                                       
         BNE   ERREND                                                           
VCOMP10  DS    0H                                                               
         MVC   COMPOPT,8(R2)                                                    
*                                                                               
NXFLD11  DS    0H                                                               
*                                                                               
*** END OF VALIDATION ***********************************************           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*** ROUTINES ********************************************************           
*                                                                               
*********************************************************************           
* THIS ROUTINE WILL GET THE AGENCY/MEDIA CODE                                   
*********************************************************************           
GETAGY   ST    RE,FULL                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     VPJ20                                                            
VPJ10    BAS   RE,NEXTEL                                                        
VPJ20    BE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R6),C'T'          WANT TELEVISION                              
         BNE   VPJ10                                                            
****     MVC   CAGYMED,3(R6)                                                    
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
         SPACE 1                                                                
*  RESET INTERNAL VALUES FOR REP                                                
         MVI   SYSTEM,C'R'                                                      
         MVC   LKEY,=H'27'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
*                                                                               
MYEND    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVC   CONHEAD+10(31),=C'SOON REQUIRES OFFICE OR STATION'               
         LA    R2,ARRSTAH                                                       
         B     MYENDX                                                           
                                                                                
MYEND2   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVC   CONHEAD+10(27),=C'OFFICE/SALESPERSON MISMATCH'                   
         B     MYENDX                                                           
                                                                                
MYEND3   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVC   CONHEAD+10(30),=C'TOO MANY CONTRACT TYPES IN SET'                
         B     MYENDX                                                           
                                                                                
MYEND4   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVC   CONHEAD+10(35),=C'PEND PERIOD MUST BE 2 YEARS OR LESS'           
         B     MYENDX                                                           
*                                                                               
MYEND5   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVC   CONHEAD+10(36),=C'PERIOD MUST BE WITHIN PENDING PERIOD'          
         B     MYENDX                                                           
*                                                                               
MYEND6   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVC   CONHEAD+10(36),=C'TOO MANY PERIODS INPUT, CHECK RECORD'          
         B     MYENDX                                                           
*                                                                               
MYEND7   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVC   CONHEAD+10(35),=C'ACTV. DATE MUST BE IN PENDING PERIOD'          
         B     MYENDX                                                           
*                                                                               
MYENDX   OI    CONHEADH+6,X'80'                                                 
         MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
SPTFLIST DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NDEMDIRA'                                                    
         DC    CL8'NDEMDIRN'                                                    
         DC    CL8'NL=DEMFA'                                                    
         DC    CL8'NL=DEMFN'                                                    
         DC    C'X'                                                             
         SPACE                                                                  
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
CTLSWTCH DC    C'N'                CONTROL FILE SWITCH                          
         EJECT                                                                  
*                                                                               
* ROUTINE TO VALIDATE A PERIOD FIELD                                            
VALPER   NTR1                                                                   
         MVI   ERROR,INVDATE                                                    
         OC    ARFPBLK,ARFPBLK     IS IT RFP ?                                  
         BZ    PND10                                                            
         CLC   =Y(RE#RFPMY),9(R2)  ALREADY VALIDATED?                           
         BE    VPERX               YES                                          
         CLC   =C'PERMY',8(R2)    'PERMY' SYMBOLIC NAME?                        
         BNE   PND10               NO - CHECK IF DATE ENTERED                   
         BAS   RE,VALRFP           YES - LET RFP DO IT'S THING                  
         B     VPERX                                                            
*                                                                               
PND10    BAS   RE,DASH                                                          
         LTR   R4,R4                                                            
         BNZ   PND15                                                            
*                                                                               
* SINGLE DATE VALIDATION                                                        
         CLI   5(R2),5             DATA FOR MORE THAN ONE DATE?                 
         BH    ERREND              BRANCH TO ERROR                              
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
*        CLI   OFFLINE,C'Y'        IF OFFLINE, GET BROADS                       
*        BNE   VPERX                                                            
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,RIPDATS)                               
         GOTO1 DATCON,DMCB,(0,WORK+12),(2,RIPDATE)                              
         B     PND20                                                            
                                                                                
* TWO DATE VALIDATION                                                           
PND15    BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),8(R2)        VALIDATE 1ST MON/YR                          
         GOTO1 DATVAL,DMCB,(2,DUB),WORK                                         
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         LA    R4,2(R4)            POINT R4 TO 2ND Y/M DATE                     
         LA    R3,8(R2)                                                         
         AR    R3,R4                                                            
         GOTO1 DATVAL,DMCB,(2,0(R3)),WORK+6                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         CLC   WORK(6),WORK+6                                                   
         BH    ERREND                                                           
*        CLI   OFFLINE,C'Y'                                                     
*        BNE   VPERX                                                            
                                                                                
* START DATE IN WORK - END DATE IN WORK+6                                       
* SET BROADCAST START DATE IN RIPDATS                                           
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+12,GETDAY,ADDAY                      
         GOTO1 DATCON,DMCB,(0,WORK+12),(2,RIPDATS)                              
                                                                                
* SET BROADCAST END DATE IN RIPDATE                                             
         GOTO1 GETBROAD,DMCB,(1,WORK+6),WORK+12,GETDAY,ADDAY                    
         GOTO1 DATCON,DMCB,(0,WORK+18),(2,RIPDATE)                              
                                                                                
                                                                                
*   RIPDATS-1 YEAR = READ START DATE FOR REPIO                                  
PND20    GOTO1 DATCON,DMCB,(2,RIPDATS),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),(0,WORK),-1                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,RIPDATSS)                                
         GOTO1 DATCON,DMCB,(2,RIPDATS),(3,YMDST)                                
         GOTO1 DATCON,DMCB,(2,RIPDATE),(3,YMDND)                                
*                                                                               
         BAS   RE,DASH                                                          
         LTR   R4,R4               ONE DATE?                                    
         BZ    VPERX               YES, EXIT                                    
*                                                                               
* PENDING PERIOD CAN ONLY SPAN 2 YEARS                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,YMDST),WORK                                       
         GOTO1 DATCON,DMCB,(3,YMDND),WORK+6                                     
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         CLC   8(2,R1),=H'730' 2 YEARS                                          
         BH    MYEND4                                                           
*&&DO                                                                           
         ZIC   R0,YMDST            GET YEAR OF START DATE                       
         ZIC   R1,YMDND            END DATE                                     
         SR    R1,R0                                                            
         LA    R0,2                                                             
         CR    R1,R0               2 YEARS?                                     
         BH    MYEND4              HIGHER, ERROR                                
         BNE   VPERX               LESS THAN 2 YEARS, DONE                      
*                                                                               
* TWO YEARS EXACTLY, MUST CHECK MONTH                                           
*                                                                               
         ZIC   R0,YMDST+1          GET MONTH OF START DATE                      
         ZIC   R1,YMDND+1          END DATE                                     
         CR    R1,R0               LATER END MO, 2 + YRS                        
         BH    MYEND4              HIGHER, ERROR                                
         BNE   VPERX               LESS THAN 2 YEARS, DONE                      
*                                                                               
* TWELVE MONTHS DIFFERENCE                                                      
*                                                                               
         ZIC   R1,YMDST+2          START DATE, DAY                              
         LA    R0,15                                                            
         CR    R1,R0               BEG OF MONTH?                                
         BL    MYEND4              YES, ERROR                                   
*&&                                                                             
VPERX    XIT1                                                                   
*                                                                               
*                                                                               
* ROUTINE TO VALIDATE A PERIOD FIELD                                            
VALPERS  NTR1                                                                   
         MVI   ERROR,INVDATE                                                    
         OC    ARFPBLK,ARFPBLK     IS IT RFP ?                                  
         BZ    PNDS10                                                           
         CLC   =Y(RE#RFPMY),9(R2)  ALREADY VALIDATED?                           
         BE    VPERSX              YES                                          
         CLC   =C'PERMY',8(R2)    'PERMY' SYMBOLIC NAME?                        
         BNE   PNDS10              NO - CHECK IF DATE ENTERED                   
         BAS   RE,VALRFP           YES - LET RFP DO IT'S THING                  
         B     VPERSX                                                           
*                                                                               
PNDS10   BAS   RE,DASH                                                          
         LTR   R4,R4                                                            
         BNZ   PNDS15                                                           
*                                                                               
* SINGLE DATE VALIDATION                                                        
         CLI   5(R2),5             DATA FOR MORE THAN ONE DATE?                 
         BH    ERREND              BRANCH TO ERROR                              
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
*        CLI   OFFLINE,C'Y'        IF OFFLINE, GET BROADS                       
*        BNE   VPERSX                                                           
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,TEMPDATS)                              
         GOTO1 DATCON,DMCB,(0,WORK+12),(2,TEMPDATE)                             
         B     PNDS20                                                           
                                                                                
* TWO DATE VALIDATION                                                           
PNDS15   BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),8(R2)        VALIDATE 1ST MON/YR                          
         GOTO1 DATVAL,DMCB,(2,DUB),WORK                                         
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         LA    R4,2(R4)            POINT R4 TO 2ND Y/M DATE                     
         LA    R3,8(R2)                                                         
         AR    R3,R4                                                            
         GOTO1 DATVAL,DMCB,(2,0(R3)),WORK+6                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         CLC   WORK(6),WORK+6                                                   
         BH    ERREND                                                           
*        CLI   OFFLINE,C'Y'                                                     
*        BNE   VPERSX                                                           
                                                                                
* START DATE IN WORK - END DATE IN WORK+6                                       
* SET BROADCAST START DATE IN RIPDATS                                           
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+12,GETDAY,ADDAY                      
         GOTO1 DATCON,DMCB,(0,WORK+12),(2,TEMPDATS)                             
                                                                                
* SET BROADCAST END DATE IN RIPDATE                                             
         GOTO1 GETBROAD,DMCB,(1,WORK+6),WORK+12,GETDAY,ADDAY                    
         GOTO1 DATCON,DMCB,(0,WORK+18),(2,TEMPDATE)                             
                                                                                
                                                                                
*   RIPDATS-1 YEAR = READ START DATE FOR REPIO                                  
PNDS20   GOTO1 DATCON,DMCB,(2,TEMPDATS),(0,WORK)                                
*        GOTO1 ADDAY,DMCB,(C'Y',WORK),(0,WORK),-1                               
*                                                                               
*        GOTO1 DATCON,DMCB,(0,WORK),(2,RIPDATSS)                                
         GOTO1 DATCON,DMCB,(2,TEMPDATS),(3,TYMDST)                              
         GOTO1 DATCON,DMCB,(2,TEMPDATE),(3,TYMDND)                              
                                                                                
*                                                                               
VPERSX   XIT1                                                                   
*                                                                               
* ROUTINE RETURNS LENGTH OF VALID FIELD BEFORE DASH IN R4 (LENGTH-1)            
* R2 POINTS TO FIELD HEADER                                                     
*                                                                               
DASH     NTR1                                                                   
         SR    R4,R4                                                            
         ZIC   R3,5(R2)            LENGTH OF INPUT FIELD                        
         LA    R6,8(R2)            POINT R6 TO DATA                             
DASH2    CLI   0(R6),C'-'                                                       
         BE    DASH4                                                            
         LA    R6,1(R6)                                                         
         LA    R4,1(R4)            LENGTH OF VALID FIELD BEFORE DASH            
         BCT   R3,DASH2                                                         
         SR    R4,R4               SET R4 TO INDICATE NO DASH                   
         B     DASHX                                                            
*                                                                               
DASH4    LTR   R4,R4               TEST DASH IN FIRST POSITION                  
         BZ    ERREND                                                           
DASHX    DS    0H                                                               
         XIT1  REGS=(R4)                                                        
                                                                                
         EJECT                                                                  
* RFP VALIDATION                                                                
VALRFP   NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
                                                                                
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING QRFPD,R3                                                         
         MVI   QRFPMODE,QRFPSYMB   SYMBOLIC NAME VALIDATION                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QRFPWORK(0),8(R2)   PASS SYMBOLIC NAME                           
         OC    QRFPWORK,SPACES                                                  
         GOTO1 RFP,DMCB,(R3)                                                    
         OC    QRFPWORK,QRFPWORK   ERROR                                        
         BZ    ERREND                                                           
         MVC   8(L'QRFPESC,R2),QRFPWORK                                         
         MVI   5(R2),8             SET LENGTH OF EXPLODED DATA                  
         MVI   11(R2),8            PASS LENGTH OF EXPLODED DATA                 
         OI    4(R2),X'20'         SET VALIDATED BIT                            
VALRFPX  XIT1                                                                   
         EJECT                                                                  
*                                                                               
* ROUTINE TO FIND THE NUMBER OF PERIODS ON THE BUS. ACTIVITY RECORD             
*                                                                               
GETNMP   NTR1                                                                   
         MVI   PERNUM,0            INITIALIZE NUMBER OF PERIODS                 
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'1B'        GET PERIOD 1                                 
         BAS   RE,GETEL                                                         
         BNE   GNPX                NOT FOUND? DONE                              
*                                                                               
         ZIC   R1,PERNUM                                                        
         LA    R1,1(R1)            INCREMENT NUMBER OF PERIODS                  
         STC   R1,PERNUM                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'1D'        GET PERIOD 2                                 
         BAS   RE,GETEL                                                         
         BNE   GNPX                NOT FOUND? DONE                              
*                                                                               
         ZIC   R1,PERNUM                                                        
         LA    R1,1(R1)            INCREMENT NUMBER OF PERIODS                  
         STC   R1,PERNUM                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'1F'        GET PERIOD 3                                 
         BAS   RE,GETEL                                                         
         BNE   GNPX                NOT FOUND? DONE                              
*                                                                               
         ZIC   R1,PERNUM                                                        
         LA    R1,1(R1)            INCREMENT NUMBER OF PERIODS                  
         STC   R1,PERNUM                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'21'        GET PERIOD 4                                 
         BAS   RE,GETEL                                                         
         BNE   GNPX                NOT FOUND? DONE                              
*                                                                               
         ZIC   R1,PERNUM                                                        
         LA    R1,1(R1)            INCREMENT NUMBER OF PERIODS                  
         STC   R1,PERNUM                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'23'        GET PERIOD 5                                 
         BAS   RE,GETEL                                                         
         BNE   GNPX                NOT FOUND? DONE                              
*                                                                               
         ZIC   R1,PERNUM                                                        
         LA    R1,1(R1)            INCREMENT NUMBER OF PERIODS                  
         STC   R1,PERNUM                                                        
GNPX     XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* VALIDATE SET REQUEST                                                          
*    SETFLG HAS TWO CHARACTER SET TYPE                                          
*    9(R2) THE SETID ON THE SCREEN                                              
***********************************************************************         
VALSET   NTR1                                                                   
*                                                                               
         XC    KEY,KEY             GET SET RECORD                               
         LA    R6,KEY                                                           
         USING RSETREC,R6                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,AGENCY                                                  
         MVC   RSETKSET,SETFLG                                                  
         MVC   RSETKID,9(R2)                                                    
         OC    RSETKID,SPACES                                                   
         DROP  R6                                                               
*                                                                               
         MVI   ERROR,NOTFOUND                                                   
DIE      EQU   *                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSETKEY),KEYSAVE                                           
         BNE   ERREND                                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R3,NEXTSET          A(NEXT OPEN SLOT IN TABLE)                   
         ST    R3,THISSET          SET A(SET IN PROGRESS)                       
         CLC   SETFLG,=C'OF'       OFFICE SET IN PROGRESS?                      
         BNE   VALSET01            NO                                           
         ST    R3,SETOFFS          YES - SET A(OFFICE SET)                      
         B     VALSET06                                                         
VALSET01 EQU   *                                                                
         CLC   SETFLG,=C'SP'       S/P    SET IN PROGRESS?                      
         BNE   VALSET02            NO                                           
         ST    R3,SETSALS          YES - SET A(S/P    SET)                      
         B     VALSET06                                                         
VALSET02 EQU   *                                                                
         CLC   SETFLG,=C'ST'       STATION SET IN PROGRESS?                     
         BNE   VALSET06            NO                                           
         ST    R3,SETSTAS          YES - SET A(STATION SET)                     
         B     VALSET06                                                         
VALSET06 EQU   *                                                                
         XC    0(4,R3),0(R3)       CLEAR THE TABLE FLAGS                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        NEW DESCRIPTION ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   VALSET10            MUST BE OLD SET RECORD                       
*                                                                               
         USING RSET1DES,R6                                                      
         TM    RSET1FLG,X'08'      EXCLUSION SET ?                              
         BZ    *+12                                                             
         OI    REPORT,RPTQNCT      YES                                          
         OI    0(R3),X'40'         YES--SET EXCLUSION SET FLAG                  
*                                                                               
         TM    RSET1FLG,X'80'      SET OF SETS ?                                
         BZ    *+8                                                              
         OI    0(R3),X'20'         YES--SET SET OF SETS FLAG                    
         DROP  R6                                                               
*                                                                               
VALSET10 DS    0H                  PROCESS NORMAL SET RECORD                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        SET MEMBERS ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
*                                                                               
         USING RSETMEMD,R6                                                      
         ZIC   RF,RSETMELN         GET ELEMENT LENGTH                           
         SHI   RF,RSETMTOV         SUBTRACT CONTROL                             
         MVC   1(2,R3),SETFLG      FILLING TABLE FLAGS                          
         MVC   3(1,R3),RSETMLEN    LENGTH OF EACH MEMBER OF SET                 
         ZIC   R4,RSETMLEN         GET ENTRY LENGTH                             
         LA    R6,RSETMTOV(R6)     FIRST ENTRY IN THE ELEMENT                   
         LA    R3,4(R3)            ADVANCE TO FIRST ENTRY IN TABLE              
         DROP  R6                                                               
*                                                                               
VALSET20 DS    0H                  MOVE MEMBERS TO TABLE                        
         SHI   R4,1                ADJUST FOR EXECUTE                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R6)       PUT SET MEMBER IN THE TABLE                  
         AHI   R4,1                ADJUST THE ENTRY LENGTH AGAIN                
         AR    R3,R4               ADVANCE THE TABLE ONE ENTRY                  
         XC    0(4,R3),0(R3)       CLEAR NEXT TABLE ENTRY                       
         AR    R6,R4               ADVANCE TO NEXT ENTRY IN SET DSECT           
         SR    RF,R4               RF-R4-->RF                                   
         CHI   RF,0                                                             
         BNE   VALSET20                                                         
*                                                                               
         LA    R3,1(R3)            ADVANCE THE TABLE BY ONE                     
         ST    R3,NEXTSET          SAVE A(NEXT SLOT/TABLE START)                
         LR    R5,R3               ADDRESS OF NEXT TABLE                        
         L     R3,THISSET          RESET A(SET IN PROGRESS)                     
         TM    0(R3),X'20'         SET OF SETS WAS PROCESSED ?                  
         BZ    VALSETX             NO--EXIT                                     
         AHI   R3,4                SKIP THE FLAGS                               
*                                                                               
VALSET30 XC    KEY,KEY             PROCESS SET OF SETS                          
         LA    R6,KEY                                                           
         USING RSETREC,R6                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,AGENCY                                                  
         MVC   RSETKSET,SETFLG                                                  
         SHI   R4,1                ADJUST FOR EXECUTE                           
         EX    R4,HERE                                                          
         B     *+10                                                             
HERE     MVC   23(0,R6),0(R3)      MOVE THE SET IDENTIFIER                      
         OC    RSETKID,SPACES                                                   
         AHI   R4,1                READJUST ENTRY LENGTH                        
         AR    R3,R4               R3 POINTS TO NEXT ENTRY IN THE TABLE         
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSETKEY),KEYSAVE                                           
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         DS    0H                  PROCESS NORMAL SET RECORD                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        SET MEMBERS ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
*                                                                               
         USING RSETMEMD,R6                                                      
         ZIC   RF,RSETMELN         GET ELEMENT LENGTH                           
         SHI   RF,RSETMTOV         SUBTRACT CONTROL                             
         ZIC   RE,RSETMLEN         GET ENTRY LENGTH                             
         LA    R6,RSETMTOV(R6)     FIRST ENTRY                                  
         DROP  R6                                                               
*                                                                               
VALSET40 DS    0H                  MOVE MEMBERS TO TABLE                        
         SHI   RE,1                ADJUST FOR EXECUTE                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R6)       PUT THE SET ENTRY IN THE TABLE               
         AHI   RE,1                ADJUST THE ENTRY LENGTH AGAIN                
         AR    R5,RE               ADVANCE THE TABLE ONE ENTRY                  
         XC    0(4,R5),0(R5)       CLEAR NEXT SLOT                              
         AR    R6,RE               ADVANCE TO NEXT ENTRY IN SET                 
         SR    RF,RE               RF-R4-->RF                                   
         CHI   RF,0                                                             
         BNE   VALSET40                                                         
*                                                                               
         OI    0(R3),0             ANYTHING MORE IN SET OF SETS?                
         BNZ   VALSET30            YES - GO BACK AND DO IT                      
         MVI   0(R5),0             NO  - END OF TABLES MARKER                   
         AHI   R5,1                ADVANCE THE TABLE BY ONE                     
         ST    R5,NEXTSET          ADDRESS OF NEXT TABLE                        
*                                                                               
VALSETX  DS    0H                                                               
         SPACE 2                                                                
*                                                                               
*                                                                               
VSX      XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
SETFLG   DS    CL2                 TWO CHARACTER SET FILTER                     
PERNUM   DS    XL1                 NUMBER OF PERIODS IN BUS ACT REC             
NMDAY    DS    CL3                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKB                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMFCD                                                                      
* REGENMKT                                                                      
* SRBLKD DSECT                                                                  
*      SPRANSIDD                                                                
* RESFMWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REGENREG                                                       
         EJECT                                                                  
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
       ++INCLUDE REGENSET                                                       
         EJECT                                                                  
       ++INCLUDE REGENTEM                                                       
         EJECT                                                                  
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE REGENCLS                                                       
         EJECT                                                                  
       ++INCLUDE REGENCTG                                                       
         EJECT                                                                  
       ++INCLUDE REGENACT                                                       
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
*        PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMDFD                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKB                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
       ++INCLUDE RESFM38WRK                                                     
         EJECT                                                                  
       ++INCLUDE REPIOBLK                                                       
* RFP INCLUDES                                                                  
       ++INCLUDE GEGENRFPD                                                      
       ++INCLUDE REDDEQUS                                                       
STAD     DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048RESFM38   11/14/07'                                      
         END                                                                    
