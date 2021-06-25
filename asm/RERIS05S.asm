*          DATA SET RERIS05S   AT LEVEL 042 AS OF 10/20/03                      
*          DATA SET RERIS05    AT LEVEL 041 AS OF 08/06/98                      
*PHASE T80D05A,*                                                                
*INCLUDE PERVERT                                                                
         TITLE 'RIS - T80D05 - READ AND FOUT LIST DATA'                         
***********************************************************************         
*                                                                     *         
*        RERIS05 --- RIS HEADLINE AND DETAIL LINE BUILDER             *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* FEB14/89 (MRR) --- ADD 'OPTION' - LISTS TRAFFIC #, AGY # AND ADV #  *         
*                      ON DETAIL LINE IN LIEW OF SALESPERSON OR PRDS  *         
*                                                                     *         
* 11/08/89  PJS  --- OLIST DISPLAY IS ACTUALLY TRAF#/ADV/AGY.         *         
*                    CHANGE HEADER TO REFLECT THIS.                   *         
*                                                                     *         
*                                                                     *         
* 12/04/92 (BU ) --- IMPLEMENT COMBO STATION REPORTING                *         
*                                                                     *         
* 05/21/93 (BU ) --- SET/RESET 'ACMBOSTA' TO SAVE DISPLACEMENT        *         
*                                                                     *         
*                                                                     *         
* 05/03/95 (PXZ) --- RESET STATION FOR COMBO READS IN LISTDONE        *         
*                                                                     *         
* 07/18/96 (BU ) --- RESET CALCSHR ROUTINE FOR LOSSES                 *         
*                                                                     *         
* 10/24/97 (BU ) --- ALTERNATE INFORMATION FOR TAKEOVER DISPLAY       *         
*                                                                     *         
* OCT24/97 (BU ) --- RERISWRKB --> RERISWRKC                          *         
*                    RGENEROL INCLUDED EXPLICITLY                     *         
*                                                                     *         
* MAR23/98 (JRD) --- NO PENNIES IN DISPLAY                            *         
*                                                                     *         
* MAR16/98 (BU ) --- IF CONFIRMED, DON'T SHOW 'WIP' (CFX-GENERATED)   *         
*                                                                     *         
* MAY13/99 (ROB) --- REVISED CONTRACT FLIGHT DATE                     *         
*                                                                     *         
* AUG01/01 (BU ) --- INCLUDE ALLHIST/BACKHIST IN TAKEOVER SWEEP       *         
*                                                                     *         
* MAR14/02 (HQ ) --- ADD PRODUCT NAME OR CODE FILTER (3 CHAR ONLY)    *         
*                                                                     *         
* APR30/02 (HQ ) --- LISTD DISPLAY INCORRECT PRODUCT NAME FIXED       *         
*                                                                     *         
* OCT09/03 (BU ) --- ULIST:  LAST LINE DISPLAYED IN ERROR: CORRECTED  *         
*                                                                     *         
*                                                                     *         
*                    **  END TOMBSTONE  **                            *         
***********************************************************************         
*                                                                               
T80D05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0D05*,R9,R7,RR=RE                                            
***      LA    R9,2048(RB)                                                      
***      LA    R9,2048(R9)                                                      
***      LA    R7,2048(R9)                                                      
***      LA    R7,2048(R7)                                                      
***      USING T80D05,RB,R9,R7                                                  
                                                                                
         L     RC,0(R1)                                                         
         USING T80DFFD,RA                                                       
         USING GENOLD,RC                                                        
         ST    RE,RELO2            SAVE RELOCATION FACTOR                       
         XC    KEYSAVE,KEYSAVE                                                  
         ZAP   PGTOT,=P'0'         CLEAR PAGE TOTAL                             
         LA    R2,RISTITLH                                                      
         EJECT                                                                  
*                                                                               
         CLI   PRNT,1              ARE WE PRINTING REPORT?                      
         BNE   TT00                                                             
         BAS   RE,INITP            YES/INITIALIZE                               
         BAS   RE,DOHEADS             PRINT HEADLINES                           
         B     FL12                                                             
                                                                                
XIT      XIT1                                                                   
                                                                                
*                                  TEST NEXT OPTION THIS TIME                   
* TEST NEXT OPTION THIS TIME                                                    
TT00     CLI   NEXTBYTE,0                                                       
         BE    FOUTITLE                                                         
         MVC   KEY,SAVEKEY                                                      
         MVC   LISTBYTE,LISTSAVE                                                
         MVC   LINK,LINKSAVE                                                    
         BAS   RE,HIGH             RDHI TO PRE-INITIALIZE READER                
         B     FIRSTLIN                                                         
                                                                                
FOUTITLE LA    R1,RISTITLH         CLEAR SCREEN                                 
FT00     ZIC   R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    FT10                                                             
         SH    R3,=H'9'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         FOUT  (R1)                                                             
         LA    R1,9(R3,R1)                                                      
         B     FT00                                                             
FT10     EQU   *                                                                
         GOTO1 =A(SETHEADS),DMCB,(RC),RR=YES                                    
*                                  PXZ                                          
         CLI   RISTATUS,2          HAVE WE READ FOR TOTALS?                     
         BE    SKIPMYP             DON'T CLEAR MYP(HAS DISK ADDRS)              
         MVI   MYP,X'40'           FIRST TIME IN SET MYP/2 TO SPACES            
         MVC   MYP+1(L'MYP-1),MYP                                               
         MVI   MYP2,X'40'                                                       
         MVC   MYP2+1(L'MYP2-1),MYP2                                            
SKIPMYP  EQU   *                                                                
*                                                                               
*                                                                               
FIRSTLIN DS    0H                                                               
         TM    VIEWS,X'02'         IF NOT LISTD                                 
         BO    FL10                                                             
         LA    R2,RISTTL2H         CLEAR 2ND TITLE LINE                         
         XC    RISTTL2,RISTTL2                                                  
         FOUT  (R2)                                                             
FL10     LA    R2,RISOUTH                                                       
         LR    R1,R2                                                            
         LA    R4,RISBOTMH                                                      
FL10B    ZIC   R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    FL10X                                                            
         SH    R3,=H'9'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         FOUT  (R1)                                                             
         LA    R1,9(R3,R1)                                                      
         CR    R1,R4               IF LAST LINE                                 
         BE    FL10X               LEAVE IT ALONE                               
         B     FL10B                                                            
FL10X    LA    R2,RISBOTMH                                                      
         OC    RISBOTM,=60X'40'    STEREO PROBLEM                               
         FOUT  (R2)                                                             
         LA    R2,RISBOTNH                                                      
         OC    RISBOTN,=60X'40'    STEREO PROBLEM                               
         FOUT  (R2)                                                             
                                                                                
         LA    R2,RISOUTH                                                       
                                                                                
FL12     EQU   *                                                                
         CLI   PFKEY,4             ARE WE SCROLLING BACK?                       
         BNE   FLSCRLX             NO                                           
         LA    RF,MYP                                                           
         LA    RE,66               MAX NUMBER OF ADDRESSES                      
FLSCRL10 CLI   0(RF),X'40'                                                      
         BE    FLSCRL15            GOT IT                                       
         LA    RF,4(RF)                                                         
         BCT   RE,FLSCRL10                                                      
FLSCRL15 C     RE,=F'65'                                                        
         BNL   FLSCRLX             MUST HAVE AT LEAST 2 SCREENS                 
         LA    R1,REPIOTBL                                                      
         USING REPIOD,R1                                                        
         S     RF,=F'8'                        BACK TO PREVIOUS SCREEN          
         MVC   RIPKEY+28(4),0(RF)              SET DISK ADDR TO KEY             
         MVC   0(8,RF),=X'4040404040404040'    CLEAR CURRENT AND PREV           
         OI    RIPSTAT,X'80'                   DO GETREC/READHI                 
         MVC   RIPFULL,VDATCON                 FUDGE DATCON IN RIPFULL          
         DROP  R1                                                               
FLSCRLX  EQU   *                                                                
                                                                                
         LA    RE,RECORD                                                        
         ST    RE,AIOAREA                                                       
*                                                                               
         USING LINE2,R2                                                         
READREC  GOTO1 VREAD,DMCB,(RC),LINK                                             
         CLI   DMCB+1,X'FF'        ARE WE OVER MAXIOCTR ?                       
         BE    MAXOUT              YES                                          
                                                                                
         CLI   ERRAREA,0           ERROR FREE ?                                 
         BNE   EXXMOD              NO - A PROBLEM                               
                                                                                
         LA    R1,REPIOTBL                                                      
         USING REPIOD,R1                                                        
         TM    RIPSTAT2,X'01'      NEED TO RESET COMBO TBL?                     
         BNO   READ05               NO                                          
*                                   YES                                         
* IN SCROLLING BACK IT MAY BE NECESSARY TO RESET COMBO TBL                      
         NI    RIPSTAT2,X'FF'-X'01' CLEAR BIT                                   
         L     RF,ACMBOSTA                                                      
         AR    RF,RA               POINT TO CURRENT STATION                     
         S     RF,=F'5'            POINT TO PREVIOUS STATON                     
         SR    RF,RA               GET DISPLACEMENT                             
         ST    RF,ACMBOSTA         AND SAVE DISPALCEMENT                        
         DROP  R1                                                               
*                                                                               
READ05   CLI   DMCB,X'FF'          END OF FILE ?                                
         BNE   READ10              NO - CONTINUE                                
         CLI   CMBOREP,C'Y'        YES - IS THIS A COMBO STATION RUN?           
         BNE   LISTDONE            NO  - FINISHED                               
         L     RF,ACMBOSTA         YES - CHECK FOR NEXT STATION                 
         AR    RF,RA               RE-ADDRESS REGISTER                          
         LA    RF,5(RF)            A(NEXT STATION IN LIST)                      
         LR    RE,RF                                                            
         SR    RE,RA               DON'T STORE ADDRESS -                        
*                                     STORE DISPLACEMENT                        
         ST    RE,ACMBOSTA         STORE A(NEW STATION) BACK                    
         CLI   0(RF),0             END OF LIST?                                 
         BE    LISTDONE            YES - FINISHED                               
         MVC   TBLSTA,0(RF)        LOAD IN NEXT STATION                         
* SET UP REPIOBLK FOR COMBO STATION SREADS                                      
         LA    R1,REPIOTBL                                                      
         USING REPIOD,R1                                                        
         MVC   RIPSTA,0(RF)        NEXT STATION                                 
         MVI   RIPSTAT,RIPRDHIO    AND GO HIGH                                  
*                                                                               
         MVI   RIPKEY,X'8E'                                                     
         MVC   RIPKEY+1(2),RIPREP                                               
         MVC   RIPKEY+3(5),0(RF)       NEXT STATION IN KEY                      
         MVC   RIPKEY+8(2),RIPDATSS    START DATE                               
         XC    RIPKEY+10(2),RIPKEY+10  CLEAR END DATE                           
                                                                                
         XC    LINK,LINK           SET FIRST TIME FLAG AGAIN!                   
         XC    LINKSAVE,LINKSAVE                                                
         B     READREC                                                          
         DROP  R1                                                               
*                                                                               
READ10   EQU   *                                                                
         MVC   LINK,4(R1)          SAVE KEY TYPE FROM READER                    
*                                                                               
* - VIEWS=X'01'= LIST$, X'02'=LISTD, X'04'=BUY$                                 
* - VIEWS=X'80' NO BOTTOM PAGE TOTALS                                           
*                                                                               
         CLI   PRNT,1              IF PRINTING                                  
         BE    READ20                                                           
         CLI   VIEWS,0             IS IT LIST$/LISTD/BUY$ ?                     
         BE    READ15              NO                                           
         TM    VIEWS,X'80'         YES  - GIVE BOTTOM PAGE TOTALS ?             
         BO    READ15              NO                                           
         B     NEWLIST             YES  - GO TO NEWLIST                         
*                                                                               
READ15   CLI   0(R2),0             TEST SCREEN FILLED                           
         BE    SETNEXT                                                          
* ALL LIST SCREENS NOW PF2 TO CONTRACT SCREEN                                   
* SO NEED TO SAVE LAST SCREEN LINE TO DISPLAY PF INFO                           
         LA    R1,RISBOTMH         TEST LAST LINE                               
         CR    R1,R2                                                            
         BE    SETNEXT                                                          
*                                                                               
READ20   MVC   SAVEKEY,KEY                                                      
                                                                                
*FOR PFKEY SCROLLING                                                            
* SAVE DISK ADDRESS OF FIRST REC IN - USE MYP/2 SINCE THESE ARE ONLY            
* USED WHEN PRINTING REPORT AND SO WE SAVE SPACE                                
*                                                                               
         CLI   RISTATUS,1          IN MIDDLE OF TOT READ                        
         BE    SCRLEND             DON'T SAVE THESE ADDRESSES                   
         CLI   NEXTBYTE,2          PXZ                                          
         BE    SCRLEND                                                          
         LA    R1,REPIOTBL                                                      
         USING REPIOD,R1                                                        
         LA    RF,MYP             USE MYP/MYP2 TO STORE DISK ADDRESS            
         LA    RE,66               MAX NUMBER OF ADDRS                          
SCRL10   CLI   0(RF),X'40'                                                      
         BE    SCRL20                                                           
         LA    RF,4(RF)                                                         
         BCT   RE,SCRL10              IF THEY GO OVER 66 SCREENS                
         B     SCRLEND                LOONEY TOONS                              
SCRL20   MVC   0(4,(RF)),RIPKEY+28    SAVE DISKADDR                             
         MVI   NEXTBYTE,2                                                       
                                                                                
SCRLEND  EQU   *                                                                
         DROP  R1                                                               
*****************************************************************               
         BAS   RE,GETDATA          LOAD STANDARD PART OF LINE                   
         CLI   LISTBYTE,LISTDLRS                                                
         BE    LSTDLR                                                           
         CLI   LISTBYTE,LISTUNCF   LIST UNCONFIRMED                             
         BE    LSTUNC                                                           
         CLI   LISTBYTE,LISTTRAF   LIST TRAFFIC INSTRUCTIONS                    
         BE    LSTTRAF                                                          
*                                                                               
NEXTLINE SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         LA    R2,0(R2,R5)                                                      
         B     READREC                                                          
         EJECT                                                                  
                                                                                
* - NEW LIST LOGIC TO READ ALL CONTRACTS FIRST FOR BOTTOM PAGE TOTAL            
*                                                                               
*      READ ALL CONTRACT RECORDS FIRST TIME IN KEEPING TOTAL IN                 
*      DLRTOTAL - WHEN SCREEN IS FULL SAVES KEY OF NEXT CONTRACT                
*      RECORD AND CONTINUES READ BUT SKIPS LOGIC THAT PUTS DATA TO              
*      SCREEN. AT EOF SETS SAVED KEY TO RIPKEY, TURNS ON RISTATUS               
*      TO INDICATE TOTAL READ DONE (=2), AND RETURNS TO NORMAL                  
*      PROCESSING.                                                              
*                                                                               
*   RISTATUS = 0  FIRST TIME IN - READ CONTRACT AND SET TO SCREEN               
*   RISTATUS = 1  IN MIDDLE OF READ - READ CONTRACT ONLY                        
*   RISTATUS = 2  FINISHED READ FOR TOTALS                                      
NEWLIST  EQU   *                                                                
         CLI   PRNT,1              ARE WE PRINTING REPORT                       
         BE    READ20              YES                                          
                                                                                
         LA    R1,RISBOTMH         ARE WE AT LAST SCREEN LINE?                  
         CR    R1,R2                                                            
         BNE   READ20              N0-DO NORMAL PROCESSING                      
*                                                                               
* CHK DATES                                                                     
         LA    R4,RCONELEM                                                      
         USING RCONELEM,R4                                                      
         ST    RE,SAVERE                                                        
*                                                                               
         CLC   RCONDATE(3),TBLBGN  TEST CONTRACT START/END DATES                
         BL    NEW03                                                            
         CLC   RCONDATE(3),TBLEND                                               
         BNH   NEW05                                                            
         B     READREC                                                          
*                                                                               
NEW03    CLC   RCONDATE+3(3),TBLBGN                                             
         BL    READREC                                                          
         DROP  R4                                                               
*                                                                               
                                                                                
* LAST SCREEN LINE                                                              
NEW05    CLI   RISTATUS,2          HAVE WE FINISHED READ FOR TOTS               
         BNE   NEW10               NO                                           
         BAS   R5,FOUTOTS          YES - SET TOTAL TO SCREEN                    
         B     SETNEXT             RETURN TO NORMAL PROCESSING                  
*                                                                               
* - CONTRACT READ TO GET REQUEST TOTAL                                          
* - ONLY GETS HERE FIRST TIME IN AFTER FIRST SCREEN IS FULL                     
NEW10    EQU   *                                                                
         CLI   RISTATUS,0          ,,IF FIRST TIME IN AND SCREEN FULL           
         BNE   NEW15                                                            
         LA    R1,REPIOTBL         ,,SAVE KEY TO CONTINUE READ AFTER            
         USING REPIOD,R1                                                        
         MVC   SAVEKEY,RIPKEY      ,,FIRST READ FOR TOTALS DONE                 
*                                                                               
         OC    ACOMBSV,ACOMBSV           HAVE WE SAVED COMBO LIST               
         BNZ   *+10                      YES                                    
         MVC   ACOMBSV,ACMBOSTA          NO SAVE IT NOW                         
*                                                                               
         DROP  R1                                                               
NEW15    MVI   RISTATUS,1          SET MIDDLE OF TOTAL READ FLAG                
         BAS   R5,LSTDLR           RETURNS $ IN R0                              
                                                                                
         CLI   DLRS,C'0'           SHOW 0 DOLLARS ONLY                          
         BNE   NEW20                                                            
         LTR   R0,R0                                                            
         BNZ   READREC                                                          
NEW20    CLI   DLRS,C'-'            EXCLUDE 0 DOLLARS                           
         BNE   NEW25                                                            
         LTR   R0,R0                                                            
         BZ    READREC                                                          
                                                                                
NEW25    CVD   R0,DUB                                                           
         AP    DLRTOTAL,DUB        ADD TO DOLLAR TOTALS                         
         L     R1,CONTOT           ADD TO CONTRACT NUMBER TOTAL                 
         LA    R1,1(R1)                                                         
         ST    R1,CONTOT                                                        
**       C     R1,=F'12'                                                        
**       BNE   *+6                                                              
**       DC    H'0'                                                             
***      BAS   RE,CALCSHR          CALCULATES $ SHARE                           
         GOTO1 =A(CALCSHR),RR=Y                                                 
         B     READREC             GET NEXT RECORD                              
         EJECT                                                                  
* SETS TOTALS AND PF INFO AT BOTTOM OF SCREEN FOR LIST REQUESTS                 
* RETURNS TO R5                                                                 
FOUTOTS  EQU   *                                                                
         CLI   PFJUMP,2                ,,IF LISTD                               
         BNE   FOUT02                                                           
         TM    VIEWS,X'80'             ,,AND NO TOTALS                          
         BNO   FOUT02                                                           
         LA    R2,RISBOTM                                                       
         MVC   0(L'PFTITL,R2),PFTITL   ,,SET PFTITL (PF1 AND PF2)               
****     FOUT  (R2)                                                             
****     LA    R2,RISBOTN                                                       
****     FOUT  (R2)                                                             
         BR    R5                      ,,AND GET OUT                            
                                                                                
*                                                                               
* - IF PFJUMP NOT = 0, PUT STANDARD PF1 INFO TO LAST LINE                       
FOUT02   CLI   PFJUMP,0                                                         
         BE    FOUT10                                                           
         LA    R2,RISBOTM                                                       
         MVC   0(L'PFTIT1,R2),PFTIT1    SET PF INFO                             
****     FOUT  (R2)                                                             
****     LA    R2,RISBOTN                                                       
****     FOUT  (R2)                                                             
                                                                                
FOUT10   CLI   VIEWS,0             LIST$/LISTD/BUY$ NOT = 0                     
         BER   R5                                                               
         TM    VIEWS,X'80'         DON'T GIVE TOTALS                            
         BOR   R5                                                               
                                                                                
         LA    R2,RISBOTM                                                       
* FOR LIST$                                                                     
         MVC   0(L'PFTIT1,R2),PFTIT1  SET PFKEY OPTION                          
         LA    R2,RISBOTN                                                       
         MVC   0(4,R2),=C'CON='                                                 
         EDIT  (B4,CONTOT),(5,4(R2)),ALIGN=LEFT                                 
* FOR LISTD                                                                     
         TM    VIEWS,X'02'         IF LISTD VIEW                                
         BNO   FOUT20                                                           
         XC    RISBOTM,RISBOTM                                                  
         LA    R2,RISBOTM                                                       
         MVC   0(L'PFTITL,R2),PFTITL  SET PFKEY OPTION                          
                                                                                
         LA    R2,RISBOTN                                                       
         OC    SHRPCT,SHRPCT       PRINT SHR %                                  
         BNZ   FOUT15                                                           
         CP    ALLSHR,=PL8'0'                                                   
         BE    FOUT15                                                           
         ZAP   WORK(16),SHRTOT     SHR $AMT INTO WORK                           
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),ALLSHR                                                  
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
         ST    R1,SHRPCT                                                        
                                                                                
FOUT15   MVC   11(4,R2),=C'SHR='                                                
         EDIT  (B4,SHRPCT),(7,15(R2)),2,TRAIL=C'%',ALIGN=LEFT                   
                                                                                
         MVC   0(4,R2),=C'CON='                                                 
         EDIT  (B4,CONTOT),(5,4(R2)),ALIGN=LEFT                                 
                                                                                
* - COMMON TO LIST$ AND LISTD                                                   
FOUT20   MVC   22(4,R2),=C'PAG='                                                
         ZAP   DUB,PGTOT                                                        
         GOTO1 =A(DIV100),DMCB,(RC),RR=Y                                        
         EDIT  (P8,DUB),(8,26(R2)),ALIGN=LEFT,FLOAT=-                           
                                                                                
*                                                                               
         MVC   34(4,R2),=C'REQ='                                                
         ZAP   DUB,DLRTOTAL                                                     
         GOTO1 =A(DIV100),DMCB,(RC),RR=Y                                        
         EDIT  (P8,DUB),(11,38(R2)),FLOAT=-,ALIGN=LEFT                          
         LA    R2,RISBOTMH                                                      
         FOUT  (R2)                                                             
         LA    R2,RISBOTNH                                                      
         FOUT  (R2)                                                             
         BR    R5                                                               
         EJECT                                                                  
FOUTER   CLI   LISTBYTE,LISTDLRS                                                
         BER   RE                                                               
         CLI   LISTBYTE,LISTUNCF   LIST UNCONFIRMED                             
         BER   RE                                                               
         CLI   LISTBYTE,LISTTRAF   LIST TRAFFIC DATA                            
         BER   RE                                                               
         LA    R2,RISOUTH                                                       
         XC    KEY,KEY                                                          
         LA    RE,IOAREA                                                        
         ST    RE,AIOAREA                                                       
FOUTER2  SR    R5,R5                                                            
         IC    R5,LISTBYTE                                                      
         B     BRANCH(R5)                                                       
         SPACE 2                                                                
BRANCH   DC    F'0'                                                             
         B     LSTDIV                                                           
         B     LSTSAL                                                           
         B     LSTAGY                                                           
         B     LSTADV                                                           
         B     LSTSTA                                                           
         B     LSTOFF                                                           
         B     LSTPRD                                                           
         B     LSTCTG                                                           
         DC    F'0'                LSTDLR                                       
         B     LSTGRP                                                           
         DC    F'0'                LSTUNC - LIST UNCONFIRMED                    
         DC    F'0'                LSTTRAF - LIST TRAFFIC                       
         SPACE 1                                                                
         EJECT                                                                  
GETDATA  LA    R4,RCONELEM                                                      
         USING RCONELEM,R4                                                      
         ST    RE,SAVERE                                                        
*                                                                               
         CLC   RCONDATE(3),TBLBGN  TEST CONTRACT START/END DATES                
         BL    DATA5                                                            
         CLC   RCONDATE(3),TBLEND                                               
         BNH   DATA8                                                            
         B     READREC                                                          
*                                                                               
DATA5    CLC   RCONDATE+3(3),TBLBGN                                             
         BL    READREC                                                          
                                                                                
                                                                                
*                                                                               
*                                                                               
DATA8    SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,XCFLD                                                         
*                                                                               
*DATA10   CLI   0(R4),0                                                         
*         BNE   *+6                                                             
*         DC    H'0'                                                            
*         CLI   0(R4),1                                                         
*         BE    DATA20                                                          
*         SR    R5,R5                                                           
*         IC    R5,1(R4)                                                        
*         LA    R4,0(R4,R5)                                                     
*         B     DATA10                                                          
*                                                                               
DATA20   DS    0H                                                               
* CHECK DOLLAR FILTER                                                           
* FUDGE RISTATUS AND USE LSTDLR TO GET $                                        
         CLI   DLRS,0              ARE WE FILTERING DOLLARS?                    
         BE    DATA23              NO                                           
                                                                                
         MVC   BYTE,RISTATUS       YES/SAVE RISTATUS VALUE                      
         MVI   RISTATUS,1          STATUS=1 MIDDLE OF TOTAL READ                
         BAS   R5,LSTDLR           RETURNS $ IN FULL AND R0                     
         MVC   RISTATUS,BYTE       RESET RISTATUS                               
                                                                                
         CLI   DLRS,C'0'              SHOW 0 DOLLARS ONLY                       
         BNE   DATA22                                                           
         LTR   R0,R0                                                            
         BNZ   READREC                                                          
DATA22   CLI   DLRS,C'-'               EXCLUDE 0 DOLLARS                        
         BNE   DATA23                                                           
         LTR   R0,R0                                                            
         BZ    READREC                                                          
                                                                                
DATA23   EQU   *                                                                
         DROP  R4                                                               
         LA    R4,RCONELEM                                                      
                                                                                
         TM    VIEWS,X'02'         IS IT LISTD VIEW?                            
         BO    DATANEW             YES                                          
*                                                                               
                                                                                
         MVC   L2OFF,RCONKOFF      0C KEY                                       
         MVC   L2AGY,RCONKAGY                                                   
         MVC   L2AOF,RCONKAOF                                                   
         MVC   L2ADV,RCONKADV                                                   
         MVC   L2GRP,RCONKGRP                                                   
         MVC   DUB(4),RCONKCON                                                  
         UNPK  WORK(9),DUB(5)                                                   
         MVC   L2CON,WORK+1                                                     
*                                                                               
DATA26   ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),X'17'         COMBO?                                       
         BE    DATA27                                                           
         CLI   1(R4),0                                                          
         BE    DATA28                                                           
         B     DATA26                                                           
DATA27   MVI   L2CON+(L'L2CON),X'83'              SET LOWER CASE 'C'            
                                                                                
* ERASE LEADING ZEROS OF CONTRACT NUMBER                                        
DATA28   LA    R4,RCONELEM                                                      
**       LA    R1,L2CON           REMOVE SINCE PFJUMP GETS FOULED               
**       CLI   0(R1),C'0'                                                       
**       BNE   *+16                                                             
**       MVI   0(R1),C' '                                                       
**       LA    R1,1(R1)                                                         
**       B     *-16                                                             
         MVC   L2STA,RCONKSTA                                                   
         MVC   L2MED,=C'TV'                                                     
         CLI   RCONKSTA+4,C' '                                                  
         BE    DATA30                                                           
         MVC   L2MED,=C'AM'                                                     
         CLI   RCONKSTA+4,C'A'                                                  
         BE    DATA30                                                           
         MVC   L2MED,=C'CM'        COMBINED STATIONS                            
         CLI   RCONKSTA+4,C'C'                                                  
         BE    DATA30                                                           
         MVC   L2MED,=C'FM'                                                     
         SPACE 2                                                                
DATA30   MVC   L2SAL,RCONSAL                                                    
         MVC   L2DIV,RCONTEM                                                    
         MVC   L2CTG,RCONCTGY                                                   
*                                                                               
         GOTO1 (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),L2PER1,0,RFBLOCK          
***      GOTO1 VDATCON,DMCB,(3,RCONDATE),(5,L2PER1)                             
***      MVI   L2PER1+8,C'-'                                                    
***      GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(5,L2PER2)                           
*                                                                               
         L     RE,SAVERE                                                        
         SPACE 1                                                                
         CLI   LISTBYTE,LISTPRD                                                 
         BNE   0(RE)                                                            
         CLC   RCONPRD,=C'   '     PRODUCT CODE                                 
         BE    DATA40                                                           
         OC    SAVEPRD,SAVEPRD                                                  
         BZ    *+14                                                             
         CLC   RCONPRD,SAVEPRD     SATISFY THE FILTER?                          
         BNE   READREC             NO, EXIT                                     
         MVC   L2NAME(2),=C'C='    MOVE CODE, GET NAME LATTER                   
         MVC   L2NAME+2(3),RCONPRD                                              
         BR    RE                                                               
         SPACE 1                                                                
DATA40   MVC   L2NAME,=CL20'NO PRODUCT GIVEN'                                   
LSTP     SR    R6,R6               NAME IS IN 05 ELEMENT                        
         IC    R6,1(R4)                                                         
         LA    R4,0(R6,R4)                                                      
         CLI   0(R4),5                                                          
         BE    LSTP2                                                            
         CLI   0(R4),0                                                          
         BNE   LSTP                                                             
         BR    RE                                                               
         SPACE 1                                                                
         USING RCONEXEL,R4                                                      
LSTP2    EQU   *                                                                
*        CLC   =C'Q1',RCONEXPR                                                  
*        BNE   *+6                                                              
*        DC    H'0'                                                             
         OC    SAVEPRD,SAVEPRD      PRODUCT FILTER?                             
         BZ    *+14                 NO                                          
         CLC   SAVEPRD,RCONEXPR     YES, PRODUCT NAME = FILTER?                 
         BNE   READREC              NO, SKIP TO NEXT REC                        
         MVC   L2NAME,RCONEXPR                                                  
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
* R4 POINTS TO 01 ELEMENT  KEY= 0C                                              
*                                                                               
         USING RCONELEM,R4                                                      
         USING LINE3A,R2                                                        
         USING LINE3B,R3                                                        
DATANEW  DS    0H                                                               
         CLI   PRNT,1              ARE WE PRINTING REPORT                       
         BE    PRNTDATA            YES-GO THERE                                 
                                                                                
         LA    R1,RISBOTMH         ARE WE AT LAST SCREEN LINE ?                 
         CR    R1,R2                                                            
         BE    SETNEXT             YES/SO STOP HERE                             
                                                                                
                                                                                
         ZIC   RE,0(R2)             GET ADDR OF 2ND LINE                        
         LA    R3,0(RE,R2)                                                      
         LR    RF,R3                CLEAR 2ND LINE                              
         ZIC   R1,0(RF)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,CLRLINE                                                       
         LA    RE,9(R1,RF)         AND 3D LINE AS WELL                          
         LR    RF,RE                                                            
         ZIC   R1,0(RF)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
CLRLINE  XC    8(0,RF),8(RF)                                                    
*                                                                               
* CHECK DOLLAR FILTER                                                           
* FUDGE RISTATUS AND USE LSTDLR TO GET $                                        
*        CLI   DLRS,0              ARE WE FILTERING DOLLARS?                    
*        BE    DATAN05             NO                                           
*        MVC   BYTE,RISTATUS       YES/SAVE RISTATUS VALUE                      
*        MVI   RISTATUS,1          STATUS=1 MIDDLE OF TOTAL READ                
*        BAS   R5,LSTDLR           RETURNS $ IN FULL AND R0                     
*        MVC   RISTATUS,BYTE       RESET RISTATUS                               
                                                                                
*        CLI   DLRS,C'0'              SHOW 0 DOLLARS ONLY                       
*        BNE   DATAN03                                                          
*        LTR   R0,R0                                                            
*        BNZ   READREC                                                          
*DATAN03  CLI   DLRS,C'-'               EXCLUDE 0 DOLLARS                       
*        BNE   DATAN05                                                          
*        LTR   R0,R0                                                            
*        BZ    READREC                                                          
                                                                                
DATAN05  EQU   *                                                                
*                                                                               
         DROP  R4                                                               
         LA    R4,RCONELEM         RESET R4(LSTDLR TRASHES IT)                  
         USING RCONELEM,R4                                                      
*                                                                               
         MVC   L3STA,RCONKSTA      STATION                                      
         MVC   L3OFF,RCONKOFF      OFFICE                                       
         BAS   RE,GETAGYNM         RETURNS AGENCY NAME IN WORK                  
         MVC   L3AGY,WORK                                                       
         BAS   RE,GETADVNM         RETURNS ADVERTISER NAME IN WORK              
         MVC   L3ADV,WORK                                                       
         BAS   RE,GETPRDNM         RETURNS PROD NAME IN WORK                    
         MVC   L3PRD,WORK                                                       
         BAS   RE,GETSLSNM         RETUNRS SALESPERSON'S NAME IN WORK           
         MVC   L3SLS,WORK                                                       
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK2,0,RFBLOCK                   
         GOTO1 VDATCON,DMCB,WORK2,(5,L3DATS)     START DATE                     
         GOTO1 VDATCON,DMCB,WORK2+6,(5,L3DATE)   END DATE                       
         UNPK  WORK(9),RCONKCON(5)                     UNSIGNED PACKED          
         MVC   L3CNT(7),WORK+1                                                  
                                                                                
* SET COMBO INDICATOR                                                           
DATAN07  ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),X'17'         COMBO?                                       
         BE    DATAN08                                                          
         CLI   1(R4),0                                                          
         BE    DATAN08A                                                         
         B     DATAN07                                                          
DATAN08  MVI   L3CNT+8,X'83'        SET LOWER CASE 'C'                          
         DROP  R4                                                               
DATAN08A EQU   *                                                                
         TM    VIEWS3,X'20'        TAKEOVERS ONLY?                              
         BNO   DATAN08Z            NO                                           
         LA    R4,RCONELEM         FIND                                         
DATAN08D EQU   *                                                                
         CLI   0(R4),0             END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                SHOULD BE ONLY PROCESSING TKOS               
         CLI   0(R4),X'2A'         MOVE HISTORY ELEMENT?                        
         BE    DATAN08H            YES -                                        
         CLI   0(R4),X'2C'         ALLHIST/BACKHIST ELEMENT?                    
         BE    DATAN08H            YES -                                        
         ZIC   RF,1(R4)                                                         
         AR    R4,RF               BUMP TO NEXT ELEMENT                         
         B     DATAN08D            GO BACK FOR NEXT                             
DATAN08H EQU   *                                                                
         USING RCONMMEL,R4                                                      
         UNPK  WORK(9),RCONMMOC(5) UNSIGNED PACKED: ORIGINAL CON#               
         MVC   L3DEMO(7),WORK+1                                                 
***->    GOTO1 VDATCON,DMCB,(2,RCONMMDT),(5,L3ACTV)                             
         BAS   RE,JOINDATE                                                      
         PRINT GEN                                                              
         GOTO1 VDATCON,DMCB,(3,STAJOIN),(5,L3ACTV)                              
         PRINT NOGEN                                                            
         B     DATAN17             SKIP DEMO                                    
*                                                                               
         DROP  R4                                                               
*                                                                               
DATAN08Z EQU   *                                                                
* GET DEMO                                                                      
DATAN09  LA    R4,RCONELEM                                                      
         USING RCONELEM,R4                                                      
         LR    R1,R4              R4 -> RCONELEM                                
DATAN10  ZIC   R0,1(R1)                                                         
         LTR   R0,R0                                                            
         BZ    DATAN17                                                          
         AR    R1,R0                                                            
         CLI   0(R1),X'12'         SAR                                          
         BE    DATAN12                                                          
         CLI   0(R1),X'10'         BOP                                          
         BNE   DATAN10                                                          
         USING RCONBPEL,R1                                                      
         LA    RE,RCONBPDM+1                                                    
         LA    RF,6                                                             
DATAN11  MVC   WORK(3),0(RE)                                                    
         TM    0(RE),X'40'         IS DEMO MARKED AS PRIMARY?                   
         BO    DATAN15                                                          
         LA    RE,3(RE)                                                         
         BCT   RF,DATAN11                                                       
         MVC   WORK(3),RCONBPDM+1  NO DEMO MARKED - USE 1ST                     
         B     DATAN15                                                          
         USING RSARCO,R1                                                        
DATAN12  LA    RE,RSARDEM                                                       
         LA    RF,8                                                             
DATAN12B MVC   WORK(3),0(RE)                                                    
         TM    0(RE),X'40'         IS IT PRIMARY DEMO?                          
         BO    DATAN15                                                          
         LA    RE,3(RE)                                                         
         BCT   RF,DATAN12B                                                      
         MVC   WORK(3),RSARDEM     NO DEMO MARKED AS PRIMARY/USE 1ST            
         B     DATAN15                                                          
         DROP  R1                                                               
DATAN15  NI    WORK,X'FF'-X'40'    CLEAR PRIMARY DEMO FLAG (X'40')              
         BAS   RE,GETDEMNM         RETURNS DEMO NAME IN WORK                    
         MVC   L3DEMO,WORK                                                      
                                                                                
* FUDGE RISTATUS AND USE LSTDLR TO GET $                                        
DATAN17  MVC   BYTE,RISTATUS       SAVE RISTATUS VALUE                          
         MVI   RISTATUS,1          STATUS=1 MIDDLE OF TOTAL READ                
         BAS   R5,LSTDLR           RETURNS $ IN FULL AND R0                     
         MVC   RISTATUS,BYTE       RESET RISTATUS                               
*                                                                               
         CVD   R0,DUB                                                           
         AP    PGTOT,DUB           ADD TO PAGE TOTALS                           
*                                                                               
         ZAP   WORK+20(8),DUB                                                   
         SRP   WORK+20(8),64-2,5                                                
         EDIT  (P8,WORK+20),L3DLR,FLOAT=$,MINUS=YES,ZERO=NOBLANK                
                                                                                
         CLI   RISTATUS,0          ,,STATUS=0=FIRST TIME IN                     
         BNE   DATAN18                                                          
         CVD   R0,DUB                                                           
         AP    DLRTOTAL,DUB        ,,ADD TO DOLLAR TOTALS                       
****     BAS   RE,CALCSHR          GET SHR $ AMT                                
         GOTO1 =A(CALCSHR),RR=Y                                                 
         L     R1,CONTOT           ,,ADD TO CONTRACT NUMBER TOTALS              
         LA    R1,1(R1)                                                         
         ST    R1,CONTOT                                                        
                                                                                
         DROP  R4                                                               
DATAN18  LA    R4,RCONELEM         RESET R4 (LSTDLR USES R4)                    
         USING RCONELEM,R4                                                      
*                                                                               
* SHARE                                                                         
         LR    R5,R4                                                            
DATAN18B ZIC   R1,1(R5)                                                         
         LTR   R1,R1                                                            
         BZ    DATAN19                                                          
         AR    R5,R1                                                            
         CLI   0(R5),X'06'                                                      
         BH    DATAN19                                                          
         BNE   DATAN18B                                                         
         USING RCONSPEL,R5                                                      
         TM    RCONSPES,X'04'      IS IT PERCENTAGE                             
         BNO   DATAN19                                                          
         EDIT  (B4,RCONSPAM),(7,L3SHR+3),2,TRAIL=C'%'                           
         B     DATAN19                                                          
         DROP  R5                                                               
* - CREATION DATE                                                               
DATAN19  OC    TBLCDATS,TBLCDATS                USING HEADER DATES?             
         BZ    DATAN19B                                                         
         GOTO1 VDATCON,DMCB,(3,RCONHDRD),(5,L3CRDAT)   HEADER                   
         B     DATAN20                                                          
DATAN19B GOTO1 VDATCON,DMCB,(3,RCONCREA),(5,L3CRDAT)    BUY                     
                                                                                
* GET SEND INFO                                                                 
DATAN20  DS    0H                                                               
         MVI   BYTE,0                                                           
***      TM    RCONMODR+1,X'40'    FOR GRAPHNET CONTRACTS, SHOW LAST            
***      BNO   *+8                 CONF DATE/TIME INSTEAD OF SENT               
***      MVI   BYTE,1                                                           
*                                                                               
         LA    R4,RCONELEM                                                      
         MVI   CNFNOWIP,C'N'       SET CONFIRMED/NO WIP TO 'NO'                 
DATAN20A ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    DATAN21                                                          
         AR    R4,R1                                                            
         CLI   0(R4),X'1F'         GET EXTENDED DESC ELEM                       
         BNE   DATAN20A                                                         
         TM    6(R4),X'40'         IF CONFIRMED                                 
         BNO   DATAN21                                                          
         MVI   CNFNOWIP,C'Y'       SET CONFIRMED/NO WIP TO 'YES'                
DATAN21  ZIC   R1,1(R4)            GET X'20' ELEMENT                            
         LTR   R1,R1                                                            
         BZ    DATANX                                                           
         AR    R4,R1                                                            
         CLI   0(R4),X'20'                                                      
         BNE   DATAN21                                                          
         CLI   BYTE,1                                                           
         BE    DATAN30                                                          
                                                                                
         USING RCONSEND,R4                                                      
                                                                                
         CLC   RCONSRV,RCONSSV     REP OR STATION VERSION HIGHER?               
         BH    DATAN40                                                          
***      CLC   RCONSRDT,RCONSSDT   REP OR STATION DATE HIGHER?                  
****     BH    DATAN40                                                          
                                                                                
         MVI   L3VER,C'S'                                                       
         EDIT  (1,RCONSSV),(3,L3VER+1),ALIGN=LEFT                               
         TM    RCONSENF,X'10'      STATION WORK IN PROGRESS?                    
         BO    DATAN30                                                          
         CLI   CNFNOWIP,C'Y'       IS ORDER CONFIRMED?                          
         BE    DATAN30             YES - DON'T DISPLAY 'WIP'                    
         MVC   L3SNT,=C'WIP  '   NO , DISPLAY SO                                
         B     DATAN50                                                          
*                                                                               
* STATION DATE AND TIME OF LAST SEND                                            
*                                                                               
DATAN30  DS    0H                                                               
         CLC   RCONSRDT,RCONSSDT   REP OR STATION DATE HIGHER?                  
         BH    DATAN45                                                          
         TM    VIEWS3,X'20'        TAKEOVERS ONLY: SPECIAL DISPLAY?             
         BO    DATAN30A                                                         
         GOTO1 VDATCON,DMCB,(2,RCONSSDT),(5,L3ACTV)                             
DATAN30A EQU   *                                                                
         OC    RCONSSTI,RCONSSTI                                                
         BZ    DATAN50                                                          
         MVC   L3SNT(2),RCONSSTI                                                
         MVI   L3SNT+2,C':'                                                     
         MVC   L3SNT+3(2),RCONSSTI+2                                            
         B     DATAN50                                                          
                                                                                
DATAN40  MVI   L3VER,C'R'                                                       
         EDIT  (1,RCONSRV),(3,L3VER+1),ALIGN=LEFT                               
         TM    RCONSENF,X'20'      REP WORK IN PROGRESS?                        
         BO    DATAN45                                                          
         CLI   CNFNOWIP,C'Y'       IS ORDER CONFIRMED?                          
         BE    DATAN45             YES - DON'T DISPLAY 'WIP'                    
         MVC   L3SNT,=C'WIP  '     NO , DISPLAY SO                              
         B     DATAN50                                                          
*                                                                               
* REP DATE AND TIME OF LAST SEND                                                
*                                                                               
DATAN45  DS    0H                                                               
         CLC   RCONSRDT,RCONSSDT   REP OR STATION DATE HIGHER?                  
         BNH   DATAN30                                                          
         TM    VIEWS3,X'20'        TAKEOVERS ONLY: SPECIAL DISPLAY?             
         BO    DATA45A             YES                                          
         GOTO1 VDATCON,DMCB,(2,RCONSRDT),(5,L3ACTV)                             
DATA45A  EQU   *                                                                
         OC    RCONSRTI,RCONSRTI                                                
         BZ    DATAN50                                                          
         MVC   L3SNT(2),RCONSRTI                                                
         MVI   L3SNT+2,C':'                                                     
         MVC   L3SNT+3(2),RCONSRTI+2                                            
                                                                                
DATAN50  DS    0H                                                               
         DROP  R4                                                               
         CLI   CNFNOWIP,C'Y'       CONFIRMED ORDER?                             
         BNE   DATAN60             NO                                           
         MVC   L3VER,=C'CNF '      MARK AS SUCH                                 
*                                                                               
DATAN60  EQU   *                                                                
         LA    R4,RCONELEM                                                      
                                                                                
                                                                                
DATANX   FOUT  (R2)                FOUT 1ST LINE                                
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         FOUT  (R2)                FOUT 2ND LINE                                
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               POINT R2 TO 3D LINE (BLANK IN LISTD)         
         LA    R1,RISBOTMH       ,,IS IT LAST SCREEN LINE ?                     
         CR    R1,R2                                                            
         BNE   NEXTLINE          ,,NO/ LET NORMAL PROCESS SKIP OVER IT          
         B     READREC           ,,YES/ GET NEXT RECORD                         
         EJECT                                                                  
* PUT DATA TO PRINT LINE                                                        
PRNTDATA DS    0H                                                               
*                                                                               
* CHECK $ FILTER                                                                
         CLI   DLRS,0              ARE WE FILTERING DOLLARS ?                   
         BE    PATAN05                                                          
* FUDGE RISTATUS AND USE LSTDLR TO GET $                                        
         MVC   BYTE,RISTATUS       SAVE RISTATUS VALUE                          
         MVI   RISTATUS,1          STATUS=1 MIDDLE OF TOTAL READ                
         BAS   R5,LSTDLR           RETURNS $ IN FULL AND R0                     
         MVC   RISTATUS,BYTE       RESET RISTATUS                               
                                                                                
         CLI   DLRS,C'0'              SHOW 0 DOLLARS ONLY ?                     
         BNE   PATAN03                                                          
         LTR   R0,R0                                                            
         BNZ   READREC                                                          
PATAN03  CLI   DLRS,C'-'               EXCLUDE 0 DOLLARS ?                      
         BNE   PATAN05                                                          
         LTR   R0,R0                                                            
         BZ    READREC                                                          
                                                                                
PATAN05  EQU   *                                                                
         LA    R4,RCONELEM                                                      
         USING RCONELEM,R4                                                      
                                                                                
         LA    R2,MYP                                                           
         USING PLIN3A,R2                                                        
         LA    R3,MYP2                                                          
         USING PLIN3B,R3                                                        
         XC    MYP,MYP                                                          
         XC    MYP2,MYP2                                                        
*                                                                               
         MVC   P3STA,RCONKSTA      STATION                                      
         MVC   P3OFF,RCONKOFF      OFFICE                                       
         BAS   RE,GETAGYNM         RETURNS AGENCY NAME IN WORK                  
         MVC   P3AGY,WORK                                                       
         BAS   RE,GETADVNM         RETURNS ADVERTISER NAME IN WORK              
         MVC   P3ADV,WORK                                                       
         BAS   RE,GETPRDNM         RETURNS PROD NAME IN WORK                    
         MVC   P3PRD,WORK                                                       
         BAS   RE,GETSLSNM         RETUNRS SALESPERSON'S NAME IN WORK           
         MVC   P3SLS,WORK                                                       
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK2,0,RFBLOCK                   
         GOTO1 VDATCON,DMCB,WORK2,(5,P3DATS)     START DATE                     
         GOTO1 VDATCON,DMCB,WORK2+6,(5,P3DATE)   END DATE                       
                                                                                
         UNPK  WORK(9),RCONKCON(5)                     UNSIGNED PACKED          
         MVC   P3CNT(7),WORK+1                         NEW CON #                
                                                                                
* SET COMBO INDICATIOR                                                          
PATAN07  ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),X'17'         COMBO?                                       
         BE    PATAN08                                                          
         CLI   1(R4),0                                                          
         BE    PATAN09                                                          
         B     PATAN07                                                          
PATAN08  MVI   P3CNT+(L'P3CNT),X'83'              SET LOWER CASE 'C'            
         DROP  R4                                                               
                                                                                
                                                                                
********************************************************************            
* GET OLD CONTRACT NUMBER IF TAKEOVER MODE                                      
PATAN09  TM    VIEWS3,X'20'        TAKEOVER ?                                   
         BNO   PTKOV20             NO                                           
         LA    R4,RCONELEM         YES/ GET OLD CONTRACT NUMBER                 
                                                                                
* GET TAKEOVER DARE ELEMENT                                                     
         LR    R1,R4              R4 -> RCONELEM                                
PTKOV10  ZIC   R0,1(R1)                                                         
         LTR   R0,R0                                                            
         BZ    PTKOV20                                                          
         AR    R1,R0                                                            
         CLI   0(R1),X'2A'         MOVE HISTORY                                 
         BNE   PTKOV10                                                          
         USING RCONMMEL,R1                                                      
         UNPK  WORK(9),RCONMMOC(5) PREVIOUS CONTRACT NUMBER                     
         MVC   P3DEMO(7),WORK+1                         NEW CON #               
         DROP  R1                                                               
         BAS   RE,JOINDATE         GET STATION JOIN DATE                        
         GOTO1 VDATCON,DMCB,(3,STAJOIN),(5,P3ACTV)                              
                                                                                
         B     PATAN16              SKIP GET DEMO IN TAKEOVER MODE              
****************************************************************                
                                                                                
PTKOV20  DS    0H                                                               
*                                                                               
                                                                                
* GET DEMO                                                                      
         LA    R4,RCONELEM                                                      
                                                                                
* GET DEMO                                                                      
         LR    R1,R4              R4 -> RCONELEM                                
PATAN10  ZIC   R0,1(R1)                                                         
         LTR   R0,R0                                                            
         BZ    PATAN17                                                          
         AR    R1,R0                                                            
         CLI   0(R1),X'12'         SAR                                          
         BE    PATAN12                                                          
         CLI   0(R1),X'10'         BOP                                          
         BNE   PATAN10                                                          
         USING RCONBPEL,R1                                                      
         MVC   WORK(3),RCONBPDM+1                                               
         B     PATAN15                                                          
         USING RSARCO,R1                                                        
PATAN12  MVC   WORK(3),RSARDEM                                                  
         B     PATAN15                                                          
         DROP  R1                                                               
PATAN15  NI    WORK,X'FF'-X'40'    CLEAR PRIMARY DEMO FLAG (X'40')              
         BAS   RE,GETDEMNM         RETURNS DEMO NAME IN WORK                    
         MVC   P3DEMO,WORK                                                      
*******************************************************************             
                                                                                
PATAN16  DS    0H                                                               
                                                                                
* GET DOLLARS                                                                   
* FUDGE RISTATUS AND USE LSTDLR TO GET $                                        
PATAN17  MVC   BYTE,RISTATUS       SAVE RISTATUS VALUE                          
         MVI   RISTATUS,1          STATUS=1 MIDDLE OF TOTAL READ                
         BAS   R5,LSTDLR           RETURNS $ IN FULL AND R0                     
         MVC   RISTATUS,BYTE       RESET RISTATUS                               
*                                                                               
         CVD   R0,DUB              ADD TO DOLLAR TOTALS                         
         AP    DLRTOTAL,DUB                                                     
*                                                                               
         SRP   DUB,64-2,5                                                       
         ZAP   WORK+20(8),DUB                                                   
         EDIT  (P8,WORK+20),P3DLR,FLOAT=$,MINUS=YES,ZERO=NOBLANK                
                                                                                
***      BAS   RE,CALCSHR          GET SHR $ AMT                                
         GOTO1 =A(CALCSHR),RR=Y                                                 
         L     R1,CONTOT           ,,ADD TO CONTRACT NUMBER TOTALS              
         LA    R1,1(R1)                                                         
         ST    R1,CONTOT                                                        
                                                                                
PATAN18  LA    R4,RCONELEM         RESET R4 (LSTDLR USES R4)                    
         USING RCONELEM,R4                                                      
                                                                                
                                                                                
* SHARE                                                                         
         LR    R5,R4                                                            
PATAN18B ZIC   R1,1(R5)                                                         
         LTR   R1,R1                                                            
         BZ    PATAN19                                                          
         AR    R5,R1                                                            
         CLI   0(R5),X'06'                                                      
         BH    PATAN19                                                          
         BNE   PATAN18B                                                         
         USING RCONSPEL,R5                                                      
         TM    RCONSPES,X'04'      IS IT PERCENTAGE                             
         BNO   PATAN19                                                          
         EDIT  (B4,RCONSPAM),(6,P3SHR+4),2,TRAIL=C'%'                           
         B     PATAN19                                                          
         DROP  R5                                                               
                                                                                
* - CREATION DATE                                                               
PATAN19  OC    TBLBDATS,TBLBDATS                USING BUY DATES?                
         BNZ   PATAN19B                                                         
         GOTO1 VDATCON,DMCB,(3,RCONHDRD),(5,P3CRDAT)   HEADER                   
         B     PATAN20                                                          
PATAN19B GOTO1 VDATCON,DMCB,(3,RCONCREA),(5,P3CRDAT)    BUY                     
*                                                                               
                                                                                
* GET SEND INFO                                                                 
                                                                                
PATAN20  ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PATANX                                                           
         AR    R4,R1                                                            
         CLI   0(R4),X'20'                                                      
         BNE   PATAN20                                                          
                                                                                
         USING RCONSEND,R4                                                      
                                                                                
         CLC   RCONSRV,RCONSSV     REP OR STATION VERSION HIGHER?               
         BH    PATAN40                                                          
**       CLC   RCONSRDT,RCONSSDT    REP OR STATION DATE HIGHER?                 
**       BH    PATAN40                                                          
         MVI   P3VER,C'S'                                                       
         EDIT  (1,RCONSSV),(3,P3VER+1),ALIGN=LEFT                               
         TM    RCONSENF,X'10'      STATION WORK IN PROGRESS?                    
         BO    PATAN30                                                          
         CLI   CNFNOWIP,C'Y'       IS ORDER CONFIRMED?                          
         BE    PATAN30             YES - DON'T DISPLAY 'WIP'                    
         MVC   P3SNT,=C'WIP  '   NO , DISPLAY SO                                
         B     PATAN50                                                          
*                                                                               
* STATION DATE AND TIME OF LAST SEND                                            
*                                                                               
PATAN30  DS    0H                                                               
         TM    VIEWS3,X'20'        TAKEOVERS ONLY                               
         BO    PATAN32                                                          
         GOTO1 VDATCON,DMCB,(2,RCONSSDT),(5,P3ACTV)                             
PATAN32  OC    RCONSSTI,RCONSSTI                                                
         BZ    PATAN50                                                          
         MVC   P3SNT(2),RCONSSTI                                                
         MVI   P3SNT+2,C':'                                                     
         MVC   P3SNT+3(2),RCONSSTI+2                                            
         B     PATAN50                                                          
                                                                                
PATAN40  MVI   P3VER,C'R'                                                       
         EDIT  (1,RCONSRV),(3,P3VER+1),ALIGN=LEFT                               
         TM    RCONSENF,X'20'      REP WORK IN PROGRESS?                        
         BO    PATAN45                                                          
         CLI   CNFNOWIP,C'Y'       IS ORDER CONFIRMED?                          
         BE    PATAN45             YES - DON'T DISPLAY 'WIP'                    
         MVC   P3SNT,=C'WIP  '     NO , DISPLAY SO                              
         B     PATAN50                                                          
*                                                                               
* REP DATE AND TIME OF LAST SEND                                                
*                                                                               
PATAN45  DS    0H                                                               
         TM    VIEWS3,X'20'        LISTD DISPLAY?                               
         BO    PATAN46                                                          
         GOTO1 VDATCON,DMCB,(2,RCONSRDT),(5,P3ACTV)                             
PATAN46  OC    RCONSRTI,RCONSRTI                                                
         BZ    PATAN50                                                          
         MVC   P3SNT(2),RCONSRTI                                                
         MVI   P3SNT+2,C':'                                                     
         MVC   P3SNT+3(2),RCONSRTI+2                                            
                                                                                
PATAN50  DS    0H                                                               
         DROP  R4                                                               
         LA    R4,RCONELEM                                                      
         MVI   CNFNOWIP,C'N'       SET CONFIRMED/NO WIP TO 'NO'                 
PATAN55  ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PATAN60                                                          
         AR    R4,R1                                                            
         CLI   0(R4),X'1F'         GET EXTENDED DESC ELEM                       
         BNE   PATAN55                                                          
         TM    6(R4),X'40'         IF CONFIRMED                                 
         BNO   PATANX                                                           
         MVC   P3VER,=C'CNF  '     MARK AS SUCH                                 
         MVI   CNFNOWIP,C'Y'       SET CONFIRMED/NO WIP TO 'YES'                
*                                                                               
PATAN60  EQU   *                                                                
         LA    R4,RCONELEM                                                      
                                                                                
                                                                                
PATANX   BAS   RE,PRINT            PRINT FIRST LINE                             
         MVC   MYP,MYP2            AND NEXT LINE                                
         BAS   RE,PRINT                                                         
         XC    MYP,MYP                                                          
         BAS   RE,PRINT            SPACE                                        
         B     READREC           GO GET NEXT REC                                
         EJECT                                                                  
*                                                                               
JOINDATE NTR1                                                                   
         CLC   STASAVE,RCONKSTA    NEW STATION ?                                
         BE    JOINX                                                            
         MVC   STASAVE,RCONKSTA    YES                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),STASAVE                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+25(4),AIOAREA        SET UP TO USE TEMPIO                   
         LA    R1,IOAREA                                                        
         ST    R1,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         GOTO1 GETREC                                                           
         MVC   STAJOIN,RSTASTRT    BINARY DATE                                  
*                                                                               
         MVC   AIOAREA,WORK+25     RESET AIO AREA                               
         LA    R1,REPIOTBL                                                      
         USING REPIOD,R1                                                        
         MVI   RIPSTAT,RIPRDHI     REPIO READ HI BEFORE SEQ                     
         DROP  R1                                                               
         XC    KEY,KEY                                                          
JOINX    XIT1                                                                   
         EJECT                                                                  
* AGENCY NAME FOR LISTD OPTION                                                  
* CHANGED TO MATCH ON AGY + OFFICE                                              
GETAGYNM NTR1                                                                   
         XC    WORK(6),WORK                                                     
         MVC   WORK(4),RCONKAGY    DO WE ALREADY HAVE NAME                      
         MVC   WORK+4(2),RCONKAOF                                               
         LA    R2,NMENTNUM         NUMBER OF ENTRIES                            
         LA    R1,AGYNMSV                                                       
AGNM05   CLI   0(R1),0             EMPTY SLOT ?                                 
         BE    AGNM13              YES                                          
         CLC   WORK(6),0(R1)       MATCH ?                                      
         BNE   AGNM07              NO                                           
         MVC   WORK(19),6(R1)      YES                                          
         B     AGNMX                                                            
                                                                                
AGNM07   LA    R1,25(R1)                                                        
         BCT   R2,AGNM05                                                        
         LA    R1,AGYNMSV          NO MATCH/PUT NEW REC IN 1ST SLOT             
         B     AGNM13                                                           
*                                                                               
                                                                                
AGNM13   XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),RCONKAGY                                               
         MVC   KEY+23(2),RCONKAOF                                               
         MVC   KEY+25(2),REPALPHA                                               
AGNM15   BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   AGNM20                                                           
         CLC   REPALPHA,KEY+25                                                  
         BE    AGNM25                                                           
         CLC   =C'ZZ',KEY+25                                                    
         BE    AGNM25                                                           
         MVC   KEY+25(2),=C'ZZ'                                                 
         B     AGNM15                                                           
AGNM20   MVC   WORK(10),=C'NO AGY REC'                                          
         B     AGNMX                                                            
*                                                                               
AGNM25   MVC   WORK+25(4),AIOAREA        SET UP TO USE TEMPIO                   
**       LA    R1,TEMPIO                                                        
         L     R1,=A(TEMPIO)                                                    
         A     R1,RELO2                                                         
         ST    R1,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         L     R1,AIOAREA                                                       
         USING RAGYREC,R1                                                       
         MVC   WORK(20),RAGYNAM1                                                
*                                                                               
* MOVE TABEL UP ONE ENTRY                                                       
         LA    RE,AGYNMSV                                                       
         LR    R1,RE               RE POINTS TO 1ST ENTRY                       
         LA    R1,25(R1)           R1 POINTS TO 2ND ENTRY                       
         LA    R2,25               EACH ENTRY LENGTH = 25                       
         MHI   R2,NMENTNUM         25 X HALF(NUMB OF ENTRIES)                   
         S     R2,=F'26'                                                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)       MOVE TABLE UP                                
         LA    R2,1(R2)                                                         
         AR    RE,R2               RE POINTS TO LAST ENTRY FIELD                
*                                                                               
***      L     R1,FULL             SAVE CODE,NAME                               
***      MVC   0(4,R1),RCONKAGY                                                 
***      MVC   5(20,R1),WORK                                                    
         MVC   0(6,RE),RCONKAGY    AGENCY/OFF                                   
         MVC   6(19,RE),WORK                                                    
*                                                                               
         MVC   AIOAREA,WORK+25     RESTORE IO AREA                              
         B     AGNMX                                                            
         DROP  R1                                                               
                                                                                
AGNMX    B     XIT                                                              
*                                                                               
                                                                                
* DO I HAVE NAME IN TABLE ?                                                     
* R1 POINTS TO REQUISITE TABLE SAVE AREA                                        
* WORK HAS CODE                                                                 
* RETURNS NAME IN WORK OR ADDR OF NEXT EMPTY SLOT IN FULL                       
CHKNM    NTR1                                                                   
         LA    R2,NMENTNUM         NUMBER OF ENTRIES                            
         STH   R2,HALF             STORE  IN HALF                               
         LR    RE,R1               SAVE START OF AREA                           
CHKNM05  OC    0(3,R1),0(R1)        EMPTY TABLE SLOT ?                          
         BNZ   CHKNM07                                                          
         ST    R1,FULL             YES-PASS BACK ADDR                           
         B     CHKNMNO                                                          
CHKNM07  CLC   0(5,R1),WORK        TABLE MATCH ?                                
         BNE   CHKNM20                                                          
         MVC   WORK(20),5(R1)      YES-PASS BACK NAME                           
         B     CHKNMX                                                           
CHKNM20  LA    R1,25(R1)                                                        
         BCT   R2,CHKNM05                                                       
* - NO MATCH - MOVE UP TABLE ONE ENTRY                                          
         LR    R1,RE               RE POINTS TO 1ST ENTRY                       
         LA    R1,25(R1)           R1 POINTS TO 2ND ENTRY                       
         LA    R2,25               EACH ENTRY LENGTH = 25                       
         MH    R2,HALF             25 X HALF(NUMB OF ENTRIES)                   
         S     R2,=F'26'                                                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)       MOVE TABLE UP                                
         LA    R2,1(R2)                                                         
         AR    RE,R2               RE POINTS TO LAST ENTRY FIELD                
         ST    RE,FULL             PASS BACK LAST ENTRY ADDR IN FULL            
CHKNMNO  LTR   RE,RE               SET CC CODE                                  
*                                                                               
CHKNMX   B     XIT                                                              
         EJECT                                                                  
         SPACE 2                                                                
* ADVERTISER NAME FOR LISTD OPTION                                              
GETADVNM NTR1                                                                   
         XC    WORK(5),WORK        DO WE ALREADY HAVE NAME ?                    
         MVC   WORK(4),RCONKADV                                                 
         LA    R1,ADVNMSV                                                       
         BAS   RE,CHKNM                                                         
         BE    GETADVX                                                          
                                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,8                                                            
         MVC   KEY+21(4),RCONKADV                                               
         MVC   KEY+25(2),REPALPHA                                               
GETADV2  BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETADV3                                                          
         CLC   REPALPHA,KEY+25                                                  
         BE    GETADV4                                                          
         CLC   =C'ZZ',KEY+25                                                    
         BE    GETADV4                                                          
         MVC   KEY+25(2),=C'ZZ'                                                 
         B     GETADV2                                                          
GETADV3  MVC   WORK(12),=C'ADV UNKNOWN '                                        
         B     GETADVX                                                          
*                                                                               
GETADV4  MVC   WORK+25(4),AIOAREA                                               
**       LA    R1,TEMPIO                                                        
         L     R1,=A(TEMPIO)                                                    
         A     R1,RELO2                                                         
         ST    R1,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         USING RADVREC,R1                                                       
         MVC   WORK(20),RADVNAME                                                
                                                                                
         L     R1,FULL             SAVE CODE/NAME IN TABLE                      
         MVC   0(4,R1),RCONKADV                                                 
         MVC   5(20,R1),WORK                                                    
                                                                                
         MVC   AIOAREA,WORK+25     RESET IO AREA                                
         B     GETADVX                                                          
         DROP  R1                                                               
GETADVX  B     XIT                                                              
*                                                                               
         EJECT                                                                  
* - PRODUCT NAME FOR LISTD OPTION   - R4 POINTS TO 01 ELEM                      
GETPRDNM NTR1                                                                   
         CLC   RCONPRD,=3X'40'     IF PROD=SPACES                               
         BNE   GTPRD00                                                          
GT05     ZIC   R1,1(R4)            05 ELEMENT HAS PROD NAME                     
         LTR   R1,R1                                                            
         BZ    GTNOPRD                                                          
         AR    R4,R1                                                            
         CLI   0(R4),X'05'                                                      
         BNE   GT05                                                             
         LR    R1,R4                                                            
         USING RCONEXEL,R1                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(L'RCONEXPR),RCONEXPR                                        
         B     XIT                                                              
         DROP  R1                                                               
*                                                                               
GTPRD00  XC    WORK(7),WORK        PRD CODE + ADV CODE                          
         MVC   WORK(3),RCONPRD     DO WE ALREADY HAVE NAME                      
         MVC   WORK+3(4),RCONKADV                                               
         LA    R2,NMENTNUM         NUMBER OF ENTRIES                            
         LA    R1,PRDNMSV                                                       
GTPRD05  CLI   0(R1),0             EMPTY SLOT ?                                 
         BE    GTPRD10             YES                                          
         CLC   WORK(7),0(R1)       MATCH ?                                      
         BNE   GTPRD07             NO                                           
         MVC   WORK(18),7(R1)      YES                                          
         B     GTPRDX                                                           
*                                                                               
GTPRD07  LA    R1,25(R1)                                                        
         BCT   R2,GTPRD05                                                       
         LA    R1,PRDNMSV          NO MATCH/PUT NEW REC IN 1ST SLOT             
         B     GTPRD10                                                          
*                                                                               
GTPRD10  XC    KEY,KEY                                                          
         MVI   KEY,X'09'                                                        
         MVC   KEY+18(4),RCONKADV                                               
         MVC   KEY+22(3),RCONPRD                                                
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GTNOPRD                                                          
         CLC   KEY+25(2),REPALPHA                                               
         BE    GTDISPRD                                                         
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    GTDISPRD                                                         
         MVC   KEY+25(2),=C'ZZ'                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GTNOPRD                                                          
         SPACE 1                                                                
GTDISPRD MVC   WORK+25(4),AIOAREA        SET TO USE TEMPIO                      
***      LA    R1,TEMPIO                                                        
         L     R1,=A(TEMPIO)                                                    
         A     R1,RELO2                                                         
         ST    R1,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         USING RPRDREC,R1                                                       
         MVC   WORK(20),RPRDNAME                                                
         DROP  R1                                                               
*                                                                               
* MOVE TABEL UP ONE ENTRY                                                       
         LA    RE,PRDNMSV                                                       
         LR    R1,RE               RE POINTS TO 1ST ENTRY                       
         LA    R1,25(R1)           R1 POINTS TO 2ND ENTRY                       
         LA    R2,25               EACH ENTRY LENGTH = 25                       
         MHI   R2,NMENTNUM         25 X HALF(NUMB OF ENTRIES)                   
         S     R2,=F'26'                                                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)       MOVE TABLE UP                                
         LA    R2,1(R2)                                                         
         AR    RE,R2               RE POINTS TO LAST ENTRY FIELD                
*                                                                               
         MVC   0(3,RE),RCONPRD                                                  
         MVC   3(4,RE),RCONKADV                                                 
         MVC   7(18,RE),WORK                                                    
*                                                                               
         MVC   AIOAREA,WORK+25          RESET IO AREA                           
*                                                                               
         B     GTPRDX                                                           
                                                                                
GTNOPRD  MVC   WORK(12),=CL12'NOT FOUND'                                        
         B     GTPRDX                                                           
                                                                                
GTPRDX   DS    0H                                                               
         B     XIT                                                              
*                                                                               
                                                                                
GETSLSNM NTR1                                                                   
         XC    WORK(5),WORK        DO WE ALREADY HAVE NAME ?                    
         MVC   WORK(3),RCONSAL                                                  
         LA    R1,SLSNMSV                                                       
         BAS   RE,CHKNM                                                         
         BE    XIT                                                              
                                                                                
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING RSALREC,R1                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,RCONKREP                                                
         MVC   RSALKSAL,RCONSAL                                                 
         DROP  R1                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GTNOSAL                                                          
         SPACE 1                                                                
GTDISSAL MVC   WORK+25(4),AIOAREA        SET TO USE TEMPIO                      
**       LA    R1,TEMPIO                                                        
         L     R1,=A(TEMPIO)                                                    
         A     R1,RELO2                                                         
         ST    R1,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         USING RSALREC,R1                                                       
         MVC   WORK(20),RSALNAME                                                
                                                                                
         L     R1,FULL             SAVE CODE,NAME TO TABLE                      
         MVC   0(3,R1),RCONSAL                                                  
         MVC   5(20,R1),WORK                                                    
         MVC   AIOAREA,WORK+25     RESET IO AREA                                
         B     GTSALX                                                           
         DROP  R1                                                               
                                                                                
GTNOSAL  MVC   WORK(20),=CL20'NOT FOUND'                                        
         B     GTSALX                                                           
                                                                                
GTSALX   EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* WORK HAS 3 BYTE DEMO CODE                                                     
GETDEMNM NTR1                                                                   
         XC    WORK+25(5),WORK+25                                               
         MVC   WORK+25(3),WORK     SAVE DEMO CODE                               
         XC    WORK+3(2),WORK+3    DO WE ALREADY HAVE NAME                      
         LA    R1,DEMNMSV                                                       
         BAS   RE,CHKNM                                                         
         BE    XIT                                                              
                                                                                
         XC    WORK+10(15),WORK+10       CLEAR OUTPUT AREA                      
                                                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'R'                                                    
         CLI   RCONKSTA+4,C'A'                                                  
         BE    *+16                                                             
         CLI   RCONKSTA+4,C'F'                                                  
         BE    *+8                                                              
         MVI   DBSELMED,C'T'                                                    
                                                                                
         L     RF,VCALLOV                GET DEMOCON                            
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000AE0'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(0,WORK),(6,WORK+10),(0,DBLOCK),0                      
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(11),WORK+10                                                 
                                                                                
         L     R1,FULL             SAVE CODE,NAME                               
         MVC   0(5,R1),WORK+25                                                  
         MVC   5(11,R1),WORK                                                    
                                                                                
                                                                                
GETDEMX  B      XIT                                                             
                                                                                
* RESET R2 TO LINE2                                                             
         USING LINE2,R2                                                         
                                                                                
         EJECT                                                                  
LSTDIV   OC    L2DIV,L2DIV                                                      
         BZ    RESET                                                            
         CLC   L2DIV,KEY+25                                                     
         BE    LSTDIV6                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,5                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),L2DIV                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    LSTDIV4                                                          
         MVC   L2NAME,=CL20'TEAM REC NOT FOUND'                                 
         B     FOUTLINE                                                         
*                                                                               
LSTDIV4  BAS   RE,GETREC                                                        
LSTDIV6  MVC   L2NAME,RTEMDVNM                                                  
         B     FOUTLINE                                                         
         SPACE 2                                                                
LSTSAL   OC    L2SAL,L2SAL                                                      
         BZ    RESET                                                            
         CLC   L2SAL,KEY+24                                                     
         BE    LSTSAL6                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+22(2),REPALPHA                                               
         MVC   KEY+24(3),L2SAL                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    LSTSAL4                                                          
         MVC   L2NAME,=CL20'SALESPERSON UNKNOWN'                                
         B     FOUTLINE                                                         
*                                                                               
LSTSAL4  BAS   RE,GETREC                                                        
LSTSAL6  MVC   L2NAME,RSALNAME                                                  
         B     FOUTLINE                                                         
         EJECT                                                                  
LSTAGY   OC    L2AGY,L2AGY                                                      
         BZ    RESET                                                            
         CLC   L2AGY,KEY+19                                                     
         BNE   LSTAGY2                                                          
         CLC   L2AOF,KEY+23                                                     
         BE    LSTAGY6                                                          
*                                                                               
LSTAGY2  XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),L2AGY                                                  
         MVC   KEY+23(2),L2AOF                                                  
         MVC   KEY+25(2),REPALPHA                                               
LSTAGY3  BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   LSTAGY3N                                                         
         CLC   REPALPHA,KEY+25                                                  
         BE    LSTAGY4                                                          
         CLC   =C'ZZ',KEY+25                                                    
         BE    LSTAGY4                                                          
         MVC   KEY+25(2),=C'ZZ'                                                 
         B     LSTAGY3                                                          
LSTAGY3N MVC   L2NAME,=CL20'AGENCY REC NOT FOUND'                               
         B     FOUTLINE                                                         
*                                                                               
LSTAGY4  BAS   RE,GETREC                                                        
LSTAGY6  MVC   L2NAME,RAGYNAM1                                                  
         B     FOUTLINE                                                         
         SPACE 2                                                                
LSTADV   OC    L2ADV,L2ADV                                                      
         BZ    RESET                                                            
         CLC   L2ADV,KEY+21                                                     
         BE    LSTADV6                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,8                                                            
         MVC   KEY+21(4),L2ADV                                                  
         MVC   KEY+25(2),REPALPHA                                               
LSTADV2  BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   LSTADV3                                                          
         CLC   REPALPHA,KEY+25                                                  
         BE    LSTADV4                                                          
         CLC   =C'ZZ',KEY+25                                                    
         BE    LSTADV4                                                          
         MVC   KEY+25(2),=C'ZZ'                                                 
         B     LSTADV2                                                          
LSTADV3  MVC   L2NAME,=CL20'ADVERTISER UNKNOWN'                                 
         B     FOUTLINE                                                         
*                                                                               
LSTADV4  BAS   RE,GETREC                                                        
LSTADV6  MVC   L2NAME,RADVNAME                                                  
         B     FOUTLINE                                                         
         EJECT                                                                  
LSTSTA   OC    L2STA,L2STA                                                      
         BZ    RESET                                                            
         CLC   L2STA,KEY+22                                                     
         BNE   LSTSTA1                                                          
         CLC   L2MED(1),KEY+26                                                  
         BE    LSTSTA6                                                          
         CLI   KEY+26,C' '                                                      
         BNE   LSTSTA1                                                          
         CLI   L2MED,C'T'                                                       
         BE    LSTSTA6                                                          
*                                                                               
LSTSTA1  XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(4),L2STA                                                  
         MVI   KEY+26,C' '                                                      
         CLC   L2MED,=C'TV'                                                     
         BE    LSTSTA2                                                          
         MVI   KEY+26,C'A'                                                      
         CLC   L2MED,=C'AM'                                                     
         BE    LSTSTA2                                                          
         MVI   KEY+26,C'C'         COMBINED STATION                             
         CLC   L2MED,=C'CM'                                                     
         BE    LSTSTA2                                                          
         MVI   KEY+26,C'F'                                                      
*                                                                               
LSTSTA2  BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    LSTSTA4                                                          
         MVC   L2NAME,=CL20'STATION NOT FOUND'                                  
         B     FOUTLINE                                                         
*                                                                               
LSTSTA4  BAS   RE,GETREC                                                        
LSTSTA6  MVC   L2NAME,RSTAMKT                                                   
         B     FOUTLINE                                                         
         EJECT                                                                  
LSTOFF   OC    L2OFF,L2OFF                                                      
         BZ    RESET                                                            
         CLC   L2OFF,KEY+25                                                     
         BE    LSTOFF6                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,4                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),L2OFF                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    LSTOFF4                                                          
         MVC   L2NAME,=CL20'OFFICE NOT FOUND'                                   
         B     FOUTLINE                                                         
*                                                                               
LSTOFF4  BAS   RE,GETREC                                                        
LSTOFF6  MVC   L2NAME,ROFFNAME                                                  
         B     FOUTLINE                                                         
         SPACE 2                                                                
LSTCTG   OC    L2CTG,L2CTG                                                      
         BZ    RESET                                                            
         CLC   L2CTG,KEY+25                                                     
         BE    LSTCTG6                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0F'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),L2CTG                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    LSTCTG4                                                          
         MVC   L2NAME,=CL19'CATEGORY NOT FOUND'                                 
         B     FOUTLINE                                                         
*                                                                               
LSTCTG4  BAS   RE,GETREC                                                        
LSTCTG6  MVC   L2NAME,RCTGNAME                                                  
         B     FOUTLINE                                                         
         EJECT                                                                  
LSTPRD   LA    R1,RISBOTMH         IF LAST SCREEN LINE                          
         CR    R1,R2                                                            
         BE    FOUTLINE            GET OUT (RESERVED FOR PF INFO)               
                                                                                
         OC    L2NAME,L2NAME                                                    
         BZ    RESET                                                            
         CLC   L2NAME(2),=C'C='    IF NOT A CODE ALREADY HAVE PRODUCT           
         BNE   FOUTLINE                                                         
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'09'                                                        
         MVC   KEY+18(4),L2ADV                                                  
         MVC   KEY+22(3),L2NAME+2                                               
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   NOPRD                                                            
         CLC   KEY+25(2),REPALPHA                                               
         BE    DISPRD                                                           
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    DISPRD                                                           
         MVC   KEY+25(2),=C'ZZ'                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NOPRD                                                            
         SPACE 1                                                                
DISPRD   BAS   RE,GETREC                                                        
         MVC   L2NAME+6(13),RPRDNAME                                            
         B     FOUTLINE                                                         
         SPACE 1                                                                
NOPRD    MVC   L2NAME+6(14),=CL14'CODE NOT FOUND'                               
         B     FOUTLINE                                                         
         SPACE 2                                                                
LSTGRP   OC    L2GRP,L2GRP                                                      
         BZ    RESET                                                            
         CLC   L2GRP,KEY+25                                                     
         BE    LSTGRP6                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'07'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),L2GRP                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    LSTGRP4                                                          
         MVC   L2NAME,=CL19'GROUP NOT FOUND'                                    
         B     FOUTLINE                                                         
*                                                                               
LSTGRP4  BAS   RE,GETREC                                                        
LSTGRP6  MVC   L2NAME,RGRPNAME                                                  
         B     FOUTLINE                                                         
         EJECT                                                                  
LSTDLR   LA    R4,RCONELEM                                                      
         XC    FULL,FULL                                                        
         MVC   BYTE2,ESTBUCKT      ORDERED DOLLARS                              
         CLI   RISALTD,C'I'        INVOICE DOLLARS ?                            
         BNE   LSTDLR2                                                          
         MVC   BYTE2,INVBUCKT      YES                                          
LSTDLR2  SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         AR    R4,R6                                                            
         CLC   BYTE2,0(R4)                                                      
         BE    LSTDLR4                                                          
         CLI   0(R4),0                                                          
         BE    LSTDLR6                                                          
         B     LSTDLR2                                                          
*                                                                               
         USING RCONBKEL,R4                                                      
LSTDLR4  SR    R1,R1                                                            
         CLC   MOSTRT,RCONBKYR                                                  
         BH    LSTDLR2                                                          
         CLC   MOSEND,RCONBKYR                                                  
         BL    LSTDLR2                                                          
         MVC   DUB(4),RCONBKAM                                                  
         L     R1,DUB                                                           
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
         B     LSTDLR2                                                          
*                                                                               
LSTDLR6  L     R0,FULL                                                          
         CLI   RISTATUS,1          IF=1 IN MIDDLE OF TOT READ(LISTD/$)          
         BER   R5                  SO JUST RETURN $ AMT                         
                                                                                
         CVD   R0,DUB                                                           
         ZAP   WORK+20(8),DUB                                                   
         SRP   WORK+20(8),64-2,5                                                
         EDIT  (P8,WORK+20),(13,L2NAME),COMMAS=YES,FLOAT=-,ZERO=NOBLANK         
         FOUT  (R2)                                                             
                                                                                
         CLI   VIEWS,0             IS IT LIST$/LISTD/BUY$ ?                     
         BE    LSTDLRX             NO                                           
         TM    VIEWS,X'80'         DO BOTTOM PAGE TOTALS ?                      
         BO    LSTDLRX             NO                                           
                                                                                
         CVD   R0,DUB              YES                                          
         AP    PGTOT,DUB           ADD TO PAGE TOTAL                            
                                                                                
         CLI   RISTATUS,0          ,,IF FIRST TIME IN                           
         BNE   LSTDLRX                                                          
         CVD   R0,DUB              ,,ADD TO OVERALL DOLLAR TOTAL                
         AP    DLRTOTAL,DUB                                                     
****     BAS   RE,CALCSHR          GET SHR $ AMT                                
         GOTO1 =A(CALCSHR),RR=Y                                                 
         L     R1,CONTOT           ADD TO CONTRACT NUMBER TOTALS                
         LA    R1,1(R1)                                                         
         ST    R1,CONTOT                                                        
LSTDLRX  B     NEXTLINE                                                         
         EJECT                                                                  
LSTUNC   LA    R4,RCONELEM                                                      
         USING RCONELEM,R4                                                      
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    LSTUNCNG                                                         
         DROP  R4                                                               
         SPACE 1                                                                
         SR    R6,R6                                                            
         TM    RCONMODR,X'10'      BUY ADDED TO RECORD?                         
         BO    LSTUNC3             YES - CON HAS BUY ACTIVITY                   
*                                                                               
*   IT IS POSSIBLE FOR AN ORDER TO HAVE BUY ACTIVITY WITH $0 WHICH              
*        DOESN'T GENERATE AN X'03' BUCKET.  THIS TEST WILL ENSURE               
*        THAT THESE ORDERS ARE INCLUDED IN THE LIST.                            
*                                                                               
LSTUNC1  IC    R6,1(R4)                                                         
         AR    R4,R6                                                            
         CLI   0(R4),X'03'         ONLY WANT UNCONFIRMED RECORDS                
         BE    LSTUNC3             WITH BUY ACTIVITY                            
         CLI   0(R4),0                                                          
         BNE   LSTUNC1                                                          
         B     LSTUNCNG                                                         
         SPACE 1                                                                
LSTUNC3  IC    R6,1(R4)                                                         
         AR    R4,R6                                                            
         CLI   0(R4),X'1F'    FIND EXTENDED DESCRIPTION ELEMENT                 
         BE    LSTUNC6                                                          
         CLI   0(R4),0             END OF RECORD?                               
         BNE   LSTUNC3                                                          
         DC    H'0'                SHOULD BE X'1F' IF ACE/GRAPHNET              
         SPACE 1                                                                
LSTUNC6  TM    6(R4),X'80'         X'80' = UNCONFIRMED                          
         BZ    LSTUNCNG                                                         
         SPACE 1                                                                
*                                                                               
*   TEST -                                                                      
**       CLC   =X'03113404',RCONKCON                                            
**       BNE   *+6                                                              
**       DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
         SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         AR    R4,R6               GET SEND (X'20') ELEMENT                     
         CLI   0(R4),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                SHOULD BE THERE                              
         USING RCONSEND,R4                                                      
                                                                                
         LA    R6,L2NAME                                                        
                                                                                
         CLC   RCONSRV,RCONSSV     REP OR STATION VERSION HIGHER?               
         BH    LSTUNC10                                                         
         MVI   0(R6),C'S'                                                       
         EDIT  (1,RCONSSV),(3,1(R6)),ALIGN=LEFT                                 
         TM    RCONSENF,X'10'      STATION WORK IN PROGRESS?                    
         BO    LSTUNC7                                                          
         CLI   CNFNOWIP,C'Y'       IS ORDER CONFIRMED?                          
         BE    LSTUNC7             YES - DON'T DISPLAY 'WIP'                    
         MVC   5(3,R6),=C'WIP'     NO , DISPLAY SO                              
         B     LSTUNC20                                                         
*                                                                               
* STATION DATE AND TIME OF LAST SEND                                            
*                                                                               
LSTUNC7  DS    0H                                                               
         GOTO1 VDATCON,DMCB,(2,RCONSSDT),(5,5(R6))                              
         OC    RCONSSTI,RCONSSTI                                                
         BZ    LSTUNC20                                                         
         MVC   14(2,R6),RCONSSTI                                                
         MVC   17(2,R6),RCONSSTI+2                                              
         MVI   16(R6),C':'                                                      
         B     LSTUNC20                                                         
                                                                                
LSTUNC10 MVI   0(R6),C'R'                                                       
         EDIT  (1,RCONSRV),(3,1(R6)),ALIGN=LEFT                                 
         TM    RCONSENF,X'20'      REP WORK IN PROGRESS?                        
         BO    LSTUNC15                                                         
         CLI   CNFNOWIP,C'Y'       IS ORDER CONFIRMED?                          
         BE    LSTUNC15            YES - DON'T DISPLAY 'WIP'                    
         MVC   5(3,R6),=C'WIP'     NO , DISPLAY SO                              
         B     LSTUNC20                                                         
*                                                                               
* REP DATE AND TIME OF LAST SEND                                                
*                                                                               
LSTUNC15 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(2,RCONSRDT),(5,5(R6))                              
         OC    RCONSRTI,RCONSRTI                                                
         BZ    LSTUNC20                                                         
         MVC   14(2,R6),RCONSRTI                                                
         MVC   17(2,R6),RCONSRTI+2                                              
         MVI   16(R6),C':'                                                      
                                                                                
LSTUNC20 DS    0H                                                               
         FOUT  (R2)                                                             
         B     NEXTLINE                                                         
LSTUNCNG DS    0H                                                               
         MVI   L2OFF,X'40'         CLEAR NOT UNCONFIRMED FROM LINE              
         MVC   L2OFF+1(LLINE2-1),L2OFF                                          
         B     READREC                                                          
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        LSTTRAF --- PLACE TRAFFIC INFORMATION IN DETAIL MID-LINE               
*                                                                               
LSTTRAF  EQU   *                                                                
         MVC   L2CTG,=C' '         CLEAR THE TRAFFIC ORDER LINE AREA            
         MVC   L2CTG+1(22),L2CTG                                                
*                                                                               
         LA    R5,L2CTG                                                         
         LA    R8,RCONREC                                                       
         MVI   ELCODE,X'1F'        GET THE EXTENDED DESCRIP ELEMENT             
         BAS   RE,GETEL                                                         
         BE    LTRAF10                                                          
         MVC   L2CTG(23),=C'NO TRAFFIC INSTRUCTIONS'                            
         B     LTRAF90                                                          
LTRAF10  EQU   *                                                                
         LA    R8,8(R8)            POINT TO TRAFFIC NUMBER                      
         LR    R1,R8               TELL STUFF TO PUT A '/'                      
         BAS   RE,LSTUF            LOAD TRAFFIC NUMBER                          
         LA    R8,RCONREC                                                       
         MVI   ELCODE,X'9F'        GET AGY AND ADV CODES ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   LTRAF90                                                          
         LA    R8,2(R8)            POINT TO ADV #/AGY #                         
         LR    R1,R8               TELL STUFF TO PUT A '/'                      
         BAS   RE,LSTUF            LOAD ADVERTISER #                            
         SR    R1,R1               TELL STUFF TO NOT PUT A '/'                  
         BAS   RE,LSTUF            LOAD AGENCY #                                
*                                                                               
*        END OF TRAFFIC DATA LOAD                                               
*                                                                               
LTRAF90  EQU   *                                                                
         FOUT  (R2)                                                             
*                                                                               
         B     NEXTLINE                                                         
*                                                                               
*        TRAFFIC DATA 'STUFFER' - PUT AS MUCH DATA AS POSSIBLE INTO             
*                                 THE 23 BYTE FIELD                             
*                                                                               
*        R5  =  OUPUT LINE POINTER                                              
*        R8  =  DATA POINTER                                                    
*                                                                               
LSTUF    EQU   *                                                                
         LA    R0,L2CTG+23         WE CAN'T GO PAST HERE                        
         LA    RF,10               EACH FIELD IS 10 MAX                         
LSTUF10  EQU   *                                                                
         CR    R0,R5               ARE WE AT END?                               
         BE    LSTUF90             YES, LEAVE                                   
         CLI   0(R8),0             CHECK FOR A NON-ZERO, NON-SPACE              
         BE    LSTUF20              VALUE                                       
         CLI   0(R8),C' '                                                       
         BE    LSTUF20                                                          
         MVC   0(1,R5),0(R8)       NO, MOVE 1 CHARACTER                         
         LA    R5,1(R5)                                                         
LSTUF20  EQU   *                                                                
         LA    R8,1(R8)                                                         
         BCT   RF,LSTUF10          LOOP                                         
LSTUF30  EQU   *                                                                
         CR    R0,R5               STILL MORE ROOM?                             
         BE    LSTUF90             NO                                           
         LTR   R1,R1               CHECK '/' FLAG                               
         BZ    LSTUF90             ZERO IS DON'T DO '/'                         
         MVC   0(1,R5),=C'/'       YES, PUT IN A FIELD SEPERATOR                
         LA    R5,1(R5)                                                         
LSTUF90  EQU   *                                                                
         BR    RE                  RETURN                                       
         EJECT                                                                  
FOUTLINE FOUT  (R2)                                                             
         SR    RE,RE               NEXT LINE                                    
         IC    RE,0(R2)                                                         
         LA    R2,0(RE,R2)                                                      
         CLI   0(R2),0                                                          
         BE    SETNEXT2                                                         
* LAST SCREEN LINE SAVED FOR PF ETC DATA DISPLAY                                
         LA    R1,RISBOTMH         IF NEXT LINE IS LAST                         
         CR    R1,R2                                                            
         BE    SETNEXT2            GET OUT NOW                                  
         B     FOUTER2                                                          
         SPACE 2                                                                
SETNEXT  BAS   RE,FOUTER                                                        
SETNEXT2 MVC   LISTSAVE,LISTBYTE                                                
         MVC   LINKSAVE,LINK                                                    
         MVI   NEXTBYTE,1                                                       
         LA    R1,REPIOTBL                                                      
         USING REPIOD,R1                                                        
         MVI   RIPSTAT,RIPRDHIO    TELL REPIO TO RETURN SAME REC                
         BAS   R5,FOUTOTS          PXZ                                          
         XC    RISMESS,RISMESS                                                  
         MVC   RISMESS(L'MSG1),MSG1                                             
         LA    R2,RISMESSH                                                      
         FOUT  (R2)                                                             
         LA    R2,RISSTAH          POSITION CURSOR                              
         USING TWAD,R4                                                          
         LR    R4,RA                                                            
         CLI   TWAOFFC,C'*'        IF DDS TERMINAL                              
         BNE   SETNXTX                                                          
         B     SETNXTX             SKIP FOR NOW                                 
         DROP  R4                                                               
         LA    R2,RISDATEH         DISPLAY KEY IO COUNT                         
         LA    R1,REPIOTBL                                                      
         USING REPIOD,R1                                                        
         EDIT  (B4,RIPFULL),(6,18(R2))                                          
         FOUT  (R2)                                                             
         DROP  R1                                                               
SETNXTX  B     EXIT                                                             
         SPACE 2                                                                
LISTDONE CLI   PRNT,1              ARE WE PRINTING REPORT                       
         BNE   LSTD10                                                           
         TM    VIEWS,X'80'         ARE WE PRINTING TOTALS ?                     
         BO    *+8                 NO                                           
         BAS   RE,TOTPRINT         YES-TOTALS                                   
         BAS   RE,LASTP            LAST TIME ROUTINES                           
         B     EXIT                                                             
                                                                                
LSTD10   DS    0H                                                               
***      MVI   NEXTBYTE,0                                                       
         MVI   NEXTBYTE,1                                                       
         XC    RISMESS,RISMESS                                                  
         MVC   RISMESS(L'MSG2),MSG2                                             
         FOUT  RISMESSH                                                         
         SPACE 2                                                                
CLEAR    CLI   0(R2),0                                                          
         BE    TSTPRD                                                           
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,ORFLD                                                         
         BZ    CLEAR1                                                           
*                                                                               
         EX    RE,XCFLD                                                         
         FOUT  (R2)                                                             
CLEAR1   LA    R2,9(RE,R2)                                                      
         B     CLEAR                                                            
         SPACE 1                                                                
TSTPRD   BAS   RE,FOUTER                                                        
                                                                                
         CLI   RISTATUS,0        ,IF FIRST TIME IN (SCREEN NOT FULL)            
         BNE   TSTPRD5                                                          
***      BAS   R5,FOUTOTS        ,PUT OUT TOTAL                                 
         MVI   RISTATUS,2        ,SET END OF TOTAL READ                         
         B     RESET                                                            
TSTPRD5  CLI   RISTATUS,1        ,,IF FIRST TIME IN (SCREEN FULL)               
         BNE   TSTPRD10                                                         
***      BAS   R5,FOUTOTS        ,,PUT OUT TOTAL                                
         MVI   RISTATUS,2        ,,SET END OF TOTAL READ                        
         LA    R1,REPIOTBL                                                      
         USING REPIOD,R1                                                        
         MVC   RIPKEY,SAVEKEY      PASS SAVED KEY TO REPIO                      
                                                                                
* - FOR COMBO STATIONS                - NEED TO RESET RIPSTA                    
         CLI   RIPSTA,X'40'                                                     
         BNH   *+10                                                             
         MVC   RIPSTA,RIPKEY+3                                                  
         OC    ACOMBSV,ACOMBSV         - AND PASS START OF COMBO LIST           
         BZ    *+10                                                             
         MVC   ACMBOSTA,ACOMBSV                                                 
*                                                                               
         DROP  R1                                                               
         B     SETNEXT2          ,,GOTO NEXT SCREEN PROCESSING                  
TSTPRD10 CLI   RISTATUS,2         .NOT FIRST TIME IN                            
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
***      BAS   R5,FOUTOTS         .PUT OUT TOTS                                 
         B     RESET                                                            
*                                                                               
RESET    BAS   R5,FOUTOTS                                                       
*                                                                               
SKIPCNT  LA    R2,RISSTAH                                                       
*                                  SET ANY LIST FIELD TO INVALID                
**       NI    RISSTAH+4,X'DF'                                                  
**       NI    RISOFFH+4,X'DF'                                                  
**       NI    RISAGYH+4,X'DF'                                                  
**       NI    RISADVH+4,X'DF'                                                  
**       NI    RISSLSH+4,X'DF'                                                  
**       NI    RISCTGH+4,X'DF'                                                  
**       NI    RISFILTH+4,X'DF'                                                 
*                                                                               
         MVI   NEXTBYTE,X'FF'      TELLS 00 THAT WE HAVE FINISHED               
         MVC   LISTSAVE,LISTBYTE   IF < ONE SCREEN LISTSAVE NOT SAVED           
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
ORFLD    OC    8(0,R2),8(R2)                                                    
*                                                                               
XCFLD    XC    8(0,R2),8(R2)                                                    
*                                                                               
         SPACE 3                                                                
         GETEL R8,34,ELCODE                                                     
         EJECT                                                                  
MSG1     DC    C'MORE CONTRACTS AVAILABLE-HIT ''ENTER'' TO CONTINUE'            
MSG2     DC    C'ACTION COMPLETED - ENTER NEXT REQUEST'                         
         SPACE 1                                                                
ATITLE   DC    F'0'                                                             
         DC    A(TLDIV)                                                         
         DC    A(TLSAL)                                                         
         DC    A(TLAGY)                                                         
         DC    A(TLADV)                                                         
         DC    A(TLSTA)                                                         
         DC    A(TLOFF)                                                         
         DC    A(TLPRD)                                                         
         DC    A(TLCTG)                                                         
         DC    A(TLDLR)                                                         
         DC    A(TLGRP)                                                         
         DC    A(TLUNC)                                                         
         DC    A(TLTRAF)                                                        
         SPACE 2                                                                
TLDIV    DC    CL22' CT DIVISION/TEAM NAME'                                     
TLSAL    DC    CL22' CT    SALES PERSON'                                        
TLAGY    DC    CL22' CT   AGENCY NAME'                                          
TLADV    DC    CL22' CT ADVERTISER NAME'                                        
TLSTA    DC    CL22' CT   STATION MARKET'                                       
TLOFF    DC    CL22' CT    OFFICE NAME'                                         
TLPRD    DC    CL22' CT    PRODUCT NAME'                                        
TLCTG    DC    CL22' CT CATEGORY NAME'                                          
TLDLR    DC    CL22' CT  TOTAL DOLLARS'                                         
TLGRP    DC    CL22' CT GROUP/SUBGRP NAME'                                      
TLUNC    DC    CL22' CT VER# SNT DATE/TIME'                                     
TLTRAF   DC    CL22'   TRAF #/ADV #/AGY #'                                      
INDENT   EQU   30                                                               
*                                                                               
*                123456789º123456789º123456789º                                 
PFTITL   DC    C'PF1=PRNT 2=DSM 3=CHA 5=MGL'                                    
PFTIT1   DC    C'PF2=DSM 3=CHA 4=BCK 5=MGL'                                     
         EJECT                                                                  
* TOTALS AT END OF PAGE WHEN PRINTING REPORT                                    
TOTPRINT NTR1                                                                   
***      CLI   LNE,74              ENSURE THAT 4 TOTAL LINES                    
         CLI   LNE,52              ENSURE THAT 4 TOTAL LINES                    
         BH    *+8                                                              
         MVI   LNE,0               PRINT ON SAME PAGE                           
         XC    MYP,MYP                                                          
                                                                                
         MVC   MYP+8(15),=C'*REPORT TOTALS*'                                    
         BAS   RE,PRINT                                                         
                                                                                
         OC    SHRPCT,SHRPCT       PRINT SHR %                                  
         BNZ   TOUT15                                                           
         CP    SHRTOT,=PL8'0'                                                   
         BE    TOUT15                                                           
         ZAP   WORK(16),SHRTOT     SHR $AMT INTO WORK                           
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),ALLSHR                                                  
         ZAP   DUB,WORK(8)                                                      
         CVB   R1,DUB                                                           
         ST    R1,SHRPCT                                                        
                                                                                
TOUT15   MVC   MYP+8(6),=C'SHARE='                                              
         LA    R2,MYP+19                                                        
         EDIT  (B4,SHRPCT),(6,0(R2)),2,TRAIL=C'%',ALIGN=LEFT                    
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP+8(9),=C'CONTRACT='                                           
         EDIT  (B4,CONTOT),(4,0(R2)),ALIGN=LEFT                                 
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP+8(8),=C'DOLLARS='                                            
         EDIT  (P8,DLRTOTAL),(15,0(R2)),2,FLOAT=-,ALIGN=LEFT                    
         BAS   RE,PRINT                                                         
                                                                                
TOTPX    B     XIT                                                              
         EJECT                                                                  
* INITIALIZE PRINT QUEUE                                                        
INITP    NTR1                                                                   
         MVI   MAXLNES,78          MAX LINES                                    
         MVI   PG,1                PG COUNT                                     
* SET 1ST TIME FOR PRTQUEUE                                                     
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         MVI   MYP-1,0                                                          
         XC    MYP,MYP                                                          
         MVC   MYP(3),=C'RIS'      ID                                           
         MVC   MYP+3(4),=C'LSTD'                                                
         MVC   MYP+11(3),=C'RIS'   SET SUB-KEY                                  
         MVI   MYP+24,C'Z'         CLASS 'Z'                                    
*                                                                               
         LA    R3,MYP-1                                                         
         USING PQPLD,R3            PRINT QUEUE PRINT LNE                        
*                                                                               
*                                                                               
INIT30   EQU   *                                                                
         MVI   QLEXTRA,X'FF'       INDICATE NEW STYLE LIST                      
         MVC   QLSRCID,PLUSER                                                   
         MVC   QLSUBID,PLSUBID                                                  
         CLI   QLCLASS,0                                                        
         BNE   *+10                                                             
         MVC   QLCLASS,PLCLASS                                                  
         CLI   QLSTAT,0                                                         
         BNE   *+10                                                             
         MVC   QLSTAT,PLSTAT                                                    
         CLI   QLLPP,0                                                          
         BNE   *+10                                                             
         MVC   QLLPP,PLLPP                                                      
         MVC   QLDESC,PLDESC                                                    
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'       KEEP PRINTED REPORT 2 HOURS                  
*                                                                               
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                 
         TM    DMCB+8,X'FF'                                                     
         BZ    INITPX                                                           
         DC    H'0',C'$PQFULL'                                                  
*                                                                               
INITPX   MVC   HALF2,QLREPRNO      SAVE REPORT NUMBER                           
         B     XIT                                                              
SAVIT    DS    H                                                                
         DROP  R3                                                               
         EJECT                                                                  
* - HEADLINES WHEN PRINTING REPORT                                              
DOHEADS  NTR1                                                                   
         MVI   LNE,0                 LNE CTR                                    
         XC    MYP,MYP                                                          
         MVI   MYP-1,X'89'           SKIP TO NEW PG                             
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MYP(7),=C'STATION'                                               
         MVC   MYP+8(7),RISSTA                                                  
         MVC   MYP+16(12),RISSTAN                                               
                                                                                
         MVC   MYP+35(6),=C'OFFICE'                                             
         MVC   MYP+44(4),RISOFF                                                 
                                                                                
         MVC   MYP+70(6),=C'AGENCY'                                             
         MVC   MYP+78(7),RISAGY                                                 
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP(7),=C'ADVRTSR'                                               
         MVC   MYP+8(4),RISADV                                                  
                                                                                
         MVC   MYP+35(7),=C'SPERSON'                                            
         MVC   MYP+44(7),RISSLS                                                 
                                                                                
         MVC   MYP+70(7),=C'CATEGRY'                                            
         MVC   MYP+78(4),RISCTG                                                 
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP(6),=C'DEMO  '                                                
         MVC   MYP+8(11),RISTAR                                                 
                                                                                
         MVC   MYP+35(8),=C'CREATION'                                           
         MVC   MYP+44(8),RISCRT                                                 
                                                                                
         MVC   MYP+70(10),=C'CON STATUS'                                        
         MVC   MYP+81(1),RISCST                                                 
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP(6),=C'MONTHS'                                                
         MVC   MYP+8(13),RISMNTS                                                
                                                                                
         MVC   MYP+35(7),=C'OPTIONS'                                            
         MVC   MYP+43(17),RISDATE                                               
                                                                                
         MVC   MYP+70(8),=C'CON TYPE'                                           
         MVC   MYP+79(2),RISFILT                                                
         GOTO1 VDATCON,DMCB,(5,0),(5,MYP+100)                                   
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP+70(5),=C'BLG $'                                              
         MVC   MYP+75(1),RISALTD                                                
                                                                                
         MVC   MYP+100(4),=C'PAGE'                                              
         EDIT  (B1,PG),(3,MYP+105),ALIGN=LEFT                                   
                                                                                
         MVI   MYP-1,SPACE3          SPACE 3 LINES AFTER PRINT                  
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP+40(19),=C'    DETAIL LISTING '                               
         BAS   RE,PRINT                                                         
         MVC   MYP+40(19),=C'    ______ _______ '                               
         MVI   MYP-1,SPACE3          SPACE 3 LINES AFTER PRINT                  
         BAS   RE,PRINT                                                         
                                                                                
         MVC   MYP(L'MYPLHEAD),MYPLHEAD                                         
         TM    VIEWS3,X'20'        TAKEOVER ?                                   
         BNO   *+10                                                             
         MVC   MYP(L'MYPLHD2),MYPLHD2                                           
         BAS   RE,PRINT                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
                                                                                
MYPLHEAD DC    C'      STA/ADV  AGENCY/ADVERTISER      PRODUCT/SALES   X        
                      FLIGHT    CONT/DEMO     DOLLARS      CRD/ACTV   VX        
               ER/SNT'                                                          
MYPLHD2  DC    C'      STA/ADV  AGENCY/ADVERTISER      PRODUCT/SALES   X        
                      FLIGHT    NEW/OLD #     DOLLARS      CRD/TKO    VX        
               ER/SNT'                                                          
                                                                                
         EJECT                                                                  
*                                                                               
*********************************************************************           
* PRINT ROUTINE                                                                 
*********************************************************************           
PRINT    NTR1                                                                   
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                 
         TM    DMCB+8,X'FF'        ERROR?                                       
         BZ    PRT10                                                            
         DC    H'0',C'$PQFULL'     PRINT ERROR                                  
PRT10    XC    MYP,MYP                                                          
         ZIC   RE,LNE                                                           
         LA    RE,1(RE)            BUMP LNE COUNT                               
         CLI   MYP-1,SPACE2        2 LINES                                      
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         B     PRTXIT                                                           
         CLI   MYP-1,SPACE3        3 LINES                                      
         BNE   PRTXIT                                                           
         LA    RE,2(RE)                                                         
*                                                                               
PRTXIT   STC   RE,LNE                                                           
         MVI   MYP-1,SPACE1        DEFAULT TO 1 ALWAYS                          
**       C     RE,=F'74'           ENSURE LINES LEFT                            
         C     RE,=F'52'           ENSURE LINES LEFT                            
         BL    PRTX                                                             
         ZIC   R1,PG               NOT ENOUGH/BUMP PAGE NUMBER                  
         LA    R1,1(R1)                                                         
         STC   R1,PG                                                            
         BAS   RE,DOHEADS          DO HEADLINES                                 
                                                                                
PRTX     B     XIT                                                              
*                                                                               
SPACE1   EQU   X'09'                                                            
SPACE2   EQU   X'11'                                                            
SPACE3   EQU   X'19'                                                            
         EJECT                                                                  
* LAST TIME WHEN PRINTING REPORT                                                
LASTP    NTR1                                                                   
         MVI   MYP-1,X'FF'           LAST TIME                                  
         L     R2,AFACILS                                                       
         L     R2,12(R2)           A(TIA)                                       
         LA    RE,=C'PRTQUE '                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',,,MYP-1,(TERMNAL,(R2))                 
         CLI   DMCB+8,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R2,RISMESSH                                                      
         MVC   8(43,R2),=C'*ACTION COMPLETED  RIS,      ON PRINT QUEUE'         
         EDIT  (B2,HALF2),(5,31(R2)),ALIGN=LEFT                                 
         FOUT (R2)                                                              
         MVI   PRNT,0                                                           
*                                                                               
         NI    RISMNTSH+4,X'DF'    FORCE VALIDATION WHEN RETURNING              
*                                                                               
         B     XIT                                                              
                                                                                
*                                                                               
MAXOUT   DS    0H                                                               
         LA    R2,RISMESSH                                                      
         MVC   8(40,R2),=C'*IO TIMEOUT - REDUCE REQUEST PARAMETERS*'            
         FOUT  (R2)                                                             
         B     SKIPCNT   TURN OFF VALIDATED BITS                                
         EJECT                                                                  
*                                                                               
DBLOK    DS    0H                                                               
       ++INCLUDE DEDBLOCK                                                       
                                                                                
*                                                                               
       ++INCLUDE RGENEROL                                                       
         LTORG                                                                  
TEMPIO   DS    CL1000                                                           
         EJECT                                                                  
       ++INCLUDE RERISWRK                                                       
       ++INCLUDE REPIOBLK                                                       
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
SETHEADS CSECT                                                                  
         NMOD1 0,*SHED*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         TM    VIEWS,X'02'         LISTD VIEW                                   
         BO    SHED0020                                                         
         SR    R6,R6               NO                                           
         IC    R6,LISTBYTE                                                      
         LA    R6,ATITLE(R6)                                                    
         L     R5,0(R6)                                                         
         A     R5,RELO2            RELOCATE                                     
         MVC   RISTITL(L'TITLE1),TITLE1                                         
         MVC   RISTITL+INDENT(22),0(R5)                                         
         FOUT  RISTITLH                                                         
         B     SHED0800                                                         
                                                                                
SHED0020 EQU   *                                                                
         TM    VIEWS3,X'20'        DISPLAY TAKEOVER ORDERS ONLY?                
         BO    SHED0040            YES                                          
         MVC   RISTITL(L'TITLED3),TITLED3                                       
         FOUT  RISTITLH                                                         
**       ZIC   R1,0(R2)                                                         
**       AR    R2,R1                                                            
**       MVC   8(L'TITLED2,R2),TITLED2                                          
**       FOUT  (R2)                                                             
         B     SHED0800                                                         
*                                                                               
SHED0040 EQU   *                                                                
         MVC   RISTITL(L'TITLED4),TITLED4                                       
         FOUT  RISTITLH                                                         
SHED0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
TITLE1   DC    C'OF GS  STA  AGENCY ADVR DT SAL                        X        
               CONTRACT CONTRACT PERIOD'                                        
*                                                                               
TITLED1  DC    C'STA   AGENCY       PRODUCT      FLIGHT   CONTRACT    AX        
               MOUNT     CREATE   SNT  '                                        
TITLED2  DC    C'OFF   ADVERTISER   SALES                 DEMO        SX        
               HARE      ACTIVITY VER  '                                        
TITLED3  DC    C'ST/OF  AGY/ADV     PROD/SALES   FLIGHT   CON/DEMO    AX        
               MT/SHR    CRD/ACT  VR/ST'                                        
TITLED4  DC    C'ST/OF  AGY/ADV     PROD/SALES   FLIGHT   NEW/OLD CON  X        
                 AMT/SHR  CRD/TKO  VR/ST'                                       
         LTORG                                                                  
         CSECT                                                                  
*--------------------------------------------------------------|                
* DIV100-ROUND PACKED NUMBER                                   |                
*    INPUT-DUB PACKED NUMBER                                   |                
*    OUTPUT-DUB PACKED NUMBER                                  |                
*--------------------------------------------------------------|                
DIV100   NTR1  BASE=*,LABEL=*                                                   
**       LA    R3,100                                                           
**       SR    R4,R4                                                            
**       SR    R5,R5                                                            
**       CVB   R5,DUB                                                           
**       DR    R4,R3                                                            
**       CVD   R5,DUB                                                           
         XC    WORK,WORK           CLEAR WORK AREA                              
         MVC   WORK+2(8),DUB                                                    
         DP    WORK(10),=P'100'    CREATE 8-CHAR RESULT, PLUS                   
*                                     TWO-CHAR REMAINDER                        
         MVC   DUB,WORK            RESET DUB WITH RESULT                        
         XIT1                                                                   
*END OF DIV100                                                                  
*----------------------------------------------------------------               
         LTORG                                                                  
         EJECT                                                                  
* GETS SHR PCT AND CALCULATES $ SHARE AMOUNT                                    
* ADDS $ OF CONTRACT INTO ALLSHR TO BE USED IN CALCULATING                      
* AVERAGE SHARE PERCENT                                                         
CALCSHR  NTR1  LABEL=*,BASE=*                                                   
         MVC   BYTE,RISTATUS       FUDGE RISTATUS                               
         MVI   RISTATUS,1                                                       
         BAS   R5,LSTDLR           RETURNS $AMT IN R0                           
         MVC   RISTATUS,BYTE                                                    
         CVD   R0,DUB                                                           
                                                                                
         L     R5,AIOAREA                                                       
         USING RCONREC,R5                                                       
         LA    R5,RCONELEM                                                      
CLC10    ZIC   R1,1(R5)                                                         
         LTR   R1,R1                                                            
         BZ    CLCX                                                             
         AR    R5,R1                                                            
         CLI   0(R5),X'06'                                                      
         BH    CLCX                                                             
         BNE   CLC10                                                            
         USING RCONSPEL,R5                                                      
         TM    RCONSPES,X'04'      ,,IS IT PERCENTAGE                           
         BNO   CLCX                ,,IF NOT, SKIP IT                            
         ZAP   WORK(16),=P'0'                                                   
         ICM   R1,15,RCONSPAM      .GET PERCENT                                 
         LTR   R1,R1               .IF ZERO                                     
         BZ    CLC20               ..CHECK FOR LOSS                             
         CVB   RF,DUB              CHECK FOR NEGATIVE (CREDIT) $$               
         LTR   RF,RF               NEGATIVE?                                    
         BM    CLCX                YES - DON'T ADD ANYTHING                     
         AP    WORK(16),DUB        GET TOTAL $ INTO WORK                        
         AP    SHRTOT,DUB          ADD TO ALL SHARE $                           
         CVD   R1,DUB              PCT GOES INTO DUB                            
         MP    WORK(16),=P'20000'  MULT BY 10,000 FOR DECL ALIGN                
*                                     AND 2 FOR ROUNDING                        
         AP    WORK(16),DUB        ADD SHR PCT FOR ROUNDING                     
         DP    WORK(16),DUB        DIVIDE $AMT / SHR PCT = ORIGINAL $           
         DP    WORK(08),=P'2'      DIVIDE FOR ROUNDING                          
                                                                                
**       AP    WORK(8),=P'5000'   ROUND                                         
**       ZAP   DUB,WORK(8)                                                      
**       ZAP   WORK(16),DUB                                                     
**       DP    WORK(16),=P'10000'                                               
**       ZAP   WORK+20(8),WORK+5(8)                                             
**       AP    ALLSHR,WORK+20(8)                                                
         AP    ALLSHR,WORK(7)                                                   
         B     CLCX                SKIP OUT                                     
CLC20    EQU   *                                                                
         ZIC   R0,RCONSPNU         SET # OF MINI ELTS                           
         LA    RF,RCONSPAM         1ST STATION AMOUNT                           
CLC30    EQU   *                                                                
         OC    0(4,RF),0(RF)       ANY VALUE IN FIELD?                          
         BNZ   CLC40               YES - IT'S A REAL LOSS!!                     
         LA    RF,9(RF)            NO  - BUMP TO NEXT STA'S $$/%                
         BCT   R0,CLC30                                                         
         B     CLCX                NO VALUES - NOT A LOSS                       
CLC40    EQU   *                   PROCESS A LOSS                               
         DROP  R5                                                               
         LA    R5,RCONELEM                                                      
CLC50    ZIC   R1,1(R5)                                                         
         LTR   R1,R1                                                            
         BZ    CLCX                END OF RECORD                                
         AR    R5,R1                                                            
         CLI   0(R5),X'08'         TRUE ACTIVITY DATE ELT?                      
         BH    CLCX                NOT PRESENT - EXIT                           
         BNE   CLC50               NO  - GO BACK FOR NEXT ELT                   
         USING RCONACEL,R5                                                      
         ZICM  RF,RCONAC$$,4       LOAD LOSS DOLLARS FOR CONVERT                
         CVD   RF,DUB                                                           
***      MP    DUB,=P'100'         ADD TWO DECL PLACES                          
*                                                                               
*   PRO RATE LOSS $$  *                                                         
***************************************                                         
*                                                                               
         MP    DUB,=P'1000'        ADD THREE DECL PLACES                        
                                                                                
         LA    R2,REPIOTBL                                                      
         USING REPIOD,R2                                                        
                                                                                
         GOTO1 VDATCON,DMCB,(2,RIPKEY+8),WORK         FLIGHT START              
         GOTO1 (RF),DMCB,(2,RIPKEY+10),WORK+6      FLIGHT END                   
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6,RR=YES                              
         SR    R1,R1                                                            
         ICM   R1,3,DMCB+8              BYTES 0-1 #DAYS IN FLIGHT               
                                                                                
         ZAP   WORK(16),DUB             LOSS $ IN 3 DEC TO WORK(16)             
         CVD   R1,DUB                   NUMBER OF FLIGHT DAYS IN DUB            
         DP    WORK(16),DUB             (LOSS $)  / (FLIGHT DAYS)               
                                                                                
* NOW WORK(8) HAS  PER DAY LOSS OVER FLIGHT TO 3 DECIMALS                       
                                                                                
***      CALCULATE # DAYS OF REQUEST PERIOD THAT FALLS                          
***      WITHIN FLIGHT                                                          
*                                                                               
         SR    R1,R1                                                            
         CLC   RIPDATE,RIPKEY+8    REQ END < FLIGHT START ?                     
         BL    CLC60               YES / 0 DAYS OVERLAP                         
         CLC   RIPDATS,RIPKEY+10   REQ START > FLIGHT END ?                     
         BH    CLC60               YES/ 0 DAYS OVERLAP                          
*                                                                               
* DO I USE REQ START OR FLIGHT START TO CALCULATE # DAYS OVERLAP ?              
                                                                                
         MVC   WORK+10(2),RIPKEY+8    SET FLIGHT START TO START DATE            
         CLC   RIPDATS,RIPKEY+8       REQ START > FLIGHT START ?                
         BNH   *+10                                                             
         MVC   WORK+10(2),RIPDATS     YES/SET REQ START DATE                    
         GOTO1 VDATCON,DMCB,(2,WORK+10),WORK+20       START DATE                
                                                                                
* DO I USE REQ END OR FLIGHT END TO CALCULATE # DAYS OVERLAP ?                  
                                                                                
         MVC   WORK+10(2),RIPKEY+10   SET FLIGHT END TO END DATE                
         CLC   RIPDATE,RIPKEY+10      REQ END > FLIGHT END ?                    
         BH    *+10                                                             
         MVC   WORK+10(2),RIPDATE     NO/ SET REQ END DATE TO END               
         GOTO1 VDATCON,DMCB,(2,WORK+10),WORK+26      REQ END                    
                                                                                
         GOTO1 =V(PERVERT),DMCB,WORK+20,WORK+26,RR=YES                          
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,DMCB+8              BYTES 0-1 #DAYS IN REQUEST              
CLC60    CVD   R1,DUB                 DUB HAS # DAYS IN REQUEST                 
                                                                                
         ZAP   WORK+8(16),WORK(8)     PER DAY LOSS TO 3 DEC                     
         MP    WORK+8(16),DUB         (PER DAY LOSS) X (# DAYS IN REQ)          
         AP    WORK+8(16),=P'5'                                                 
         DP    WORK+8(16),=P'10'       ROUND TO 2 DEC                           
         ZAP   DUB,WORK+8(14)          PUT REQUEST PERIOD LOSS -> DUB           
         DROP  R2                                                               
*******************************************************                         
         AP    ALLSHR,DUB(8)       ADD LOSS DOLLARS TO ACCUM                    
CLCX     XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042RERIS05S  10/20/03'                                      
         END                                                                    
