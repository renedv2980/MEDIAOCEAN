*          DATA SET RESFM2DA   AT LEVEL 099 AS OF 05/01/02                      
*PHASE T8182DA,*                                                                
*INCLUDE UNBOOK                                                                 
         TITLE 'T8182D - RESFM2D - PENDING CONTRACT SCROLLER'                   
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM2D (T8182D) --- PENDING CONTRACT SCROLLER           *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* JUL13/93 (SKU) --- BORN TO RUN                                  *             
*                                                                 *             
* AUG24/93 (SKU) --- ADD SIGN-ON RESTRICTION                      *             
*                                                                 *             
* SEP16/93 (SKU) --- FIX BUG OF NOT TURNING READ DELETE OFF       *             
*                                                                 *             
* APR06/94 (SKU) --- ADD PENDING/FORECAST FILTER                  *             
*                                                                 *             
* MAY02/94 (SKU) --- ADD/REMOVE FIELDS FOR FORECASTING            *             
*                                                                 *             
* AUG08/94 (SKU) --- ADD FLIGHT DATE FILTER                       *             
*                                                                 *             
* JAN18/95 (SKU) --- ENABLE NUMERIC DATE FILTER ENTRIES           *             
*                                                                 *             
* OCT12/95 (SKU) --- REPLACE SALESPERSON CODE OVER SERVICE        *             
*                                                                 *             
* MAR18/96 (SKU) --- SKIP DAYPART DISPLAY FOR NEW SAR (SEL)       *             
*                                                                 *             
* NOV07/96 (SKU) --- SKIP X'04' BUCKET CHECK FOR PENDING          *             
*                    AND CHECK IF 'ACC-' IN BUYER = NOT PENDING   *             
*                                                                 *             
* OCT20/98 (AST) --- ADD NATIONAL/LOCAL OFFICE FILTER             *             
*                                                                 *             
* MAR18/99 (SKU) --- UPGRADE DEMOCON CALL                         *             
*TOMB**************************************************************             
*                                                                               
T8182D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**182D**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         LIST                                         
         BE    LIST                                                             
                                                                                
         B     EXIT                                                             
                                                                                
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
* VALIDATE KEY                                                                  
*******************************************************************             
VKEY     DS    0H                                                               
         MVI   NEXTSCRN,C'N'       WE'RE NOT SCROLLING                          
                                                                                
         TM    PFHSTAH+4,X'20'     IF NO FIELDS WERE CHANGED                    
         BZ    VKEY010               USER WANTS TO PAGE TO NEXT SET OF          
         TM    PFHOFFH+4,X'20'       CONTRACTS                                  
         BZ    VKEY010                                                          
         TM    PFHFILH+4,X'20'                                                  
         BZ    VKEY010                                                          
         TM    PFHDATEH+4,X'20'                                                 
         BZ    VKEY010                                                          
                                                                                
         TM    PFHSTAH+4,X'80'     IF FIELDS WERE CHANGED BUT CHANGED           
         BO    VKEY010             TO THE SAME DATA, USER WANTS TO              
         TM    PFHOFFH+4,X'80'     LIST FROM THE BEGINNING                      
         BO    VKEY010                                                          
         TM    PFHFILH+4,X'80'     LIST FROM THE BEGINNING                      
         BO    VKEY010                                                          
         TM    PFHDATEH+4,X'80'    LIST FROM THE BEGINNING                      
         BO    VKEY010                                                          
                                                                                
         MVI   NEXTSCRN,C'Y'                                                    
         B     VKEYX                                                            
                                                                                
VKEY010  DS    0H                                                               
         XC    FIRSTKEY,FIRSTKEY                                                
         OI    PFHSTAH+4,X'20'     SET VALIDATED                                
         OI    PFHOFFH+4,X'20'     SET VALIDATED                                
         OI    PFHFILH+4,X'20'     SET VALIDATED                                
         OI    PFHDATEH+4,X'20'    SET VALIDATED                                
                                                                                
         LA    R2,PFHSTAH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALISTA                                                          
         MVC   STA_CALL,WORK       RETURNED FROM VALISTA                        
         MVC   STA_GRP,WORK+41                                                  
                                                                                
* IF STATION SIGN-ON, CHECK IF IT'S A VALID SIGN ON ID                          
                                                                                
         CLI   TWAACCS,C'$'                                                     
         BNE   VKEY020                                                          
         L     R6,AIO                                                           
         USING RSTASOEL,R6                                                      
         MVI   ELCODE,6            GET VALID SIGN ON ID ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   VKEY020                                                          
                                                                                
VKEY015  DS    0H                                                               
         CLC   RSTASID,TWAORIG     VALID SIGN-ON?                               
         BE    VKEY020             YES, PROCEED                                 
         BAS   RE,NEXTEL           NOPE, CHECK NEXT ELEMENT                     
         BE    VKEY015                                                          
         B     SLOCKOUT            ALL DONE, NO MATCH, NOT VALID                
         DROP  R6                                                               
                                                                                
VKEY020  DS    0H                                                               
         LA    R2,PFHOFFH          OFFICE OPTIONAL                              
         CLI   5(R2),0                                                          
         BE    VKEY030                                                          
         GOTO1 VALIOFF                                                          
                                                                                
VKEY030  DS    0H                                                               
         LA    R2,PFHDATEH         FLIGHT DATE OPTIONAL                         
         CLI   5(R2),0                                                          
         BE    VKEY050                                                          
         GOTO1 VALIPERI                                                         
                                                                                
VKEY050  DS    0H                                                               
         LA    R2,PFHFILH          FILTER OPTIONAL                              
         CLI   5(R2),0                                                          
         BE    VKEY060                                                          
         CLI   PFHFIL,C'P'                                                      
         BE    VKEY060                                                          
*                                                                               
         CLI   PFHFIL,C'N'         NATIONAL OFFICE                              
         BE    VKEY060                                                          
         CLI   PFHFIL,C'L'         LOCAL OFFICE                                 
         BE    VKEY060                                                          
*                                                                               
         CLI   PFHFIL,C'F'                                                      
         BNE   INVLFLD                                                          
                                                                                
         GOTO1 CALLOV,DMCB,PFHLAST,X'D90818BD'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   FCTLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     VKEYX                                                            
                                                                                
VKEY060  DS    0H                                                               
         GOTO1 CALLOV,DMCB,PFHLAST,X'D90818B0'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PDPLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
                                                                                
VKEYX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                               
* INPUT  - R2 POINTS TO PERIOD FIELD HEADER                                     
* OUTPUT - STARTDT HAS START DATE                                               
*          ENDDT HAS END DATE                                                   
***********************************************************************         
VALIPERI NTR1                                                                   
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),BLOCK,C',=,-'                                  
         CLI   DMCB+4,0                                                         
         BE    INVLPER             ERROR ENCOUNTERED                            
*                                                                               
* VALIDATE START DATE                                                           
*                                                                               
         LA    R5,BLOCK                                                         
         GOTO1 DATVAL,DMCB,(2,12(R5)),STARTDT                                   
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLPER                                                          
*                                                                               
* VALIDATE END DATE                                                             
* END DATE IS SECOND IN SCANNER BLOCK                                           
*                                                                               
         MVC   ENDDT,STARTDT                                                    
         CLI   1(R5),0                                                          
         BE    VALPER10                                                         
                                                                                
         GOTO1 DATVAL,DMCB,(2,22(R5)),ENDDT                                     
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLPER                                                          
*                                                                               
         CLC   ENDDT,STARTDT       END V START DATE                             
         BL    INVLDAT             ERR - END DATE BEFORE START DATE             
*                                                                               
VALPER10 DS    0H                  GET BROADCAST START-END DATES                
*                                  SET DAY TO THE 15TH SINCE THIS WOULD         
         MVC   STARTDT+4(2),=C'15' ENSURE WE'LL GET CORRECT B'CAST MON          
                                                                                
         GOTO1 GETBROAD,DMCB,(1,STARTDT),WORK,GETDAY,ADDAY                      
         CLI   DMCB,X'FF'                                                       
         BE    INVLPER             ERROR?                                       
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,BSTARTDT)                                
*                                                                               
* GET BROADCAST MONTH END DATE                                                  
*                                                                               
         MVC   ENDDT+4(2),=C'15'                                                
                                                                                
         GOTO1 GETBROAD,DMCB,(1,ENDDT),WORK,GETDAY,ADDAY                        
         CLI   DMCB,X'FF'                                                       
         BE    INVLPER             ERROR?                                       
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,BENDDT)                                
*                                                                               
VALPERX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* LIST RECORDS                                                                  
*******************************************************************             
LIST     DS    0H                                                               
         OC    FIRSTKEY,FIRSTKEY                                                
         BNZ   LIST05                                                           
         CLI   PFHFIL,C'F'                                                      
         BE    LIST03                                                           
         TWAXC PDPOF1H                                                          
         B     LIST05                                                           
                                                                                
LIST03   DS    0H                  FOR FORECASTING                              
         TWAXC FCTOF1H                                                          
                                                                                
LIST05   DS    0H                                                               
         MVI   CLEARFLG,C'N'       UNLESS FIRST TIME, SCRN NOT CLEARED          
                                                                                
         CLI   PFHFIL,C'F'                                                      
         BE    LIST08                                                           
         LA    R2,PDPOF1H          PENDING LIST STARTS AT THIS FIELD            
         B     *+8                                                              
                                                                                
LIST08   DS    0H                                                               
         LA    R2,FCTOF1H          FORECAST LIST STARTS AT THIS FIELD           
                                                                                
         CLI   NEXTSCRN,C'Y'       USER JUST PRESSED ENTER?                     
         BNE   LIST30              W/O CHANGING THE KEY FIELDS                  
         MVC   KEY,SAVEKEY         YES, DISPLAY NEXT PAGE OF CONTRACTS          
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLI   PFHOFFH+5,0                                                      
         BH    LIST10                                                           
         CLC   KEY(RCONSOFF-RCONSTYP),KEYSAVE                                   
         BNE   LISTX                                                            
         CLI   LASTSCRN,C'Y'       IF LAST TIME WAS THE LAST RECORD             
         BNE   LIST150             DON'T DO A SEQ                               
         MVI   LASTSCRN,C'N'       GET THE RECORD FOR THIS KEY NOW              
         B     LIST100                                                          
                                                                                
LIST10   DS    0H                  KEY ON OFFICE                                
         CLC   KEY(RCONSTEM-RCONSTYP),KEYSAVE                                   
         BNE   LISTX                                                            
         CLI   LASTSCRN,C'Y'       IF LAST TIME WAS THE LAST RECORD             
         BNE   LIST150             DON'T DO A SEQ                               
         MVI   LASTSCRN,C'N'       GET THE RECORD FOR THIS KEY NOW              
         B     LIST100                                                          
                                                                                
LIST30   DS    0H                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
                                                                                
         MVI   RCONSTYP,X'CC'                                                   
         MVC   RCONSREP,AGENCY                                                  
         MVC   RCONSSTA,STA_CALL                                                
         CLI   PFHOFFH+5,0                                                      
         BE    *+10                                                             
         MVC   RCONSOFF,PFHOFF                                                  
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLI   PFHOFFH+5,0                                                      
         BH    LIST35                                                           
         CLC   KEY(RCONSOFF-RCONSTYP),KEYSAVE                                   
         BNE   LISTX                                                            
         MVC   FIRSTKEY,KEY                                                     
         B     LIST100                                                          
                                                                                
LIST35   DS    0H                                                               
         CLC   KEY(RCONSTEM-RCONSTYP),KEYSAVE                                   
         BNE   LISTX                                                            
         MVC   FIRSTKEY,KEY                                                     
                                                                                
*                                                                               
*** CODE FOR NATIONAL/LOCAL OFFICE FILTER *************************             
*                                                                               
LIST100  DS    0H                                                               
*                                                                               
         GOTO1 GETOFFLG                                                         
         DROP  R6                                                               
         CLI   PFHFIL,C'L'         LOCAL FILTER?                                
         BNE   LIST101             NO                                           
         TM    BYTE,X'80'          LOCAL?                                       
         BNO   LIST150             NATIONAL OFFICE, LOCAL FILTER, SKIP          
         B     LIST102                                                          
LIST101  CLI   PFHFIL,C'N'         NATIONAL FILTER?                             
         BNE   LIST102             NO, DISPLAY ALL                              
         CLI   BYTE,0              FILTER IS NATIONAL, OFFICE NAT?              
         BNE   LIST150             OFFICE LOCAL, SKIP                           
*                                                                               
LIST102  DS    0H                                                               
         OI    DMINBTS,X'08'       READ DELETED                                 
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08' RESET                                        
                                                                                
         L     R6,AIO              INCASE RECORD IS DELETED BUT THE             
         USING RCONREC,R6          PASSIVE POINTERS AREN'T                      
         TM    RCONCNTL,X'80'      SKIP IF RECORD IS REALLY MARKED              
         BO    LIST150             FOR DELETION                                 
*                                                                               
* CHECK IF PENDING/FORECAST CONTRACT                                            
*                                                                               
LIST105  DS    0H                                                               
         XC    CONTSTAT,CONTSTAT   CLEAR FLAGS                                  
                                                                                
         CLI   PFHFIL,C'F'         FILTER ON FORECAST                           
         BE    LIST110                                                          
*                                                                               
* IF FILTER IS BLANK OR P, DEFAULT TO FILTER ON PENDING                         
*                                                                               
         TM    RCONMODR,X'10'      IS THIS A PENDING CONTRACT?                  
         BO    LIST150                                                          
         DROP  R6                                                               
                                                                                
         L     R6,AIO              YES, BUT IS IT A FORECAST CONTRACT?          
         MVI   ELCODE,X'12'        EXPANPDED SAR ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   LIST106                                                          
                                                                                
         USING RSARXEL,R6                                                       
         CLI   RSARXLEN,RSARXLTH   ONLY NEW SAR ELEM HAS FORECAST FLAG          
         BL    LIST106                                                          
         TM    RSARXFLG,X'18'      FLAGGED AS FORECAST?                         
         BNZ   LIST115                                                          
         DROP  R6                                                               
                                                                                
LIST106  DS    0H                                                               
         OI    CONTSTAT,CONTPEND   FLAG AS PENDING CONTRACT                     
         B     LIST115                                                          
                                                                                
LIST110  DS    0H                  FILTER ON FORECAST                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'        EXPANPDED SAR ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   LIST115                                                          
                                                                                
         USING RSARXEL,R6                                                       
         CLI   RSARXLEN,RSARXLTH   ONLY NEW SAR ELEM HAS FORECAST FLAG          
         BL    LIST115                                                          
         TM    RSARXFLG,X'18'      FLAGGED AS FORECAST?                         
         BZ    LIST115                                                          
         OI    CONTSTAT,CONTFORE   FLAG AS FORECAST                             
         DROP  R6                                                               
                                                                                
LIST115  DS    0H                                                               
         TM    CONTSTAT,CONTPEND+CONTFORE                                       
         BZ    LIST150                                                          
                                                                                
         L     R6,AIO              PENDING/FORECAST K IS DEFINED TO BE          
         USING RCONREC,R6                                                       
         CLC   =C'ACC-',RCONBUYR   NOT ACCOUTING                                
         BE    LIST150                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,3            NO BUCKET                                    
         BAS   RE,GETEL                                                         
         BE    LIST150                                                          
*                                                                               
         LR    RF,RA               USE R2 TO COVER THE ENTRY                    
         AH    RF,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,RF                                                       
         TM    SVPGPBIT,X'01'                                                   
                                                                                
*                                  8TH BIT (TEST X'04 ELT) ON?                  
         BNO   LIST117             NO                                           
         DROP  RF                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,4            NO INVOICE BUCKET                            
         BAS   RE,GETEL                                                         
         BE    LIST150                                                          
*                                                                               
LIST117  EQU   *                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,6            NO SPL/EPL DATA                              
         BAS   RE,GETEL                                                         
         BE    LIST150                                                          
                                                                                
LIST120  DS    0H                                                               
         CLI   PFHDATEH+5,0        FLIGHT DATE FILTER                           
         BE    LIST123                                                          
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLC   BSTARTDT,RCONDATE+3                                              
         BH    LIST150                                                          
         CLC   BENDDT,RCONDATE                                                  
         BL    LIST150                                                          
         DROP  R6                                                               
                                                                                
LIST123  DS    0H                                                               
         CLI   CLEARFLG,C'Y'                                                    
         BE    LIST130                                                          
         LR    R3,R2               SAVE OFF R2                                  
                                                                                
         CLI   PFHFIL,C'F'                                                      
         BE    LIST125                                                          
         TWAXC PDPOF1H                                                          
         B     LIST128                                                          
                                                                                
LIST125  DS    0H                  FOR FORECASTING                              
         TWAXC FCTOF1H                                                          
                                                                                
LIST128  DS    0H                                                               
         LR    R2,R3               RESTORE R2                                   
         MVI   CLEARFLG,C'Y'                                                    
                                                                                
LIST130  DS    0H                                                               
         BAS   RE,DISCON           DISPLAY THE CONTRACT                         
                                                                                
         TM    CONTSTAT,CONTPEND                                                
         BZ    LIST140                                                          
         LA    RF,PDPC24H          STOP IF WE'RE AT END OF SCREEN               
         CR    R2,RF                                                            
         BNL   LISTX                                                            
         B     LIST150                                                          
                                                                                
LIST140  DS    0H                                                               
         TM    CONTSTAT,CONTFORE                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RF,FCTC24H          STOP IF WE'RE AT END OF SCREEN               
         CR    R2,RF                                                            
         BNL   LISTX                                                            
                                                                                
LIST150  DS    0H                                                               
         GOTO1 SEQ                                                              
                                                                                
         CLI   PFHOFFH+5,0                                                      
         BH    LIST160                                                          
         CLC   KEY(RCONSOFF-RCONSTYP),KEYSAVE                                   
         BNE   LIST250                                                          
         B     LIST100                                                          
                                                                                
LIST160  DS    0H                                                               
         CLC   KEY(RCONSTEM-RCONSTYP),KEYSAVE                                   
         BE    LIST100                                                          
                                                                                
LIST250  DS    0H                  WE'VE HIT THE LAST CONTRACT                  
         MVI   LASTSCRN,C'Y'                                                    
         MVC   SAVEKEY,FIRSTKEY    NEXT SCREEN GOES TO THE BEGINNNING           
*** TESTING                                                                     
         B     ENDLIST                                                          
*** TESTING                                                                     
         TM    CONTSTAT,CONTPEND                                                
         BZ    *+14                                                             
         MVC   PDPLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     ENDLIST                                                          
         MVC   FCTLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     ENDLIST                                                          
                                                                                
LISTX    DS    0H                                                               
         MVI   LASTSCRN,C'N'                                                    
         MVC   SAVEKEY,KEY                                                      
*** TESTING                                                                     
         B     NEXTLIST                                                         
*** TESTING                                                                     
         TM    CONTSTAT,CONTPEND                                                
         BZ    *+14                                                             
         MVC   PDPLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     NEXTLIST                                                         
         MVC   FCTLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     NEXTLIST                                                         
         EJECT                                                                  
*******************************************************************             
* DISPLAY CONTRACT INFORMATION                                                  
* R2 HAS STARTING FIELD HEADER                                                  
* AIO HAS ADDRESS OF CONTRACT RECORD                                            
*******************************************************************             
DISCON   NTR1                                                                   
         TM    CONTSTAT,CONTFORE   IF FORECAST, JUMP TO FORECAST DISP           
         BNZ   DIS4CAST                                                         
         TM    CONTSTAT,CONTPEND   IF PENDING                                   
         BNZ   *+6                                                              
         DC    H'0'                ANYTHING ELSE, DIES                          
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
* OFFICE                                                                        
         MVC   8(L'RCONKOFF,R2),RCONKOFF                                        
         BAS   RE,BUMPNEXT                                                      
         DROP  R6                                                               
                                                                                
* BUDGET                                                                        
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISK05                                                           
         OC    RSARXBGT,RSARXBGT     ANY BUDGET                                 
         BZ    DISK03                                                           
         EDIT  (B4,RSARXBGT),(8,8(R2)),ALIGN=LEFT                               
         B     DISK05                                                           
                                                                                
DISK03   DS    0H                  BUDGET IS ZERO                               
         CLI   RSARXLEN,RSARXLTH   IS THIS AN EXPANDED SAR ELMENT?              
         BL    DISK05                                                           
         TM    RSARXFLG,X'40'      IF BUDGET ENTERED FLAG ON                    
         BZ    DISK05                PRINT 0 DOLLARS                            
         MVI   8(R2),C'0'                                                       
                                                                                
DISK05   DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
         DROP  R6                                                               
                                                                                
* SHARE GOAL                                                                    
         L     R6,AIO                                                           
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISK08                                                           
         CLI   RSARXLEN,RSARXLTH   ONLY EXPANDED SAR HAS SHARE GOAL             
         BL    DISK08                                                           
         OC    RSARXSHG,RSARXSHG                                                
         BZ    DISK07                                                           
         EDIT  RSARXSHG,(3,8(R2)),ALIGN=LEFT                                    
         B     DISK08                                                           
                                                                                
DISK07   DS    0H                  SHARE GOAL IS ZERO                           
         TM    RSARXFLG,X'80'      WAS IT ENTERED?                              
         BZ    DISK08                                                           
         MVI   8(R2),C'0'          YES, PRINT 0                                 
                                                                                
DISK08   DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
         DROP  R6                                                               
*                                                                               
* STATION BUDGET = MARKET BUDGET * SHARE GOAL                                   
*                                                                               
         L     R6,AIO                                                           
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISK11                                                           
         CLI   RSARXLEN,RSARXLTH   ONLY EXPANDED SAR HAS SHARE GOAL             
         BL    DISK11                                                           
                                                                                
         MVC   FULL,RSARXBGT       GET MARKET BUDGET                            
         L     RF,FULL             IF ZERO, SEE IF ENTERED AS ZERO              
         LTR   RF,RF                                                            
         BZ    DISK10                                                           
                                                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,FULL                                                       
         XC    WORK,WORK                                                        
         MVC   WORK+1(1),RSARXSHG                                               
         MH    RF,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   WORK+3,100                                                       
         D     RE,WORK                                                          
         LR    R4,RF                                                            
         EDIT  (R4),(8,8(R2)),ALIGN=LEFT                                        
         B     DISK11                                                           
                                                                                
DISK10   DS    0H                                                               
         TM    RSARXFLG,X'40'      BUDGET ENTERED AS ZERO?                      
         BNO   DISK11              NO                                           
         MVI   8(R2),C'0'          YES - SEND BACK A ZERO                       
                                                                                
DISK11   DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
         DROP  R6                                                               
                                                                                
* FLIGHT                                                                        
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,8(R2))                               
         MVI   16(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,17(R2))                            
         BAS   RE,BUMPNEXT                                                      
                                                                                
* ADVERTISER                                                                    
         GOTO1 ADVNAME,DMCB,RCONKADV                                            
         MVC   8(L'PDPADV,R2),WORK                                              
         BAS   RE,BUMPNEXT                                                      
                                                                                
* AGENCY                                                                        
         GOTO1 AGYNAME,DMCB,RCONKAGY                                            
         MVC   8(L'PDPAGY,R2),WORK                                              
         BAS   RE,BUMPNEXT                                                      
                                                                                
* BUYER                                                                         
         CLI   TWAOFFC,C'*'        DISPLAY K# FOR DDS TERMINALS                 
         BNE   DISK13                                                           
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,8(R2)),ALIGN=LEFT                                   
         B     DISK14                                                           
                                                                                
DISK13   DS    0H                                                               
         MVC   8(L'PDPBUY,R2),RCONBUYR                                          
                                                                                
DISK14   DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
                                                                                
* PRODUCT                                                                       
         CLC   RCONPRD,SPACES      IF PRODUCT CODE IS SPACES                    
         BNE   DISK15              USE PRODUCT EXPANSION ELEMENT                
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,5                                                         
         BAS   RE,GETEL                                                         
         BNE   DISK18                                                           
         USING RCONEXEL,R6                                                      
         MVC   8(L'PDPPRD,R2),RCONEXPR                                          
         B     DISK18                                                           
         DROP  R6                                                               
                                                                                
DISK15   DS    0H                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         GOTO1 PRDNAME,DMCB,RCONKADV,RCONPRD                                    
         MVC   8(L'PDPPRD,R2),WORK                                              
         DROP  R6                                                               
                                                                                
DISK18   DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
                                                                                
* DAYPARTS                                                                      
         L     R6,AIO                                                           
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISK70                                                           
                                                                                
DISK20   DS    0H                                                               
         LA    R4,WORK             DAYPARTS                                     
         XC    WORK(80),WORK                                                    
         XC    DPTFLAG,DPTFLAG     0=1 CHAR DPT CODE                            
         LA    R5,RSARXDPT                                                      
         SR    RE,RE               COUNTER -# DAYPARTS INPUTTED                 
         LA    R3,6                                                             
                                                                                
DISK30   LA    R8,DPTABLE                                                       
         OC    0(1,R5),0(R5)                                                    
         BZ    DISK60              NO DPT                                       
                                                                                
         CLI   1(R5),X'C1'                                                      
         BL    DISK40              NO CHAR IN CPP PART OF FIELD                 
         CLI   1(R5),X'E9'                                                      
         BH    DISK40              NO CHAR IN CPP PART OF FIELD                 
*                                  3 CHAR DPT CODE                              
         MVI   DPTFLAG,1                                                        
         B     DISK50                                                           
                                                                                
DISK40   CLC   3(1,R8),0(R5)                                                    
         BE    DISK50              ONE CHAR CODE IN RSARDPT                     
         CLI   0(R8),X'FF'                                                      
         BNE   DISK45                                                           
         MVC   8(6,R2),=C'******'                                               
         B     DISK70                                                           
*                                                                               
DISK45   LA    R8,L'DPTABLE(R8)                                                 
         B     DISK40                                                           
                                                                                
DISK50   CH    R3,=H'6'                                                         
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
                                                                                
         CLI   DPTFLAG,0                                                        
         BNE   *+14                DPT IS THREE CHAR CODE                       
         MVC   0(3,R4),0(R8)       DPT 1 CHAR MOVE FROM TABLE                   
         B     *+10                                                             
         MVC   0(3,R4),0(R5)       MOVE DPT 3 CHAR FROM RSARDPT                 
         A     RE,=F'1'            INCREMENT COUNTER                            
         LA    R4,3(R4)                                                         
                                                                                
DISK60   LA    R5,3(R5)            NEXT DPT FIELD                               
         BCT   R3,DISK30                                                        
         MVC   8(L'PDPDPT,R2),WORK                                              
                                                                                
DISK70   DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
         DROP  R6                                                               
                                                                                
* SALESPERSON CODE                                                              
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
         MVC   8(L'RCONSAL,R2),RCONSAL                                          
                                                                                
         BAS   RE,BUMPNEXT                                                      
         DROP  R6                                                               
                                                                                
* DEMOS                                                                         
* DISPLAY FIRST PRIMARY DEMO ONLY.  IF NONE EXISTS, DISPLAY FIRST DEMO          
         L     R6,AIO                                                           
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISK120                                                          
                                                                                
         LA    RE,RSARXDEM                                                      
         LA    RF,6                                                             
                                                                                
DISK95   DS    0H                                                               
         TM    0(RE),X'40'         CHECK FOR PRIMARY DEMO                       
         BO    DISK100                                                          
         LA    RE,3(RE)                                                         
         BCT   RF,DISK95                                                        
                                                                                
         LA    RE,RSARXDEM         NO PRIMARY, JUST TAKE FIRST                  
                                                                                
DISK100  DS    0H                                                               
         LA    R5,WORK                                                          
         XC    WORK(30),WORK                                                    
         MVC   0(3,R5),0(RE)                                                    
                                                                                
         LA    R4,BLOCK                                                         
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
                                                                                
         CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
                                                                                
         LA    R5,WORK                                                          
                                                                                
         GOTO1 DEMOCON,DMCB,(1,(R5)),(9,8(R2)),(0,DBLOCKD)                      
                                                                                
DISK120  DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
         DROP  R4,R6                                                            
                                                                                
* LAST UPDATED DATE                                                             
DISK130  DS    0H                                                               
         L     R6,AIO                                                           
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISK140                                                          
         CLI   RSARXLEN,RSARXLTH                                                
         BL    DISK140                                                          
                                                                                
         CLI   RSARXLEN,RSARXLTH   ONLY NEW SAR ELEM HAS UPDATE DATE            
         BL    DISK140                                                          
         GOTO1 DATCON,DMCB,(3,RSARXLAD),(5,8(R2))                               
         DROP  R6                                                               
                                                                                
DISK140  DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
                                                                                
* COMMENT LINE 1                                                                
         L     R6,AIO                                                           
         USING RCONBCEL,R6                                                      
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISK150                                                          
                                                                                
         CLI   RCONBCLN,3                                                       
         BL    DISK150                                                          
         ZIC   R1,RCONBCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISK150  DS    0H                                                               
         OI    6(R2),X'80'+X'20'   PROTECT                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* COMMENT LINE 2                                                                
         BAS   RE,NEXTEL                                                        
         BNE   DISK160                                                          
                                                                                
         CLI   RCONBCLN,3                                                       
         BL    DISK160                                                          
         ZIC   R1,RCONBCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISK160  DS    0H                                                               
         OI    6(R2),X'80'+X'20'   PROTECT                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* COMMENT LINE 3                                                                
         BAS   RE,NEXTEL                                                        
         BNE   DISK170                                                          
                                                                                
         CLI   RCONBCLN,3                                                       
         BL    DISK170                                                          
         ZIC   R1,RCONBCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISK170  DS    0H                                                               
         OI    6(R2),X'80'+X'20'   PROTECT                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* COMMENT LINE 4                                                                
         BAS   RE,NEXTEL                                                        
         BNE   DISK180                                                          
                                                                                
         CLI   RCONBCLN,3                                                       
         BL    DISK180                                                          
         ZIC   R1,RCONBCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISK180  DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
         B     DISCONX                                                          
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY FOR FORECAST CONTRACTS                                                
***********************************************************************         
DIS4CAST DS    0H                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
* OFFICE                                                                        
         MVC   8(L'RCONKOFF,R2),RCONKOFF                                        
         BAS   RE,BUMPNEXT                                                      
         DROP  R6                                                               
                                                                                
* MARKET BUDGET                                                                 
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISF05                                                           
         OC    RSARXBGT,RSARXBGT     ANY BUDGET                                 
         BZ    DISF03                                                           
         EDIT  (B4,RSARXBGT),(8,8(R2)),ALIGN=LEFT                               
         B     DISF05                                                           
                                                                                
DISF03   DS    0H                  BUDGET IS ZERO                               
         CLI   RSARXLEN,RSARXLTH   IS THIS AN EXPANDED SAR ELMENT?              
         BL    DISF05                                                           
         TM    RSARXFLG,X'40'      IF BUDGET ENTERED FLAG ON                    
         BZ    DISF05                PRINT 0 DOLLARS                            
         MVI   8(R2),C'0'                                                       
                                                                                
DISF05   DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
         DROP  R6                                                               
                                                                                
* SHARE GOAL                                                                    
         L     R6,AIO                                                           
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISF08                                                           
         CLI   RSARXLEN,RSARXLTH   ONLY EXPANDED SAR HAS SHARE GOAL             
         BL    DISF08                                                           
         OC    RSARXSHG,RSARXSHG                                                
         BZ    DISF07                                                           
         EDIT  RSARXSHG,(3,8(R2)),ALIGN=LEFT                                    
         B     DISF08                                                           
                                                                                
DISF07   DS    0H                  SHARE GOAL IS ZERO                           
         TM    RSARXFLG,X'80'      WAS IT ENTERED?                              
         BZ    DISF08                                                           
         MVI   8(R2),C'0'          YES, PRINT 0                                 
                                                                                
DISF08   DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
         DROP  R6                                                               
*                                                                               
* STATION BUDGET = MARKET BUDGET * SHARE GOAL                                   
*                                                                               
         L     R6,AIO                                                           
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISF90                                                           
         CLI   RSARXLEN,RSARXLTH   ONLY EXPANDED SAR HAS SHARE GOAL             
         BL    DISF90                                                           
                                                                                
         MVC   FULL,RSARXBGT       GET MARKET BUDGET                            
         L     RF,FULL             IF ZERO, SEE IF ENTERED AS ZERO              
         LTR   RF,RF                                                            
         BZ    DISF80                                                           
                                                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,FULL                                                       
         XC    WORK,WORK                                                        
         MVC   WORK+1(1),RSARXSHG                                               
         MH    RF,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   WORK+3,100                                                       
         D     RE,WORK                                                          
         LR    R4,RF                                                            
         EDIT  (R4),(8,8(R2)),ALIGN=LEFT                                        
         B     DISF90                                                           
                                                                                
DISF80   DS    0H                                                               
         TM    RSARXFLG,X'40'      BUDGET ENTERED AS ZERO?                      
         BNO   DISF90              NO                                           
         MVI   8(R2),C'0'          YES - SEND BACK A ZERO                       
                                                                                
DISF90   DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
         DROP  R6                                                               
                                                                                
* ADVERTISER                                                                    
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         GOTO1 ADVNAME,DMCB,RCONKADV                                            
         MVC   8(L'PDPADV,R2),WORK                                              
         BAS   RE,BUMPNEXT                                                      
                                                                                
* AGENCY                                                                        
         GOTO1 AGYNAME,DMCB,RCONKAGY                                            
         MVC   8(L'PDPAGY,R2),WORK                                              
         BAS   RE,BUMPNEXT                                                      
                                                                                
* LAST UPDATED DATE                                                             
         CLI   TWAOFFC,C'*'        DISPLAY K# FOR DDS TERMINALS                 
         BNE   DISF130                                                          
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,8(R2)),ALIGN=LEFT                                   
         B     DISF140                                                          
         DROP  R6                                                               
                                                                                
DISF130  DS    0H                                                               
         L     R6,AIO                                                           
         USING RSARXEL,R6                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISF140                                                          
         CLI   RSARXLEN,RSARXLTH                                                
         BL    DISF140                                                          
                                                                                
         CLI   RSARXLEN,RSARXLTH   ONLY NEW SAR ELEM HAS UPDATE DATE            
         BL    DISF140                                                          
         GOTO1 DATCON,DMCB,(3,RSARXLAD),(5,8(R2))                               
         DROP  R6                                                               
                                                                                
DISF140  DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
                                                                                
* FLIGHT                                                                        
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,8(R2))                               
         MVI   16(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,17(R2))                            
         BAS   RE,BUMPNEXT                                                      
                                                                                
* COMMENT LINE 1                                                                
         L     R6,AIO                                                           
         USING RCONBCEL,R6                                                      
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISF150                                                          
                                                                                
         CLI   RCONBCLN,3                                                       
         BL    DISF150                                                          
         ZIC   R1,RCONBCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISF150  DS    0H                                                               
         OI    6(R2),X'80'+X'20'   PROTECT                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* COMMENT LINE 2                                                                
         BAS   RE,NEXTEL                                                        
         BNE   DISF160                                                          
                                                                                
         CLI   RCONBCLN,3                                                       
         BL    DISF160                                                          
         ZIC   R1,RCONBCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISF160  DS    0H                                                               
         OI    6(R2),X'80'+X'20'   PROTECT                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* COMMENT LINE 3                                                                
         BAS   RE,NEXTEL                                                        
         BNE   DISF170                                                          
                                                                                
         CLI   RCONBCLN,3                                                       
         BL    DISF170                                                          
         ZIC   R1,RCONBCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISF170  DS    0H                                                               
         OI    6(R2),X'80'+X'20'   PROTECT                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* COMMENT LINE 4                                                                
         BAS   RE,NEXTEL                                                        
         BNE   DISF180                                                          
                                                                                
         CLI   RCONBCLN,3                                                       
         BL    DISF180                                                          
         ZIC   R1,RCONBCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISF180  DS    0H                                                               
         BAS   RE,BUMPNEXT                                                      
                                                                                
DISCONX  DS    0H                                                               
         XIT1  REGS=(R2)                                                        
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
* BUMP TO NEXT DISPLAY FIELD                                                    
*******************************************************************             
BUMPNEXT DS    0H                                                               
         OI    6(R2),X'80'+X'20'   PROTECT THE FIELD                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*******************************************************************             
* GET ADVERTISER EXPANDED NAME                                                  
* P1 HAS ADV CODE                                                               
* WORK WILL CONTAIN EXPANDED NAME                                               
*******************************************************************             
ADVNAME  NTR1                                                                   
         MVC   SEQKEY,KEY                                                       
         XC    WORK,WORK                                                        
         L     R1,0(R1)                                                         
         MVC   WORK(L'RADVKADV),0(R1)                                           
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RADVKEY,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,0(R1)                                                   
         MVC   RADVKREP,AGENCY                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   ADVNAMX                                                          
                                                                                
         LA    RF,IO                                                            
         LA    RF,1000(RF)                                                      
         ST    RF,AIO                                                           
                                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RADVREC,R6                                                       
         MVC   WORK(L'RADVNAME),RADVNAME                                        
                                                                                
         LA    RF,IO                                                            
         ST    RF,AIO                                                           
         DROP  R6                                                               
                                                                                
ADVNAMX  DS    0H                                                               
         MVC   KEY,SEQKEY          RE-ESTABLISH SEQ                             
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
*  ROUTINE FINDS THE OFFICE2 RECORD FOR THE RECORD JUST FOUND                   
*  - AND RETRIEVES ITS NATIONAL/LOCAL FLAG                                      
*******************************************************************             
GETOFFLG NTR1                                                                   
         XC    BYTE,BYTE           CLEAR FLAG                                   
         MVC   SEQKEY,KEY                                                       
         XC    WORK,WORK                                                        
*                                                                               
         MVC   WORK(L'RCONSOFF),KEY+8                                           
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ROFF2KEY,R6                                                      
         MVI   ROFF2TYP,X'44'                                                   
         MVC   ROFF2REP,AGENCY                                                  
         MVC   ROFF2OFF,WORK                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'ROFF2KEY),KEYSAVE                                          
         BNE   GOFX                                                             
                                                                                
         LA    RF,IO                                                            
         LA    RF,1000(RF)                                                      
         ST    RF,AIO                                                           
                                                                                
         GOTO1 GETREC              AUTOMATICALLY GETS '10' ELEM                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        ELEM OF OFF2 REC                             
         BAS   RE,GETEL                                                         
         USING ROFF2FXE,R6                                                      
         MVC   BYTE,ROFF2PRF+1     SAVE NAT/LOC FLAG                            
                                                                                
         LA    RF,IO                                                            
         ST    RF,AIO                                                           
         DROP  R6                                                               
                                                                                
GOFX     DS    0H                                                               
         MVC   KEY,SEQKEY          RE-ESTABLISH SEQ                             
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* GET AGENCY EXPANDED NAME                                                      
* P1 HAS AGY CODE AND OFFICE                                                    
* WORK WILL CONTAIN EXPANDED NAME                                               
*******************************************************************             
AGYNAME  NTR1                                                                   
         MVC   SEQKEY,KEY                                                       
         XC    WORK,WORK                                                        
         L     R1,0(R1)                                                         
         MVC   WORK(L'RAGYKAGY+L'RAGYKAOF),0(R1)                                
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGYKEY,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY(L'RAGYKAGY+L'RAGYKAOF),0(R1)                            
         MVC   RAGYKREP,AGENCY                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   AGYNAMX                                                          
                                                                                
         LA    RF,IO                                                            
         LA    RF,1000(RF)                                                      
         ST    RF,AIO                                                           
                                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RAGYREC,R6                                                       
         MVC   WORK(L'RAGYNAM1),RAGYNAM1                                        
                                                                                
         LA    RF,IO                                                            
         ST    RF,AIO                                                           
         DROP  R6                                                               
                                                                                
AGYNAMX  DS    0H                                                               
         MVC   KEY,SEQKEY          RE-ESTABLISH SEQ                             
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* GET PRODUCT EXPANDED NAME                                                     
* P1 HAS ADV CODE                                                               
* P2 HAS PRD CODE                                                               
* WORK WILL CONTAIN EXPANDED NAME                                               
*******************************************************************             
PRDNAME  NTR1                                                                   
         MVC   SEQKEY,KEY                                                       
         XC    WORK,WORK                                                        
         L     RF,0(R1)                                                         
         MVC   ADVCODE,0(RF)                                                    
         L     RF,4(R1)                                                         
         MVC   WORK(L'RCONPRD),0(RF)                                            
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPRDKEY,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,ADVCODE                                                 
         MVC   RPRDKPRD,WORK                                                    
         MVC   RPRDKREP,AGENCY                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RPRDKEY),KEYSAVE                                           
         BNE   PRDNAMX                                                          
                                                                                
         LA    RF,IO                                                            
         LA    RF,1000(RF)                                                      
         ST    RF,AIO                                                           
                                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RPRDREC,R6                                                       
         MVC   WORK(L'RPRDNAME),RPRDNAME                                        
                                                                                
         LA    RF,IO                                                            
         ST    RF,AIO                                                           
         DROP  R6                                                               
                                                                                
PRDNAMX  DS    0H                                                               
         MVC   KEY,SEQKEY          RE-ESTABLISH SEQ                             
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* ERROR MESSAGES                                                                
*******************************************************************             
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
                                                                                
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
                                                                                
SLOCKOUT MVC   RERROR,=AL2(55)                                                  
         B     ERREND                                                           
                                                                                
INVLPER  MVC   RERROR,=AL2(441)    MMMYY(-MMMYY)                                
         B     ERREND                                                           
                                                                                
INVLDAT  MVC   RERROR,=AL2(INVDAT)                                              
         B     ERREND                                                           
                                                                                
NEXTLIST MVC   RERROR,=AL2(15)     PRESS ENTER FOR NEXT                         
         B     INFEND                                                           
                                                                                
ENDLIST  MVC   RERROR,=AL2(16)     END OF LIST                                  
         B     INFEND                                                           
                                                                                
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
                                                                                
INFEND   DS    0H                                                               
         LA    R2,PFHSTAH          PUT CURSOR HERE                              
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
                                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* LOCAL STORAGE AREA                                                            
*******************************************************************             
*    VALID DAYPARTS                                                             
DPTABLE  DS    0CL4                                                             
         DC    CL4'MNGM'            MORNING                                     
         DC    CL4'DAYD'            DAYTIME                                     
         DC    CL4'ELYE'            EARLY FRINGE                                
         DC    CL4'ENWR'            EARLY NEWS                                  
         DC    CL4'ACCA'            PRIME ACCESS                                
         DC    CL4'LNWT'            LATE NEWS                                   
         DC    CL4'LTEL'            LATE FRINGE                                 
         DC    CL4'WKDW'            WEEKEND                                     
         DC    CL4'KIDK'            KIDS                                        
         DC    CL4'FRGF'            FRINGE                                      
         DC    CL4'NWSN'            NEWS                                        
         DC    CL4'PRIP'            PRIME                                       
         DC    CL4'MOVV'            MOVIES                                      
         DC    CL4'SPES'            SPECIALS                                    
         DC    CL4'SPOJ'            SPORTS                                      
         DC    CL4'SPSO'            SOAPS                                       
         DC    CL4'COMU'            COMPETITIVE                                 
         DC    CL4'LOCX'            LOCAL                                       
         DC    X'FF'                                                            
                                                                                
* RATING SERVICE                                                                
RTGSTAB  DS    0CL3                                                             
         DC    CL3'ARB'                                                         
         DC    CL3'NSI'                                                         
         DC    CL3'BIR'            BIRCH                                        
         DC    CL3'SRC'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
RELO     DS    A                                                                
STA_CALL DS    CL(L'RCONKSTA)                                                   
STA_GRP  DS    CL(L'RCONKGRP)                                                   
SAL_OFF  DS    CL(L'RCONROFF)                                                   
SAL_TEAM DS    CL(L'RCONRTEM)                                                   
NEXTSCRN DS    C                   Y=USER PRESSED ENTER/DISP NEXT SCRN          
SARFOUND DS    C                   Y=RECORD HAS SAR ELEMENT                     
DPTFLAG  DS    C                   Y=RECORD HAS DAYPART                         
SEQKEY   DS    CL(L'KEY)                                                        
ADVCODE  DS    CL(L'RCONKADV)                                                   
CONTSTAT DS    X                                                                
CONTPEND EQU   X'10'               CONTRACT IS PENDING                          
CONTFORE EQU   X'20'               CONTRACT IS FORECAST                         
CLEARFLG DS    C                                                                
         EJECT                                                                  
*                                                                               
         ORG                                                                    
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE RESFMWTWA                                                      
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMDDD          (OUR HEADER LIST SCREEN OVERLAY)             
         ORG   PFHLAST                                                          
       ++INCLUDE RESFMB0D          (OUR PENDING LIST SCREEN OVERLAY)            
         ORG   PFHLAST                                                          
       ++INCLUDE RESFMBDD          (OUR FORECAST LIST SCREEN OVERLAY)           
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
CONTRACT DSECT                                                                  
       ++INCLUDE REGENCON                                                       
ADV      DSECT                                                                  
       ++INCLUDE REGENADV                                                       
AGY      DSECT                                                                  
       ++INCLUDE REGENAGY                                                       
PRD      DSECT                                                                  
       ++INCLUDE REGENPRD                                                       
OFF2     DSECT                                                                  
       ++INCLUDE REGENOFF2                                                      
SALESMAN DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
STATION  DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
SAVEKEY  DS    CL(L'KEY)           NEED FOR PAGING                              
FIRSTKEY DS    CL(L'KEY)           FOR WHEN WE LOOP AROUND AGAIN                
LASTSCRN DS    C                                                                
STARTDT  DS    CL6                 START DATE IN EBCDIC                         
ENDDT    DS    CL6                 END DATE IN EBCDIC                           
BSTARTDT DS    XL3                 START DATE OF START B'CAST MONTH             
BENDDT   DS    XL3                 END DATE OF END B'CAST MONTH                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099RESFM2DA  05/01/02'                                      
         END                                                                    
