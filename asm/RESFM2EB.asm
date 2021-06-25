*          DATA SET RESFM2EB   AT LEVEL 135 AS OF 01/12/00                      
*PHASE T8182EB,*                                                                
         TITLE 'T8182E - RESFM2E - PENDING CONTRACT UPDATE'                     
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM2E (T8182E) --- PENDING CONTRACT UPDATE             *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* JUL13/93 (SKU) --- BORN TO RUN                                  *             
*                                                                 *             
* AUG24/93 (SKU) --- ADD SIGN-ON RESTRICTION                      *             
*                                                                 *             
* SEP16/93 (SKU) --- FIX BUG OF NOT TURNING OFF READ DELETE       *             
*                                                                 *             
* SEP23/93 (SKU) --- USE HELEN INSTEAD OF ADDELEM SO SAR COMMENTS *             
*                    WON'T GET SORTED                             *             
*                                                                 *             
* FEB24/94 (SKU) --- ADD PENDING/FORECAST FILTER AND HOTKEY TO    *             
*                    TO THE CONTRACT PROGRAM                      *             
*                                                                 *             
* MAY02/94 (SKU) --- CHANGE FORECASTING FILTER SUPPORT            *             
*                                                                 *             
* AUG08/94 (SKU) --- ADD DATE FILTER                              *             
*                                                                 *             
* NOV30/94 (SKU) --- ALTERNATE LIST ON OFFICE + SALESPERSON       *             
*                                                                 *             
* DEC07/94 (SKU) --- CHANGE DATE FILTER TO BROADCAST MONTHS       *             
*                                                                 *             
* JAN18/95 (SKU) --- ENABLE NUMERIC DATE FILTER ENTRIES           *             
*                                                                 *             
* MAR13/96 (JRD) --- PROTECT COMMENTS                             *             
*                                                                 *             
* NOV07/96 (SKU) --- SKIP X'04' INVOICE ELEMENT CHECK FOR PENDING *             
*                    CHECK IF ACCOUNTING CONTRACT                 *             
*                                                                 *             
* MAR17/98 (SKU) --- PUT BACK X'04' INVOICE ELEMENT CHECK FOR     *             
*                    PENDING CONTRACTS                            *             
* JAN10/00 (MLB) --- PUT A CHECK FOR GARBAGE IN ELEMENT X'11'     *             
*                    CAUSED BY STERIO                             *             
*TOMB**************************************************************             
*                                                                               
T8182E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**182E**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         MVC   MYSCRNUM,TWASCR                                                  
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
                                                                                
         OC    PFAID,PFAID         IF A PFKEY WAS PRESSED                       
         BZ    MAIN10              EXIT AND JUMP TO CONTRACT PROG               
         CLI   PFAID,12            EXCEPT IF PFKEY HIT IS PF12                  
         BNE   EXIT                THEN REFRESH DISPLAY                         
                                                                                
MAIN10   DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         UPDATE RECORDS                               
         BE    UPDATE                                                           
                                                                                
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
                                                                                
         TM    PDCSTAH+4,X'20'     IF NO FIELDS WERE CHANGED                    
         BZ    VKEY010               USER WANTS TO PAGE TO NEXT SET OF          
         TM    PDCOFFH+4,X'20'       CONTRACTS                                  
         BZ    VKEY010                                                          
         TM    PDCSALH+4,X'20'                                                  
         BZ    VKEY010                                                          
         TM    PDCFILH+4,X'20'                                                  
         BZ    VKEY010                                                          
         TM    PDCDATEH+4,X'20'                                                 
         BZ    VKEY010                                                          
                                                                                
         TM    PDCSTAH+4,X'80'     IF FIELDS WERE CHANGED BUT CHANGED           
         BO    VKEY010             TO THE SAME DATA, USER WANTS TO              
         TM    PDCOFFH+4,X'80'     UPDT FROM THE BEGINNING                      
         BO    VKEY010                                                          
         TM    PDCSALH+4,X'80'                                                  
         BO    VKEY010                                                          
         TM    PDCFILH+4,X'80'                                                  
         BO    VKEY010                                                          
         TM    PDCDATEH+4,X'80'                                                 
         BO    VKEY010                                                          
                                                                                
         MVI   NEXTSCRN,C'Y'                                                    
         B     VKEYX                                                            
                                                                                
VKEY010  DS    0H                                                               
         OI    PDCSTAH+4,X'20'     SET VALIDATED                                
         OI    PDCOFFH+4,X'20'     SET VALIDATED                                
         OI    PDCSALH+4,X'20'     SET VALIDATED                                
         OI    PDCFILH+4,X'20'     SET VALIDATED                                
         OI    PDCDATEH+4,X'20'    SET VALIDATED                                
                                                                                
         XC    FIRSTKEY,FIRSTKEY                                                
         MVC   PDCSPST,=C'Sal'     DEFAULT SALESPERSON COLUMN                   
                                                                                
         XC    KEY_ON,KEY_ON                                                    
         LA    R2,PDCSTAH                                                       
         CLI   5(R2),0                                                          
         BNE   VKEY013                                                          
         CLI   PDCSALH+5,0         KEYING ON SALESPERSON ONLY?                  
         BE    MISSFLD                                                          
         LA    R2,PDCOFFH          DON'T SPECIFY OFFICE IF FILTERING            
         CLI   5(R2),0             ON SALESPERSON                               
         BNE   INVLFLD                                                          
         OI    KEY_ON,KO_OSAL      KEY ON ONLY SALESPERSON                      
         MVC   PDCSPST,=C'Sta'     SHOW STATION COLUMN                          
         B     VKEY050                                                          
                                                                                
VKEY013  DS    0H                                                               
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
         LA    R2,PDCOFFH          NEED EITHER THE OFFICE OR THE                
         CLI   5(R2),0             SALESPERSON                                  
         BNE   VKEY030                                                          
                                                                                
         LA    R2,PDCSALH                                                       
         CLI   5(R2),0                                                          
         BE    VKEY060                                                          
         B     VKEY050                                                          
                                                                                
VKEY030  DS    0H                  OFFICE IS PRESENT                            
         CLI   PDCSALH+5,0                                                      
         BNE   INVLFLD                                                          
         GOTO1 VALIOFF                                                          
         OI    KEY_ON,KO_OFF       WE'RE KEYING ON OFFICE                       
         B     VKEY060                                                          
                                                                                
VKEY050  DS    0H                  SALERSPERSON IS PRESENT                      
         LA    R2,PDCSALH                                                       
         GOTO1 VSAL                                                             
         OI    KEY_ON,KO_SAL       WE'RE KEYING ON SALESPERSON                  
                                                                                
VKEY060  DS    0H                  FILTER IS OPTIONAL                           
         XC    PDCPFKY,PDCPFKY                                                  
         MVC   PDCPFKY(8),=C'Pending:'                                          
         MVI   PENDFILT,C'P'       SAVE SO WE KNOW IF WE SHOULD PC/FC           
         LA    R2,PDCFILH          WHEN WE PFKEY TO CONTRACT                    
         CLI   PDCFILH+5,0                                                      
         BE    VKEY080                                                          
         CLI   PDCFIL,C'F'                                                      
         BNE   VKEY070                                                          
         MVI   PENDFILT,C'F'                                                    
         MVC   PDCPFKY,=C'Forecast:'                                            
         B     VKEY080                                                          
                                                                                
VKEY070  DS    0H                                                               
         CLI   PDCFIL,C'P'                                                      
         BNE   INVLFLD                                                          
                                                                                
VKEY080  DS    0H                  DATE FILTER IS OPTIONAL                      
         LA    R2,PDCDATEH                                                      
         CLI   5(R2),0                                                          
         BE    VKEYX                                                            
         GOTO1 VALIPERI                                                         
                                                                                
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
* UPDT RECORDS                                                                  
*******************************************************************             
UPDATE   DS    0H                                                               
         OC    FIRSTKEY,FIRSTKEY   FIRST TIME, DISPLAY CONTRACTS                
         BNZ   UPDT03                                                           
         GOTO1 CLEARSCN                                                         
         B     UPDT05                                                           
                                                                                
UPDT03   DS    0H                                                               
         BAS   RE,CHKCMT                                                        
                                                                                
UPDT05   DS    0H                  IF PF12, USER WANTS TO REFRESH SCN           
         CLI   PFAID,12                                                         
         BNE   UPDT06                                                           
         MVC   SAVEKEY,FRESHKEY                                                 
                                                                                
UPDT06   DS    0H                                                               
         MVI   CLEARFLG,C'N'       SCREEN HASN'T BEEN CLEARED YET               
                                                                                
         LA    R2,PDCNUMH          UPDT STARTS AT THIS FIELD                    
         LA    R3,DA_K1            SAVE DISK ADDRESSES                          
         XC    DA_K1(12),DA_K1                                                  
                                                                                
         CLI   NEXTSCRN,C'Y'       USER JUST PRESSED ENTER/PF12?                
         BNE   UPDT30              W/O CHANGING THE KEY FIELDS                  
         MVC   KEY,SAVEKEY         YES, DISPLAY NEXT PAGE OF CONTRACTS          
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         OC    KEY_ON,KEY_ON       DEFAULT STATION                              
         BNZ   UPDT08                                                           
         CLC   KEY(RCONKOFF-RCONKTYP),KEYSAVE                                   
         BNE   UPDT260                                                          
         B     UPDT20                                                           
                                                                                
UPDT08   DS    0H                                                               
         TM    KEY_ON,KO_OFF       KEY ON OFFICE                                
         BZ    UPDT10                                                           
         CLC   KEY(RCONKAGY-RCONKTYP),KEYSAVE                                   
         BNE   UPDT260                                                          
         B     UPDT20                                                           
                                                                                
UPDT10   DS    0H                                                               
         TM    KEY_ON,KO_OSAL      KEY ON ONLY SALESPERSON                      
         BZ    UPDT15                                                           
         CLC   KEY(RCONRSTA-RCONRTYP),KEYSAVE                                   
         BNE   UPDT260                                                          
         B     UPDT20                                                           
                                                                                
UPDT15   DS    0H                                                               
         TM    KEY_ON,KO_SAL       KEY ON SALESPERSON                           
         BZ    UPDT20                                                           
         CLC   KEY(RCONRAGY-RCONRTYP),KEYSAVE                                   
         BNE   UPDT260                                                          
                                                                                
UPDT20   DS    0H                                                               
         CLI   PFAID,12            IF REFRESH, GET RECORD NOW                   
         BE    UPDT100                                                          
         CLI   LASTSCRN,C'Y'       IF LAST TIME WAS THE LAST SCREEN             
         BNE   UPDT220             DON'T DO A SEQ                               
         MVI   LASTSCRN,C'N'       GET THE RECORD FOR THIS KEY NOW              
         B     UPDT100                                                          
                                                                                
UPDT30   DS    0H                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
         XC    KEY,KEY                                                          
                                                                                
         OC    KEY_ON,KEY_ON       DEFAULT STATION                              
         BNZ   UPDT40                                                           
         MVI   RCONKTYP,X'0C'                                                   
         MVC   RCONKREP,AGENCY                                                  
         MVC   RCONKGRP,STA_GRP                                                 
         MVC   RCONKSTA,STA_CALL                                                
         GOTO1 HIGH                                                             
         CLC   KEY(RCONKOFF-RCONKTYP),KEYSAVE                                   
         BNE   UPDT260                                                          
         MVC   FIRSTKEY,KEY                                                     
         B     UPDT100                                                          
                                                                                
UPDT40   DS    0H                                                               
         TM    KEY_ON,KO_OFF       KEY ON OFFICE                                
         BZ    UPDT50                                                           
         MVI   RCONKTYP,X'0C'                                                   
         MVC   RCONKREP,AGENCY                                                  
         MVC   RCONKGRP,STA_GRP                                                 
         MVC   RCONKSTA,STA_CALL                                                
         MVC   RCONKOFF,PDCOFF                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(RCONKAGY-RCONKTYP),KEYSAVE                                   
         BNE   UPDT260                                                          
         MVC   FIRSTKEY,KEY                                                     
         B     UPDT100                                                          
                                                                                
UPDT50   DS    0H                                                               
         TM    KEY_ON,KO_SAL       KEY ON SALESPERSON                           
         BZ    UPDTX                                                            
         MVI   RCONRTYP,X'AC'                                                   
         MVC   RCONRREP,AGENCY                                                  
         MVC   RCONROFF,SAL_OFF                                                 
         MVC   RCONRTEM,SAL_TEAM                                                
                                                                                
         CLI   PDCSALH+5,3                                                      
         BL    UPDT55                                                           
         MVC   RCONRSAL(1),PDCSAL+2     LAST                                    
         MVC   RCONRSAL+1(1),PDCSAL     FIRST                                   
         MVC   RCONRSAL+2(1),PDCSAL+1   MIDDLE                                  
         B     UPDT58                                                           
                                                                                
UPDT55   DS    0H                                                               
         MVC   RCONRSAL(1),PDCSAL+1     LAST                                    
         MVC   RCONRSAL+1(1),PDCSAL     FIRST                                   
                                                                                
UPDT58   DS    0H                                                               
         OC    RCONRSAL,SPACES                                                  
                                                                                
         MVC   RCONRSTA,STA_CALL                                                
         GOTO1 HIGH                                                             
                                                                                
         TM    KEY_ON,KO_OSAL                                                   
         BO    UPDT60                                                           
                                                                                
         CLC   KEY(RCONRAGY-RCONRTYP),KEYSAVE                                   
         BNE   UPDT260                                                          
         MVC   FIRSTKEY,KEY                                                     
         B     UPDT100                                                          
                                                                                
UPDT60   DS    0H                  KEY ON ONLY SALESPERSON                      
         CLC   KEY(RCONRSTA-RCONRTYP),KEYSAVE                                   
         BNE   UPDT260                                                          
         MVC   FIRSTKEY,KEY                                                     
         DROP  R6                                                               
                                                                                
UPDT100  DS    0H                                                               
         OI    DMINBTS,X'08'       READ DELETED                                 
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08' RESET                                        
                                                                                
         L     R6,AIO              INCASE RECORD IS DELETED BUT THE             
         USING RCONREC,R6          PASSIVE POINTERS AREN'T                      
         TM    RCONCNTL,X'80'      SKIP IF RECORD IS REALLY MARKED              
         BO    UPDT220             FOR DELETION                                 
*                                                                               
* CHECK IF PENDING/FORECAST CONTRACT                                            
*                                                                               
UPDT110  DS    0H                                                               
         XC    CONTSTAT,CONTSTAT   CLEAR FLAGS                                  
                                                                                
         CLI   PDCFIL,C'F'         FILTER ON FORECAST                           
         BE    UPDT140                                                          
                                                                                
*                                                                               
* IF FILTER IS BLANK OR P, DEFAULT TO PENDING                                   
*                                                                               
UPDT120  DS    0H                                                               
         TM    RCONMODR,X'10'      IS THIS A PENDING CONTRACT?                  
         BO    UPDT220                                                          
                                                                                
         L     R6,AIO              YES, BUT IS IT A FORECAST CONTRACT?          
         MVI   ELCODE,X'12'        EXPANPDED SAR ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   UPDT130                                                          
                                                                                
         USING RSARXEL,R6                                                       
         CLI   RSARXLEN,RSARXLTH   ONLY NEW SAR ELEM HAS FORECAST FLAG          
         BL    UPDT130                                                          
         TM    RSARXFLG,X'10'      FLAGGED AS FORECAST?                         
         BO    UPDT150                                                          
         DROP  R6                                                               
                                                                                
UPDT130  DS    0H                                                               
         OI    CONTSTAT,CONTPEND   FLAG AS PENDING CONTRACT                     
         B     UPDT150                                                          
                                                                                
UPDT140  DS    0H                  FILTER ON FORECAST                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'        EXPANPDED SAR ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   UPDT150                                                          
                                                                                
         USING RSARXEL,R6                                                       
         CLI   RSARXLEN,RSARXLTH   ONLY NEW SAR ELEM HAS FORECAST FLAG          
         BL    UPDT150                                                          
         TM    RSARXFLG,X'10'      FLAGGED AS FORECAST?                         
         BZ    UPDT150                                                          
         OI    CONTSTAT,CONTFORE   FLAG AS FORECAST                             
         DROP  R6                                                               
                                                                                
UPDT150  DS    0H                                                               
         TM    CONTSTAT,CONTPEND+CONTFORE                                       
         BZ    UPDT220                                                          
                                                                                
         L     R6,AIO              PENDING/FORECAST K HAS                       
         USING RCONREC,R6                                                       
         CLC   =C'ACC-',RCONBUYR   ACCOUNTING CONTRACT                          
         BE    UPDT220                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,3            NO ESTIMATE BUCKET                           
         BAS   RE,GETEL                                                         
         BE    UPDT220                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,4            NO INVOICE BUCKET                            
         BAS   RE,GETEL                                                         
         BE    UPDT220                                                          
*                                                                               
         L     R6,AIO              NO SPL/EPL DATA                              
         MVI   ELCODE,6                                                         
         BAS   RE,GETEL                                                         
         BE    UPDT220                                                          
                                                                                
UPDT160  DS    0H                                                               
         CLI   PDCDATEH+5,0        FLIGHT DATE FILTER                           
         BE    UPDT170                                                          
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLC   BSTARTDT,RCONDATE+3                                              
         BH    UPDT220                                                          
         CLC   BENDDT,RCONDATE                                                  
         BL    UPDT220                                                          
         DROP  R6                                                               
                                                                                
UPDT170 DS     0H                                                               
* IF FILTERING ONLY ON SALESPERSON                                              
* IF STATION SIGN-ON, CHECK IF IT'S A VALID SIGN ON ID                          
         TM    KEY_ON,KO_OSAL                                                   
         BZ    UPDT200                                                          
         CLI   TWAACCS,C'$'                                                     
         BNE   UPDT200                                                          
         NI    CONTSTAT,X'FF'-CONTXACC                                          
         MVC   AIO,AIO2                                                         
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAKEY,R6                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,SVKEY+(RCONRSTA-RCONRTYP)                               
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RSTASOEL,R6                                                      
         MVI   ELCODE,6            GET VALID SIGN ON ID ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   UPDT190                                                          
                                                                                
UPDT180  DS    0H                                                               
         CLC   RSTASID,TWAORIG     VALID SIGN-ON?                               
         BE    UPDT190             YES, PROCEED                                 
         BAS   RE,NEXTEL           NOPE, CHECK NEXT ELEMENT                     
         BE    UPDT180                                                          
         XC    FIRSTKEY,FIRSTKEY   ALL DONE, NO MATCH, GET NEXT KEY             
         OI    CONTSTAT,CONTXACC   NO ACCESS                                    
         DROP  R6                                                               
                                                                                
UPDT190  DS    0H                  RESTORE ORIGINAL CONTRACT KEY/REC            
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         OI    DMINBTS,X'08'       READ DELETED                                 
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08' RESET                                        
         TM    CONTSTAT,CONTXACC                                                
         BO    UPDT220                                                          
                                                                                
UPDT200  DS    0H                                                               
         MVC   0(4,R3),KEY+28      SAVE OFF DISK ADDRESS                        
         LA    R3,4(R3)                                                         
                                                                                
         CLI   CLEARFLG,C'Y'       WAS SCREEN ALREADY CLEARED                   
         BE    UPDT210                                                          
         GOTO1 CLEARSCN                                                         
         MVI   CLEARFLG,C'Y'                                                    
                                                                                
UPDT210  DS    0H                                                               
         BAS   RE,DISCON           DISPLAY THE CONTRACT                         
         LA    RF,PDCFINH          STOP IF WE'RE AT END OF SCREEN               
         CR    R2,RF                                                            
         BNL   UPDTX                                                            
                                                                                
UPDT220  DS    0H                                                               
         GOTO1 SEQ                                                              
                                                                                
         OC    KEY_ON,KEY_ON       DEFAULT STATION                              
         BNZ   UPDT230                                                          
         CLC   KEY(RCONKOFF-RCONKTYP),KEYSAVE                                   
         BNE   UPDT260                                                          
         B     UPDT100                                                          
                                                                                
UPDT230  DS    0H                                                               
         TM    KEY_ON,KO_OFF       KEY ON OFFICE                                
         BZ    UPDT240                                                          
         CLC   KEY(RCONKAGY-RCONKTYP),KEYSAVE                                   
         BNE   UPDT260                                                          
         B     UPDT100                                                          
                                                                                
UPDT240  DS    0H                  KEY ON ONLY SALESPERSON                      
         TM    KEY_ON,KO_OSAL                                                   
         BZ    UPDT250                                                          
         CLC   KEY(RCONRSTA-RCONRTYP),KEYSAVE                                   
         BNE   UPDT260                                                          
         B     UPDT100             GO CHECK IF STATION VALID SIGNON             
                                                                                
UPDT250  DS    0H                  KEY ON SALESPERSON                           
         CLC   KEY(RCONRAGY-RCONRTYP),KEYSAVE                                   
         BE    UPDT100                                                          
                                                                                
UPDT260  DS    0H                  WE'VE HIT THE LAST CONTRACT                  
         MVI   LASTSCRN,C'Y'                                                    
         MVC   SAVEKEY,FIRSTKEY    NEXT SCREEN GOES TO THE BEGINNNING           
         MVC   PDCLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         LA    R2,PDCSTAH          PUT CURSOR HERE                              
         B     ENDUPDT                                                          
                                                                                
UPDTX    DS    0H                                                               
         MVI   LASTSCRN,C'N'                                                    
         MVC   SAVEKEY,KEY                                                      
         MVC   PDCLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     NEXTUPDT                                                         
         EJECT                                                                  
*******************************************************************             
* DISPLAY CONTRACT INFORMATION                                                  
* R2 HAS STARTING FIELD HEADER                                                  
* AIO HAS ADDRESS OF CONTRACT RECORD                                            
*******************************************************************             
DISCON   NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
         LA    RF,PDCNUMH          SAVE OFF CONTRACT NUMBERS IN CASE            
         CR    R2,RF               WE HOTKEY TO THE CONTRACT PROGRAM            
         BNE   DISK10                                                           
         MVC   PENDREC1,RCONKCON                                                
         MVC   FRESHKEY,KEY        SAVE INCASE WE NEED TO REFRESH SCN           
         B     DISK30                                                           
                                                                                
DISK10   DS    0H                                                               
         LA    RF,PDCNM2H                                                       
         CR    R2,RF                                                            
         BNE   DISK20                                                           
         MVC   PENDREC2,RCONKCON                                                
         B     DISK30                                                           
                                                                                
DISK20   DS    0H                                                               
         MVC   PENDREC3,RCONKCON                                                
                                                                                
DISK30   DS    0H                                                               
* P/BLANK = PENDING, F = FORECAST                                               
         TM    CONTSTAT,CONTPEND+CONTFORE                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
* CONTRACT NUMBER                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,8(R2)),ALIGN=LEFT                                   
         OI    6(R2),X'80'         XMIT FIELD                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* FLIGHT                                                                        
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,8(R2))                               
         MVI   16(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,17(R2))                            
         OI    6(R2),X'80'         XMIT FIELD                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* AGENCY                                                                        
         MVC   8(4,R2),RCONKAGY                                                 
         CLC   RCONKAOF,SPACES                                                  
         BE    DISK60                                                           
                                                                                
         LR    R3,R2               SQUASH FOR 2-3 CHAR AGENCY CODE              
         LA    R3,10(R3)                                                        
         CLI   0(R3),C' '                                                       
         BE    DISK50                                                           
         LA    R3,1(R3)                                                         
         CLI   0(R3),C' '                                                       
         BE    DISK50                                                           
         LA    R3,1(R3)                                                         
                                                                                
DISK50   DS    0H                                                               
         MVI   0(R3),C'-'                                                       
         MVC   1(2,R3),RCONKAOF                                                 
                                                                                
DISK60   DS    0H                                                               
         OI    6(R2),X'80'         XMIT FIELD                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* ADVERTISER                                                                    
         GOTO1 ADVNAME,DMCB,RCONKADV                                            
         MVC   8(L'PDCADV,R2),WORK                                              
         OI    6(R2),X'80'         XMIT FIELD                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* SALESPERSON                                                                   
         TM    KEY_ON,KO_OSAL                                                   
         BO    DISK65                                                           
         MVC   8(3,R2),RCONSAL                                                  
         B     DISK68                                                           
                                                                                
* SHOW STATION IF KEYING ON SALESPERSON                                         
DISK65   DS    0H                                                               
         MVC   8(4,R2),RCONKSTA                                                 
         CLI   RCONKSTA+4,C' '                                                  
         BE    DISK68                                                           
         MVI   12(R2),C'-'                                                      
         MVC   13(1,R2),RCONKSTA+4                                              
         CLI   RCONKSTA+3,C' '                                                  
         BNE   DISK68                                                           
         MVC   11(2,R2),12(R2)                                                  
         MVI   13(R2),C' '                                                      
                                                                                
DISK68   DS    0H                                                               
         OI    6(R2),X'80'         XMIT FIELD                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         DROP  R6                                                               
                                                                                
* LAST UPDATED DATE                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISK70                                                           
         USING RSARXEL,R6                                                       
         CLI   RSARXLEN,RSARXLTH   ONLY NEW SAR ELEM HAS UPDATE DATE            
         BL    DISK70                                                           
         GOTO1 DATCON,DMCB,(3,RSARXLAD),(5,8(R2))                               
         DROP  R6                                                               
                                                                                
DISK70   DS    0H                                                               
         OI    6(R2),X'80'         XMIT FIELD                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* COMMENT LINE 1                                                                
         NI    YESUPDT,X'FF'-X'20'                                              
         NI    1(R2),X'FF'-X'20'   DEFAULT UNPROTECT                            
*                                                                               
         OI    1(R2),X'20'         NOT ENOUGH COMMENTS                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    DISK80                                                           
                                                                                
         OI    YESUPDT,X'20'       NO SAR FOUND                                 
         MVC   8(40,R2),=C'* CANNOT BE UPDATED - MISSING SAR DATA *'            
         OI    1(R2),X'20'         MAKE PROTECT, SO USER CAN'T UPDATE           
         B     DISK90                                                           
                                                                                
DISK80   DS    0H                                                               
         L     R6,AIO                                                           
         USING RCONBCEL,R6                                                      
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISK90                                                           
                                                                                
         B     DISK82                                                           
*                                                                               
DISK81   BAS   RE,NEXTEL                                                        
         BNE   DISK90                                                           
DISK82   CLI   RCONBCLN,3                                                       
         BL    DISK90                                                           
         LA    R1,63               SET MAX LENGTH OF ELT                        
         CLI   RCONBCLN,63         COMMENT > MAX LEN?                           
         BH    DISK81              YES - USE OVERRIDE LENGTH                    
         ZIC   R1,RCONBCLN         NO  - USE ELEMENT LENGTH                     
DISK85   EQU   *                                                                
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISK90   DS    0H                                                               
         OI    4(R2),X'20'         SET VALIDATED                                
         OI    6(R2),X'80'         XMIT FIELD                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* COMMENT LINE 2                                                                
DISK91   BAS   RE,NEXTEL                                                        
         BNE   DISK100                                                          
                                                                                
         CLI   RCONBCLN,3                                                       
         BL    DISK100                                                          
         LA    R1,63               SET MAX LENGTH OF ELT                        
         CLI   RCONBCLN,63         COMMENT > MAX LEN?                           
         BH    DISK91              YES - USE OVERRIDE LENGTH                    
         ZIC   R1,RCONBCLN         NO  - USE ELEMENT LENGTH                     
DISK95   EQU   *                                                                
         ZIC   R1,RCONBCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISK100  DS    0H                                                               
         NI    1(R2),X'FF'-X'20'   DEFAULT UNPROTECT                            
** JRD   TM    YESUPDT,X'20'       NO SAR FOUND                                 
** JRD   BZ    DISK110                                                          
         OI    1(R2),X'20'         MAKE PROTECT, SO USER CAN'T UPDATE           
                                                                                
DISK110  DS    0H                                                               
         OI    4(R2),X'20'         SET VALIDATED                                
         OI    6(R2),X'80'         XMIT FIELD                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* COMMENT LINE 3                                                                
DISK115  BAS   RE,NEXTEL                                                        
         BNE   DISK120                                                          
                                                                                
         CLI   RCONBCLN,3                                                       
         BL    DISK120                                                          
*                                                                               
         LA    R1,63               SET MAX LENGTH OF ELT                        
         CLI   RCONBCLN,63         COMMENT > MAX LEN?                           
         BH    DISK115             YES - USE OVERRIDE LENGTH                    
*                                                                               
         ZIC   R1,RCONBCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISK120  DS    0H                                                               
         NI    1(R2),X'FF'-X'20'   DEFAULT UNPROTECT                            
** JRD   TM    YESUPDT,X'20'       NO SAR FOUND                                 
** JRD   BZ    DISK130                                                          
         OI    1(R2),X'20'         MAKE PROTECT, SO USER CAN'T UPDATE           
                                                                                
DISK130  DS    0H                                                               
         OI    4(R2),X'20'         SET VALIDATED                                
         OI    6(R2),X'80'         XMIT FIELD                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
* COMMENT LINE 4                                                                
DISK135  BAS   RE,NEXTEL                                                        
         BNE   DISK140                                                          
                                                                                
         CLI   RCONBCLN,3                                                       
         BL    DISK140                                                          
         LA    R1,63               SET MAX LENGTH OF ELT                        
         CLI   RCONBCLN,63         COMMENT > MAX LEN?                           
         BH    DISK135             YES - USE OVERRIDE LENGTH                    
*                                                                               
         ZIC   R1,RCONBCLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RCONBCOM                                                 
                                                                                
DISK140  DS    0H                                                               
         NI    1(R2),X'FF'-X'20'   DEFAULT UNPROTECT                            
** JRD   TM    YESUPDT,X'20'       NO SAR FOUND                                 
** JRD   BZ    DISK150                                                          
         OI    1(R2),X'20'         MAKE PROTECT, SO USER CAN'T UPDATE           
                                                                                
DISK150  DS    0H                                                               
         OI    4(R2),X'20'         SET VALIDATED                                
         OI    6(R2),X'80'         XMIT FIELD                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
DISCONX  DS    0H                                                               
         XIT1  REGS=(R2)                                                        
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
* VALIDATE SALESPERSON                                                          
*******************************************************************             
VSAL     NTR1                                                                   
         LA    R6,KEY                                                           
         USING RSALKEY,R6                                                       
         XC    KEY,KEY                                                          
                                                                                
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,PDCSAL                                                  
         OC    RSALKSAL,SPACES     SPACE PAD                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(L'RSALKEY),KEYSAVE                                           
         BNE   INVLFLD                                                          
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
         MVC   SAL_TEAM,RSALTEAM   TEAM CODE                                    
         MVC   SAL_OFF,RSALOFF     OFFICE CODE                                  
                                                                                
VSALX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
* CLEAR THE SCREEN                                                              
*******************************************************************             
CLEARSCN NTR1                                                                   
         XC    PENDREC1,PENDREC1   INIT REC NUMS. WE NEED THEM                  
         XC    PENDREC2,PENDREC2   WHEN WE HOTKEY TO THE CONTRACT               
         XC    PENDREC3,PENDREC3   PROGRAM                                      
                                                                                
         TWAXC PDCNUMH,PDCLUPH,PROT=Y                                           
         TWAXC PDCCM1H,PDCLU2H,PROT=Y                                           
         TWAXC PDCCT2H,PDCLU3H,PROT=Y                                           
         TWAXC PDCCT3H,PDCFINH,PROT=Y                                           
CLEARSX  B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* CHECK IF USER CHANGED THE COMMENTS                                            
* IF THE COMMENTS ARE CHANGED, UPDATE THE CONTRACT(S)                           
*******************************************************************             
CHKCMT   NTR1                                                                   
         XC    YESUPDT,YESUPDT                                                  
         LA    R3,COMMENTS         WE'LL BE CHECKING 3 SETS OF COMMENTS         
         LA    R5,DA_K1                                                         
                                                                                
CHKCMT10 DS    0H                                                               
         LA    R2,PDCNUMH          K COMMENTS IN FIRST CONTRACT                 
         ZICM  RF,0(R3),2                                                       
         AR    R2,RF                                                            
         LA    R4,4                                                             
                                                                                
CHKCMT20 DS    0H                                                               
         TM    4(R2),X'20'         WAS FIELD CHANGED?                           
         BO    CHKCMT30                                                         
         OI    4(R2),X'20'         SET TO VALID FOR NEXT TIME                   
         OC    0(4,R5),0(R5)       NO DISK ADDRESS, USER MADE CHANGE            
         BZ    CHKCMT30            TO A BLANK RECORD, SO DON'T UPDATE           
         ZICM  R6,0(R3),2                                                       
         OI    YESUPDT,X'80'+X'40' SET FLAG                                     
                                                                                
CHKCMT30 DS    0H                                                               
         ZIC   R0,0(R2)            IF NOT, KEEP CHECKING                        
         AR    R2,R0                                                            
         BCT   R4,CHKCMT20         FOR TOTAL OF 4 COMMENT LINES                 
                                                                                
         TM    YESUPDT,X'40'                                                    
         BZ    CHKCMT40                                                         
         GOTO1 UPDCONT,DMCB,(R5),(R6)         YES, UPDATE CONTRACT              
                                                                                
CHKCMT40 DS    0H                                                               
         NI    YESUPDT,X'FF'-X'40' RESET FOR NEXT CHECK                         
         LA    R5,4(R5)            NEXT DISK ADDRESSS                           
         LA    R3,L'COMMENTS(R3)                                                
         CLC   =X'0000',0(R3)                                                   
         BNE   CHKCMT10            NOW CHECK THE OTHER CONTRACT                 
                                                                                
         TM    YESUPDT,X'80'                                                    
         BO    CONTUPDT            NOTIFY USER                                  
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* UPDATE THE SAR COMMENTS AND PUT TODAY'S DATE IN LAST UPDATE FIELD             
*   OF THE CONTRACT(S)                                                          
* P1=DISK ADDRESS OF RECORD TO UPDATE                                           
* P2=OFFSET OF START OF COMMENT ON SCREEN TO ADD TO RECORD                      
*******************************************************************             
UPDCONT  NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         L     RF,0(R1)                                                         
         MVC   KEY+28(4),0(RF)     DISK ADDRESS                                 
                                                                                
         L     RF,4(R1)                                                         
         LA    R2,PDCNUMH                                                       
         AR    R2,RF               FIRST LINE OF COMMENT ON SCREEN              
         ST    R2,SAVER2           IF ERROR WE WANT TO POINT HERE               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         CLI   TWAOFFC,C'*'        IF DDS, BYPASS                               
         BE    UPDK20                                                           
         CLC   =C'O=',TWAACCS      TEST FOR OFFICE RESTRICTION                  
         BNE   UPDK20                                                           
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLC   RCONKOFF,TWAACCS+2                                               
         BNE   NOACCESS                                                         
         DROP  R6                                                               
                                                                                
UPDK20   DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'        SAR COMMENT ELEMENT                          
         GOTO1 REMELEM                                                          
                                                                                
         LA    R3,4                                                             
                                                                                
UPDK30   DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    UPDK40                                                           
                                                                                
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RCONBCEL,R6                                                      
                                                                                
         MVI   RCONBCCO,X'11'                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RCONBCOM(0),8(R2)   DATA TO ELEMENT                              
         AH    R1,=H'3'            ELEMENT LENGTH                               
         STC   R1,RCONBCLN                                                      
         DROP  R6                                                               
                                                                                
         L     R6,AIO                                                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R6),ELEM,=C'ADD=CODE'             
         CLI   DMCB+12,X'05'       REC TOO LONG                                 
         BE    RECFULL             RECORD FULL - CHANGE NOT PROCESSED           
                                                                                
UPDK40   DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT COMMENT LINE                            
         BCT   R3,UPDK30                                                        
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'        EXPANDED SAR ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    UPDK50                                                           
         L     R2,SAVER2                                                        
         B     NOSAR               SAR ELEMENT NOT FOUND                        
                                                                                
UPDK50   DS    0H                                                               
         USING RSARXEL,R6                                                       
         XC    ELEM,ELEM                                                        
         ZIC   R1,RSARXLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),RSARXEL                                                  
         DROP  R6                                                               
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'        EXPANDED SAR ELEMENT                         
         GOTO1 REMELEM                                                          
                                                                                
         LA    R6,ELEM             LAST ACTIVITY DATE SET TO TODAY              
         USING RSARXEL,R6                                                       
         MVI   RSARXLEN,RSARXLTH                                                
         GOTO1 DATCON,DMCB,(5,0),(3,RSARXLAD)                                   
         DROP  R6                                                               
                                                                                
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
                                                                                
UPDKX    B     EXIT                                                             
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
* ERROR MESSAGES                                                                
*******************************************************************             
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
                                                                                
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
                                                                                
INVLPER  MVC   RERROR,=AL2(441)                                                 
         B     ERREND                                                           
                                                                                
INVLDAT  MVC   RERROR,=AL2(INVDAT)                                              
         B     ERREND                                                           
                                                                                
NOSAR    MVC   RERROR,=AL2(391)    MISSING SAR DATA                             
         B     ERREND                                                           
                                                                                
NOACCESS MVC   RERROR,=AL2(282)    OFFICE RESTRICTION                           
         B     ERREND                                                           
                                                                                
SLOCKOUT MVC   RERROR,=AL2(55)     SECURITY LOCKOUT                             
         B     ERREND                                                           
                                                                                
RECFULL  MVC   RERROR,=AL2(339)    RECORD FULL, CHANGE NOT PROCESSED            
         B     ERREND                                                           
                                                                                
NEXTUPDT MVC   RERROR,=AL2(15)     PRESS ENTER FOR NEXT                         
         B     INFEND                                                           
                                                                                
ENDUPDT  MVC   RERROR,=AL2(16)     END OF UPDT                                  
         B     INFEND                                                           
                                                                                
CONTUPDT MVC   RERROR,=AL2(104)    CONTRACT UPDATED                             
         B     INFEND                                                           
                                                                                
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
                                                                                
INFEND   DS    0H                                                               
         LA    R2,PDCSTAH          PUT CURSOR HERE                              
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
                                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* LOCAL STORAGE AREA                                                            
*******************************************************************             
COMMENTS DS    0CL2                                                             
         DC    AL2(PDCCM1H-PDCNUMH)                                             
         DC    AL2(PDCCT2H-PDCNUMH)                                             
         DC    AL2(PDCCT3H-PDCNUMH)                                             
         DC    AL2(0)                                                           
                                                                                
RELO     DS    A                                                                
STA_CALL DS    CL(L'RCONKSTA)                                                   
STA_GRP  DS    CL(L'RCONKGRP)                                                   
SAL_OFF  DS    CL(L'RCONROFF)                                                   
SAL_TEAM DS    CL(L'RCONRTEM)                                                   
NEXTSCRN DS    C                   Y=USER PRESSED ENTER/DISP NEXT SCRN          
YESUPDT  DS    X                   X'80'=AT LEAST ONE K WAS UPDATED             
*                                  X'40'=SAR CMTS HAS CHGED FOR THIS K          
*                                  X'20'=NO SAR, PROTECT CMT FIELDS             
CLEARFLG DS    C                   FLAG TO CLEAR SCREEN                         
SVKEY    DS    CL(L'KEY)                                                        
SEQKEY   DS    CL(L'KEY)                                                        
SAVER2   DS    F                   POINTS TO 1ST CMT LINE OF EACH K             
CONTSTAT DS    X                                                                
CONTPEND EQU   X'10'               CONTRACT IS PENDING                          
CONTFORE EQU   X'20'               CONTRACT IS FORECAST                         
CONTXACC EQU   X'40'               ON ACCESS TO STATION                         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMDED          (OUR UPDATE SCREEN OVERLAY)                  
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
CONTRACT DSECT                                                                  
       ++INCLUDE REGENCON                                                       
SALESMAN DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
ADV      DSECT                                                                  
       ++INCLUDE REGENADV                                                       
STATION  DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
KEY_ON   DS    X                                                                
KO_OFF   EQU   X'01'               KEY ON ALSO OFFICE                           
KO_SAL   EQU   X'02'               KEY ON ALSO SALESPERSON                      
KO_OSAL  EQU   X'04'               KEY ON ONLY SALESPERSON                      
LASTSCRN DS    C                   Y=WE'VE JUST HIT THE LAST SCREEN             
SAVEKEY  DS    CL(L'KEY)           NEED FOR PAGING                              
FRESHKEY DS    CL(L'KEY)           NEED FOR REFRESH SCREEN                      
FIRSTKEY DS    CL(L'KEY)           FOR WHEN WE LOOP AROUND AGAIN                
DA_K1    DS    F                   DISK ADDRESS OF CONTRACT 1                   
DA_K2    DS    F                   DISK ADDRESS OF CONTRACT 2                   
DA_K3    DS    F                   DISK ADDRESS OF CONTRACT 3                   
STARTDT  DS    CL6                 START DATE IN EBCDIC                         
ENDDT    DS    CL6                 END DATE IN EBCDIC                           
BSTARTDT DS    XL3                 START DATE OF BROADCAST MONTH                
BENDDT   DS    XL3                 END DATE OF BROADCAST MONTH                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135RESFM2EB  01/12/00'                                      
         END                                                                    
