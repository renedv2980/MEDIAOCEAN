*          DATA SET RESFM36    AT LEVEL 075 AS OF 05/01/02                      
*PHASE T81836A,*                                                                
         TITLE 'T81836 - PRODUCT RECORDS - REP TO SPOT TRANSFER'                
**********************************************************************          
*                                                                    *          
*        RESFM36 (T81836) --- PRODUCT RECORDS                        *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* 20FEB98  (ASTE) -- BIRTH DATE                                      *          
*                                                                    *          
**********************************************************************          
T81836   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1836**,R7,RR=R3                                              
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
         MVI   ACTELOPT,C'N'       DON'T ADD GENCON ACTIVITY ELEMENT            
         OI    GENSTAT4,NODELLST                                                
*        CLI   THISLSEL,C'S'       LIST SELECT?                                 
*        BNE   *+8                                                              
*        MVI   MODE,VALREC         GOTO CHANGE                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
*                                                                               
VKEY     DS    0H                                                               
         LA    R2,CONACTH                                                       
         CLI   ACTNUM,1            ACTION ADD?                                  
         BE    ERRINV                                                           
         CLI   ACTNUM,4            ACTION DELETE?                               
         BE    ERRINV                                                           
         CLI   ACTNUM,6            ACTION RESTORE?                              
         BE    ERRINV                                                           
*                                                                               
         XC    ADVRT,ADVRT                                                      
         XC    PRODUCT,PRODUCT                                                  
         XC    CONAG,CONAG       AGENCY FOR CONTRACT RECORD                     
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST RECORDS?                                
         BE    VK50                                                             
*                                                                               
* VALKEY FOR MAINT                                                              
         LA    R2,RSMAGYH                                                       
         CLI   5(R2),0             AGENCY REQUIRED FOR CONTRACT RECORDS         
         BE    ERRMIS                                                           
         MVC   CONAG,8(R2)                                                      
         OC    CONAG,SPACES                                                     
         BAS   RE,VALAGNC          VALIDATE AGENCY                              
*                                                                               
         LA    R2,RSMADVH                                                       
         CLI   5(R2),0             ADVERTISER REQUIRED                          
         BE    ERRMIS                                                           
         MVC   ADVRT,8(R2)                                                      
         OC    ADVRT,SPACES                                                     
         GOTO1 VALIADV             VALIDATE ADVERTISER                          
*                                                                               
         LA    R2,RSMPRDH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         MVC   PRODUCT,8(R2)                                                    
         OC    PRODUCT,SPACES                                                   
         B     VK100                                                            
*                                                                               
* VALKEY FOR LIST                                                               
VK50     DS    0H                                                               
         LA    R2,RSLAGYH                                                       
         CLI   5(R2),0             AGENCY REQUIRED FOR CONTRACT RECORDS         
         BE    ERRMIS                                                           
         MVC   CONAG,8(R2)                                                      
         OC    CONAG,SPACES                                                     
         BAS   RE,VALAGNC          VALIDATE AGENCY                              
*                                                                               
         LA    R2,RSLADVH                                                       
         CLI   5(R2),0             ADVERTISER REQUIRED                          
         BE    ERRMIS                                                           
         MVC   ADVRT,8(R2)                                                      
         OC    ADVRT,SPACES                                                     
         GOTO1 VALIADV             VALIDATE ADVERTISER                          
*                                                                               
         LA    R2,RSLPRDH                                                       
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         MVC   PRODUCT,8(R2)                                                    
         OC    PRODUCT,SPACES                                                   
*                                                                               
VK100    DS    0H                                                               
         XC    KEY,KEY             BUILD PRODUCT RECORD KEY                     
         LA    R6,KEY                                                           
         USING RPRDREC,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,ADVRT                                                   
         MVC   RPRDKPRD,PRODUCT                                                 
         MVC   RPRDKREP,AGENCY                                                  
         MVC   SAVEKEY,KEY                                                      
VKX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
*                                                                               
VREC     DS    0H                                                               
         XC    CONCNT,CONCNT       CLEAR COUNTERS                               
         XC    BUYCNT,BUYCNT                                                    
         MVI   DONFLG,C'N'         RESET DONE FLAG TO NO                        
*                                                                               
         LA    R2,RSMSPTH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'NO'                                                   
         BE    VRXIT                                                            
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'YES'                                                  
         BNE   ERRINV                                                           
*                                                                               
         XC    KEY,KEY             CLEAR KEY TO GET CONTRACT RECS               
         LA    R6,KEY                                                           
         USING RCONREC,R6                                                       
         MVC   RCONPCTP(2),=X'AB01'                                             
         MVC   RCONPCRP,AGENCY                                                  
         MVC   RCONPCAG,CONAG                                                   
         MVC   RCONPCAD,ADVRT                                                   
         MVC   RCONPCPC,PRODUCT                                                 
         MVC   SVKEY,KEY                                                        
*                                                                               
         GOTO1 HIGH                                                             
         B     VR10                                                             
VR04     XC    KEY,KEY                                                          
         MVC   KEY,SVKEY           RESTORE CONTRACT KEY                         
         LA    R6,KEY                                                           
         GOTO1 HIGH                                                             
VR05     GOTO1 SEQ                                                              
VR10     CLC   KEY(23),SVKEY       BREAK ON CHANGE IN REP,AGY,ADV,PRD           
         BNE   VR100                                                            
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVC   CONTR,RCONPCCN      GET CONTRACT #                               
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),CONTR(4)                                              
                                                                                
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         XC    KEY,KEY             CLEAR KEY TO GET BUY RECS                    
         LA    R6,KEY                                                           
         USING RBUYREC,R6                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,AGENCY                                                  
         MVC   RBUYKCON,WORK+32    CONTRACT#, 9'S COMPL. REVERS                 
         MVC   SVKEY2,KEY                                                       
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(22),SVKEY2      SAME REP AND CONTRACT #                      
         BNE   VR04                GET NEXT CONTRACT                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'        IS SPOTPAK INTERFACE ELEM PRESENT?           
         BAS   RE,GETEL                                                         
         BNE   VR15                NO, GET NEXT BUY                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
         BNE   VR15                NO, GET NEXT BUY                             
*                                                                               
* INCREMENT CONTRACT COUNT                                                      
         L     R1,CONCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,CONCNT                                                        
         B     VR25                                                             
*                                                                               
VR15     GOTO1 SEQ                                                              
         CLC   KEY(22),SVKEY2      SAME REP AND CONTRACT #                      
         BNE   VR04                GET NEXT CONTRACT                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'        IS SPOTPAK INTERFACE ELEM PRESENT?           
         BAS   RE,GETEL                                                         
         BNE   VR15                NO, GET NEXT BUY                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
         BNE   VR15                NO, GET NEXT BUY                             
*                                                                               
         USING RBUYELEM,R6                                                      
VR25     XI    RBUYRTS,X'80'       FLIP REP-TO-SPOT TRANSFER BIT                
         GOTO1 PUTREC                                                           
*                                                                               
* INCREMENT BUY COUNT                                                           
         L     R1,BUYCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,BUYCNT                                                        
         B     VR15                GET NEXT BUY                                 
*                                                                               
VR100    DS    0H                                                               
         MVC   RSMSPT,=C'DONE'       'DONE'                                     
         MVI   RSMSPTH+5,4                                                      
         OI    RSMSPTH+6,X'20'     MAKE PROTECTED                               
         OI    RSMSPTH+6,X'80'                                                  
         OI    GENSTAT2,RETEQSEL   RET TO LIST AFTER ADDITONAL ENTER            
         MVI   DONFLG,C'Y'         SET FLAG TO DONE                             
*                                                                               
VRXIT    B     DREC                                                             
         DROP  R6                                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
*                                                                               
DKEY     DS    0H                                                               
         L     R6,AIO                                                           
         USING RPRDREC,R6                                                       
*                                                                               
         MVC   RSMAGY,CONAG                                                     
         OI    RSMAGYH+6,X'80'                                                  
*                                                                               
         MVC   ADVRT,RPRDKADV                                                   
         OC    ADVRT,SPACES                                                     
         MVC   RSMADV,ADVRT                                                     
         OI    RSMAGYH+6,X'80'                                                  
*                                                                               
         MVC   PRODUCT,RPRDKPRD                                                 
         OC    PRODUCT,SPACES                                                   
         MVC   RSMPRD,PRODUCT                                                   
         OI    RSMPRDH+6,X'80'                                                  
*                                                                               
         MVC   SAVEKEY,0(R6)                                                    
         MVC   SAVEKEY+25,AGENCY    RESTORE REP CODE                            
         XC    MYLSKEY,MYLSKEY                                                  
         MVC   MYLSKEY,0(R6)                                                    
         MVC   MYLSKEY+25,AGENCY    RESTORE REP CODE                            
*                                                                               
DKXX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
*                                                                               
DREC     DS    0H                                                               
         CLI   DONFLG,C'Y'         IS TRANSFER DONE?                            
         BE    DR90                YES, DON'T CLEAR FIELDS OR FLAG              
*                                                                               
         XC    CONCNT,CONCNT       CLEAR COUNTERS                               
         XC    BUYCNT,BUYCNT                                                    
*                                                                               
* CLEAR TRANSFER FIELD                                                          
         LA    R2,RSMSPTH                                                       
         MVC   8(4,R2),0                                                        
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
DR90     DS    0H                                                               
         EDIT  CONCNT,RSMCON,ALIGN=LEFT,ZERO=NOBLANK                            
         EDIT  BUYCNT,RSMBUY,ALIGN=LEFT,ZERO=NOBLANK                            
         OI    RSMCONH+6,X'80'                                                  
         OI    RSMBUYH+6,X'80'                                                  
*                                                                               
DR100    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   DONFLG,C'Y'         IS TRANSFER DONE?                            
*        CLC   RSMSPT,=C'DONE'     DID WE CHANGE RECORD?                        
         BNE   DRXIT               NO, EXTI - YES, PRINT MSG                    
*                                                                               
         MVI   DONFLG,C'N'         RESET TO NO                                  
         LA    R2,CONHEADH                                                      
         MVC   RERROR,=H'94'       XTER MESSAGE                                 
         MVI   RMSGTYPE,C'I'       INFORMATION MSG TYPE                         
         GOTO1 MYERROR                                                          
DRXIT    B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*                        LIST AND PRINT ROUTINE                *                
****************************************************************                
****************************************************************                
*                                                                               
LIST     DS    0H                                                               
         OC    KEY,KEY             FIRST TIME?                                  
         BNZ   LR10                                                             
         MVC   KEY,SAVEKEY                                                      
         XC    MYLSKEY,MYLSKEY                                                  
*                                                                               
LR10     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RPRDKEY,R6                                                       
         CLC   RPRDKREP,AGENCY         SAME AGY SIGNON?                         
         BE    LR12                                                             
*        MVC   RPRDKREP,AGENCY     NO, BECAUSE OF LISTMON!!!                    
*        MVC   RPRDKADV,ADVRT      NO, BECAUSE OF LISTMON!!!                    
         MVC   KEY,MYLSKEY         DIFNO, BECAUSE OF LISTMON!!!                 
LR12     GOTO1 HIGH                                                             
         B     LR20                                                             
LR15     GOTO1 SEQ                                                              
LR20     CLC   KEY(22),KEYSAVE     SAME ADVERTISER?                             
         BNE   LRXIT               NO DONE                                      
*                                                                               
         LA    R6,KEY                                                           
         USING RPRDKEY,R6                                                       
         CLC   RPRDKREP,AGENCY         SAME AGY SIGNON?                         
         BNE   LR15                NO, CHECK NEXT RECORD                        
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LPRD,RPRDKPRD       PRINT PRD CODE                               
         MVC   MYLSKEY,KEY                                                      
*                                                                               
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'01'        GET 01 ELEM FOR PRD NAME                     
         BAS   RE,GETEL                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
         BNE   LR15                CHECK NEXT RECORD * IF LIVE!                 
*                                                                               
         USING RPRDELEM,R6                                                      
         MVC   LPRDNAM,RPRDNAME                                                 
         GOTO1 LISTMON                                                          
         B     LR15                                                             
*                                                                               
LRXIT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
ERRMIS   MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
ERRINV   MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
*                                                                               
* THIS ROUTINE CLEARS OUT A FIELD                                               
* ON ENTRY, R2 POINTS TO FIELD HEADER                                           
*&&DO                                                                           
         SPACE 1                                                                
CLEAR    DS    0H                                                               
         ZIC   R1,0(R2)            GET LENGTH OF FIELD                          
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'         EXTENDED HEADER                              
         BZ    CL10                                                             
         SH    R1,=H'8'                                                         
         SPACE 1                                                                
CL10     EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         BR    RE                                                               
         EJECT                                                                  
*&&                                                                             
*                                                                               
* THIS ROUTINE SEARCHES FOR A VALID AGENCY RECORD THAT MATCHES THE              
* USER'S INPUT FOR AGENCY                                                       
VALAGNC  NTR1                                                                   
*        BUILD KEY                                                              
         XC    KEY,KEY             BUILD AGENCY RECORD KEY                      
         LA    R6,KEY                                                           
         USING RAGYREC,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY,CONAG                                                   
         MVC   RAGYKAOF,SPACES     MOVE IN OFFICE                               
         MVC   RAGYKREP,AGENCY                                                  
*                                                                               
         GOTO1 READ                                                             
VAGX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMFAD                                                                      
* RESFMEAD                                                                      
* RESFMWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMB6D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMB7D                                                       
         EJECT                                                                  
       ++INCLUDE REGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE REGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
SAVEKEY  DS    CL27                                                             
MYLSKEY  DS    CL27                                                             
SVKEY    DS    CL27                FOR INTERVENING GETRECS                      
SVKEY2   DS    CL27                FOR INTERVENING GETRECS                      
SVDMWORK DS    F                                                                
CONAG    DS    CL4                                                              
ADVRT    DS    CL4                                                              
PRODUCT  DS    CL3                                                              
CONCNT   DS    F                   NUMBER OF CONTRACTS TRANSFERRED              
BUYCNT   DS    F                   NUMBER OF BUYS TRANSFERRED                   
CONTR    DS    F                   COMTRACT #                                   
DONFLG   DS    C                   TRANSFER COMPLETE FLAG                       
         SPACE 4                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LPRD     DS    CL3                                                              
         DS    CL7                                                              
LPRDNAM  DS    CL20                                                             
         EJECT                                                                  
*DEMO DSECT INCLUDING DEDBLOCK                                                  
DEMOD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075RESFM36   05/01/02'                                      
         END                                                                    
