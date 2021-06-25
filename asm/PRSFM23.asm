*          DATA SET PRSFM23    AT LEVEL 047 AS OF 04/27/04                      
*PHASE T41C23A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*               T41C23 - PGEST MAINT/LIST                                       
*                                                                               
* KWAN 09/05/02 CONVERT PGEST FROM FIL TO SFM                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, REP               *         
*                                                                     *         
*  INPUTS       SCREEN T41CCB (MAINTENANCE)                           *         
*               SCREEN T41CCC (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED GROUP DEFINITION RECORDS                      *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - GROUP DEFINITION RECORD                         *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C23 - PGEST MAINT/LIST'                                      
*                                                                               
T41C23   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C23                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         BRAS  RE,INITIALZ         INITIALIZE WORKING STORAGES                  
*                                                                               
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTREST      ACTION RESTORE?                              
         JE    RECACERR                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
* USE FOLLOWING MODE WHEN PASSIVE POINTERS NEED TO BE ADDED/DELETED             
*                                                                               
* * * *  CLI   MODE,XRECADD        AFTER ADD                                    
* * * *  BE    DR                                                               
* * * *  CLI   MODE,XRECPUT        AFTER PUT                                    
* * * *  BE    DR                                                               
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
JUMPXIT1 XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                  VALIDATE KEY ROUTINE                         
         LA    R2,PGEMEDH          POINT TO MEDIA FLD                           
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTMEDH                                                       
         GOTO1 VALIMED             MEDIA IS REQUIRED EVEN FOR LIST              
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK10                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK10                                                             
         MVC   PGEMEDN,MEDNM                                                    
         OI    PGEMEDNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK10     XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PGSTKEY,R6                                                       
*                                                                               
         MVC   PGSTKAGY,AGENCY                                                  
         MVC   PGSTKMED,QMED                                                    
         MVI   PGSTKRCD,X'0A'      RECORD CODE FOR PGEST                        
*                                                                               
         XC    CLTLSFLT,CLTLSFLT   CLEAR CLT LIST FILTER                        
         LA    R2,PGECLTH          POINT TO CLIENT FLD                          
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTCLTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                YES                                          
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         JNE   MSSNGERR            FIELD IS REQUIRED                            
*                                                                               
         OC    TWAACCS(2),TWAACCS  ANY LIMIT ACCESS?                            
         BZ    VK30                NO                                           
         CLI   CONWHENH+5,0        NOW, SOON, OR OV?                            
         BE    VK30                NO                                           
*                                                                               
         J     CLTRQERR            SECURITY - CLIENT REQUIRED                   
*                                                                               
VK20     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                FOR LIST, NO NEED TO VALICLT                 
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK20H               FOR REPORT, NO NEED TO VALICLT               
         CLI   5(R2),0             NO INPUT?                                    
         BE    VK30                                                             
         CLC   =C'ALL',8(R2)       ALL CLIENT?                                  
         BE    VK30                YES, LEAVE CLIENT FILTER NULLS               
         ZIC   RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CLTLSFLT(0),8(R2)   GET CLIENT CODE                              
         OC    CLTLSFLT,SPACES     SPACE PADDED                                 
         B     VK30                                                             
*                                                                               
VK20H    GOTO1 VALICLT                                                          
         MVC   PGSTKCLT,QCLT                                                    
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK30                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK30                                                             
         MVC   PGECLTN,CLTNM                                                    
         OI    PGECLTNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK30     LA    R2,PGEPRDH          POINT TO PRODUCT FLD                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTPRDH                                                       
         XC    PRDLSFLT,PRDLSFLT   CLEAR PRD LIST FILTER                        
         CLI   5(R2),0                                                          
         BNE   VK40                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK50                YES - OPTIONAL                               
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK50                YES - OPTIONAL                               
         J     MSSNGERR            FIELD IS REQUIRED                            
*                                                                               
VK40     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                FOR LIST, NO NEED TO VALIPRD                 
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK40H               FOR REPORT, NO NEED TO VALIPRD               
         CLC   =C'ALL',8(R2)       ALL PRODUCT?                                 
         BE    VK50                YES, LEAVE PRODUCT FILTER NULLS              
         ZIC   RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRDLSFLT(0),8(R2)   GET CLIENT CODE                              
         OC    PRDLSFLT,SPACES     SPACE PADDED                                 
         B     VK50                                                             
*                                                                               
VK40H    GOTO1 VALIPRD                                                          
         MVC   PGSTKPRD,QPRD                                                    
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK50                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK50                                                             
         MVC   PGEPRDN,PRDNM                                                    
         OI    PGEPRDNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK50     LA    R2,PGEESTH          VALIDATE ESTIMATE                            
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTESTH                                                       
         XC    ESTLSFLT,ESTLSFLT   CLEAR EST LIST FILTER                        
         CLI   5(R2),0                                                          
         BNE   VK60                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VKX                 YES - OPTIONAL                               
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VKX                 YES - OPTIONAL                               
         J     MSSNGERR            FIELD IS REQUIRED                            
*                                                                               
VK60     CLI   5(R2),3             MAX 3 DIGITS                                 
         JH    INVFDERR                                                         
         TM    4(R2),X'08'         NUMERIC?                                     
         JNO   INVFDERR                                                         
         BAS   RE,PACK             R0 WILL RETURN BINARY ESTIMATE               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                FOR LIST, NO NEED TO VALIEST                 
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK60B               FOR REPORT, NO NEED TO VALIEST               
         STCM  R0,3,ESTLSFLT       BINARY ESTIMATE                              
         B     VKX                                                              
*                                                                               
* PGEST - ALLOW ESTIMATE=0 IF PRODUCT IS NOT ZZZ                                
*                                                                               
VK60B    LTR   R0,R0               ESTIMATE IS ZERO?                            
         BNZ   VK60E               NO, GO VALIDATE ESTIMATE                     
         CLC   QPRD,=C'ZZZ'        PRD "ZZZ"?                                   
         JE    INVFDERR            CANNOT BE ZERO FOR "ZZZ"                     
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VKX                 YES, EST NAME IS NOT DISPLAYED               
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VKX                 YES, EST NAME IS NOT DISPLAYED               
         XC    PGEESTN,PGEESTN                                                  
         OI    PGEESTNH+6,X'80'    CLEAR ESTIMATE NAME AND TRANSMIT             
         J     VKX                                                              
*                                                                               
VK60E    GOTO1 VALIEST                                                          
         MVC   PGSTKEST,BEST       BINARY ESTIMATE                              
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VKX                                                              
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VKX                                                              
         MVC   PGEESTN,ESTNM                                                    
         OI    PGEESTNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         MVI   USEIONUM,1          RECORD WILL BE READ INTO AIO1                
         B     JUMPXIT1                                                         
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                  VALIDATE DATA FOR PROCTER $ GAMBLE           
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM             REMOVE X'10' ELEM FROM PGEST                 
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PGSTELEM,R6                                                      
*                                                                               
         LA    R2,PGECHRH          CHARGE PERIOD HEADER                         
         GOTO1 ANY                                                              
         TM    4(R2),X'08'         NUMERIC?                                     
         JZ    INVFDERR            NO                                           
         CLI   5(R2),L'PGECHR      EXACT LENGTH?                                
         JNE   INVFDERR                                                         
         MVI   PGSTEID,PGSTEIDQ    ELEM ID                                      
         MVI   PGSTELN,PGSTELNQ    ELEM LENGTH                                  
         MVI   PGSTESEQ,X'01'                                                   
         MVC   PGSTNAME,=C'CHRG PER'                                            
         MVC   PGSTDATA(L'PGECHR),PGECHR                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,PGEACCH          ACCOUNT                                      
         GOTO1 ANY                                                              
         CLI   5(R2),L'PGEACC                                                   
         JNE   INVFDERR                                                         
         MVI   PGSTESEQ,X'02'                                                   
         MVC   PGSTNAME,=C'ACCOUNT '                                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTDATA(L'PGEACC),PGEACC                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,PGEBRNH          BRAND                                        
         GOTO1 ANY                                                              
         CLI   5(R2),L'PGEBRN                                                   
         JNE   INVFDERR                                                         
         MVI   PGSTESEQ,X'03'                                                   
         MVC   PGSTNAME,=C'BRAND   '                                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTDATA(L'PGEBRN),PGEBRN                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,PGEPESH          ESTIMATE #                                   
         GOTO1 ANY                                                              
         CLI   5(R2),L'PGEPES                                                   
         JNE   INVFDERR                                                         
         MVI   PGSTESEQ,X'04'                                                   
         MVC   PGSTNAME,=C'ESTIMATE'                                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTDATA(L'PGEPES),PGEPES                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,PGEEVTH          EVENT CODE                                   
         GOTO1 ANY                                                              
         CLI   5(R2),L'PGEEVT                                                   
         JNE   INVFDERR                                                         
         MVI   PGSTESEQ,X'05'                                                   
         MVC   PGSTNAME,=C'EVENT CD'                                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTDATA(L'PGEEVT),PGEEVT                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,PGEMBFH          MULTI-BRAND FLAG (Y/N)                       
         CLC   PGSTKAGY,=C'H9'     "STARCOM" AGENCY ?                           
         BNE   VR30H               NO                                           
         CLC   PGSTKCLT,=C'PGB'    CLIENT PGB?                                  
         BE    VR30                YES                                          
         CLC   PGSTKCLT,=C'PG1'    CLIENT PG1?                                  
         BNE   VR30H               NOT PGB OR PG1                               
VR30     CLI   PGEMBF,C'Y'                                                      
         BE    VR30K               OK                                           
VR30H    CLI   PGEMBF,C'N'                                                      
         JNE   INVFDERR                                                         
VR30K    MVI   PGSTESEQ,X'06'                                                   
         MVC   PGSTNAME,=C'MLT-BRND'                                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTDATA(L'PGEMBF),PGEMBF                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,PGENOBH          NO BRAND FLAG (Y/N)                          
         CLC   PGSTKAGY,=C'H9'     "STARCOM" AGENCY ?                           
         BNE   VR40H               NO                                           
         CLC   PGSTKCLT,=C'PGB'    CLIENT PGB?                                  
         BE    VR40                YES                                          
         CLC   PGSTKCLT,=C'PG1'    CLIENT PG1?                                  
         BNE   VR40H               NOT PGB OR PG1                               
VR40     CLI   PGENOB,C'N'                                                      
         BE    VR40K               OK                                           
VR40H    CLI   PGENOB,C'Y'                                                      
         JNE   INVFDERR                                                         
VR40K    MVI   PGSTESEQ,X'07'                                                   
         MVC   PGSTNAME,=C'NOBRAND '                                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTDATA(L'PGENOB),PGENOB                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,PGEBRSH          BRAND SUFFIX                                 
         TM    4(R2),X'04'         ALPHABETIC?                                  
         JZ    INVFDERR            NO                                           
         CLI   5(R2),L'PGEBRS      EXACT LENGTH?                                
         JNE   INVFDERR                                                         
         MVI   PGSTESEQ,X'08'                                                   
         MVC   PGSTNAME,=C'BRND SUF'                                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTDATA(L'PGEBRS),PGEBRS                                        
         GOTO1 ADDELEM                                                          
*                                                                               
VRX      DS    0H                                                               
         B     JUMPXIT1                                                         
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                  DISPLAY RECORD                               
         L     R6,AIO                                                           
         CLI   3(R6),X'0A'         PGEST RECORD CODE?                           
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PGSTELEM,R6                                                      
         MVI   ELCODE,PGSTEIDQ     PGEST ELEM ID                                
*                                                                               
         LA    R2,PGECHRH                                                       
         BAS   RE,GETEL                                                         
         JNE   INVFDERR                                                         
         FOUT  PGECHRH,PGSTDATA,3  CHARGE PERIOD                                
*                                                                               
         LA    R2,PGEACCH                                                       
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         FOUT  PGEACCH,PGSTDATA,6  ACCOUNT                                      
*                                                                               
         LA    R2,PGEBRNH                                                       
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         FOUT  PGEBRNH,PGSTDATA,4  PG BRAND                                     
*                                                                               
         LA    R2,PGEPESH                                                       
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         FOUT  PGEPESH,PGSTDATA,4  ESTIMATE #                                   
*                                                                               
         LA    R2,PGEEVTH                                                       
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         FOUT  PGEEVTH,PGSTDATA,6  EVENT CODE                                   
*                                                                               
         LA    R2,PGEMBFH                                                       
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         FOUT  PGEMBFH,PGSTDATA,1  MULTI-BRAND FLAG                             
*                                                                               
         LA    R2,PGENOBH                                                       
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         FOUT  PGENOBH,PGSTDATA,1  NO BRAND FLAG                                
*                                                                               
         LA    R2,PGEBRSH                                                       
         BAS   RE,NEXTEL                                                        
         JNE   INVFDERR                                                         
         FOUT  PGEBRSH,PGSTDATA,2  BRAND SUFFIX                                 
*                                                                               
DRX      DS    0H                                                               
         B     JUMPXIT1                                                         
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                  DISPLAY KEY                                  
         L     R6,AIO              RECORD SELECTED                              
         CLI   3(R6),X'0A'         PGEST RECORD CODE?                           
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PGSTKEY,R6                                                       
         MVC   PGEMED,PGSTKMED                                                  
         MVC   PGECLT,PGSTKCLT                                                  
         MVC   PGEPRD,PGSTKPRD                                                  
         EDIT  (B2,PGSTKEST),(3,PGEEST),0,ALIGN=RIGHT,FILL=0                    
         DROP  R6                                                               
*                                                                               
         MVI   USEIONUM,2          USE AIO2 FOR MED/CLT/PRD/EST RECS            
*                                                                               
         OI    PGEMEDH+6,X'80'     TRANSMIT MEDIA CODE                          
         XC    PGEMEDN,PGEMEDN                                                  
         OI    PGEMEDNH+6,X'80'    CLEARED MEDIA NAME                           
         MVI   PGEMEDH+5,1         INPUT LENGTH                                 
         LA    R2,PGEMEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   PGEMEDN,MEDNM                                                    
         OI    PGEMEDNH+6,X'80'    TRANSMIT MEDIA NAME                          
*                                                                               
         OI    PGECLTH+6,X'80'     TRANSMIT CLIENT CODE                         
         XC    PGECLTN,PGECLTN                                                  
         OI    PGECLTNH+6,X'80'    CLEARED CLIENT NAME                          
         MVI   PGECLTH+5,3         INPUT LENGTH                                 
         LA    R2,PGECLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   PGECLTN,CLTNM                                                    
         OI    PGECLTNH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         OI    PGEPRDH+6,X'80'     TRANSMIT PRODUCT CODE                        
         XC    PGEPRDN,PGEPRDN                                                  
         OI    PGEPRDNH+6,X'80'    CLEARED PRODUCT NAME                         
         MVI   PGEPRDH+5,3         INPUT LENGTH                                 
         LA    R2,PGEPRDH                                                       
         GOTO1 VALIPRD                                                          
         MVC   PGEPRDN,PRDNM                                                    
         OI    PGEPRDNH+6,X'80'    TRANSMIT PRODUCT NAME                        
*                                                                               
         OI    PGEESTH+6,X'80'     TRANSMIT ESTIMATE CODE                       
         XC    PGEESTN,PGEESTN                                                  
         OI    PGEESTNH+6,X'80'    CLEARED ESTIMATE NAME                        
         MVI   PGEESTH+5,3         INPUT LENGTH                                 
         OI    PGEESTH+4,X'08'     INPUT IS NUMERIC                             
         LA    R2,PGEESTH                                                       
         BAS   RE,PACK                                                          
         LTR   R0,R0               ESTIMATE IS ZERO?                            
         BZ    DKX                 YS, NO NEED TO VALIDATE ESTIMATE             
         GOTO1 VALIEST                                                          
         MVC   PGEESTN,ESTNM                                                    
         OI    PGEESTNH+6,X'80'    TRANSMIT ESTIMATE NAME                       
*                                                                               
DKX      DS    0H                                                               
         MVC   AIO,AIO1            AIO1 HAS REC TO BE DISPLAYED                 
         B     JUMPXIT1                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR       DS    0H                  LIST RECORDS                                 
         OC    KEY,KEY                                                          
         BNZ   LR20                CONTINUE LISTING                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGSTKEY,R4                                                       
*                                                                               
         MVC   KEY(25),SVKEY       KEY BUILT FROM VK                            
*                                                                               
LR20     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     LR40                                                             
*                                                                               
LR30     GOTO1 SEQ                 NEXT RECORD                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR40     CLC   KEY(4),KEYSAVE      SAME AGY/MED/RECORD CODE?                    
         BNE   LRX                                                              
*                                                                               
         CLC   KEY+04(3),CLTLSFLT  CLT IS HIGHER THAN FILTER?                   
         BL    LR30                                                             
         CLC   KEY+07(3),PRDLSFLT  PRD IS HIGHER THAN FILTER?                   
         BL    LR30                                                             
         CLC   KEY+10(2),ESTLSFLT  EST IS HIGHER THAN FILTER?                   
         BL    LR30                                                             
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         MVC   SVWORK(L'KEY),KEY   RESTORING SEQUENCE LATER                     
         MVI   USEIONUM,2          USE AIO2 FOR CLT/PRD/EST RECORDS             
*                                                                               
         XC    KEY+07(18),KEY+07   PREPARE CLIENT REC KEY                       
         MVI   KEY+03,X'02'        CLIENT RECORD CODE                           
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     CLIENT ON FILE?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+04(3),SVWORK+04 SAME CLIENT AS IN SAVED KEY?                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         CLI   33(R6),X'02'        FIRST CLIENT REC ELEM PRESENT?               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LSCLT+00(03),KEY+04 PUT CLT CODE TO SCR                          
         MVI   LSCLT+03,C'/'                                                    
         MVC   LSCLT+04(16),35(R6) PUT CLT NAME TO SCR (1ST 16 CHARS)           
*                                                                               
         MVC   KEY,SVWORK                                                       
         XC    KEY+10(15),KEY+10   PREPARE PRODUCT REC KEY                      
         MVI   KEY+03,X'06'        PRODUCT RECORD CODE                          
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     PRODUCT ON FILE?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+07(3),SVWORK+07 SAME PRODUCT AS IN SAVED KEY?                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         CLI   33(R6),X'06'        FIRST PRODUCT REC ELEM PRESENT?              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LSPRD+00(03),KEY+07 PUT PRD CODE TO SCR                          
         MVI   LSPRD+03,C'/'                                                    
         MVC   LSPRD+04(16),35(R6) PUT PRD NAME TO SCR (1ST 16 CHARS)           
*                                                                               
* ZERO ESTIMATE IS ALLOWED IN PGEST, BUT NOT IN ESTIMATE                        
*                                                                               
         OC    SVWORK+10(2),SVWORK+10                                           
         BNZ   LR50                                                             
         MVC   LSEST+00(3),=C'000'                                              
         MVI   LSEST+03,C'/'                                                    
         B     LR80                                                             
*                                                                               
LR50     MVC   KEY,SVWORK                                                       
         XC    KEY+12(13),KEY+12   PREPARE ESTIMATE REC KEY                     
         MVI   KEY+03,X'07'        ESTIMATE RECORD CODE                         
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     ESTIMATE ON FILE?                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+12(2),SVWORK+12 SAME ESTIMATE AS IN SAVED KEY?               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         CLI   33(R6),X'07'        FIRST ESTIMATE REC ELEM PRESENT?             
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (B2,KEY+10),(3,LSEST),0,ALIGN=RIGHT,FILL=0                       
         MVI   LSEST+03,C'/'                                                    
         MVC   LSEST+04(16),35(R6) PUT EST NAME TO SCR (1ST 16 CHARS)           
*                                                                               
LR80     MVC   KEY,SVWORK                                                       
         GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(25),KEYSAVE     SAME RECORD GOTTEN BACK?                     
         BE    *+6                                                              
         DC    H'0'                SEQUENCE IS NOT PROPERLY RESTORED            
         MVC   DMDSKADD,KEY+27     DISK ADDRESS FOR LISTMON                     
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
*                                                                               
         CLI   MODE,PRINTREP       REPORTING USING LIST ACTION?                 
         BNE   LR30                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         CLI   33(R6),PGSTEIDQ     PGEST ELEMENT PRESENT?                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    P1,P1               CLEAR PRINT LINE                             
         BRAS  RE,GETREPLN         GET REPORT LINE                              
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
*                                                                               
         B     LR30                GET NEXT RECORD                              
*                                                                               
LRX      DS    0H                                                               
         B     JUMPXIT1                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       DS    0H                  PRINT RECORDS                                
         OC    KEY,KEY                                                          
         BNZ   PR20                CONTINUE LISTING                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGSTKEY,R4                                                       
*                                                                               
         MVC   KEY(25),SVKEY       KEY BUILT FROM VK                            
*                                                                               
PR20     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PR40                                                             
*                                                                               
PR30     GOTO1 SEQ                 NEXT RECORD                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PR40     CLC   KEY(4),KEYSAVE      SAME AGY/MED/RECORD CODE?                    
         BNE   PRX                                                              
*                                                                               
         CLC   KEY+04(3),CLTLSFLT  CLT IS HIGHER THAN FILTER?                   
         BL    PR30                                                             
         CLC   KEY+07(3),PRDLSFLT  PRD IS HIGHER THAN FILTER?                   
         BL    PR30                                                             
         CLC   KEY+10(2),ESTLSFLT  EST IS HIGHER THAN FILTER?                   
         BL    PR30                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         CLI   33(R6),PGSTEIDQ     PGEST ELEMENT PRESENT?                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    P1,P1               CLEAR PRINT LINE                             
         BRAS  RE,GETREPLN         GET REPORT LINE                              
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
         B     PR30                GET NEXT RECORD                              
*                                                                               
PRX      DS    0H                                                               
         B     JUMPXIT1                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                OR NON NUMERIC                               
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
MSSNGERR MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
*                                                                               
INVFDERR MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
*                                                                               
RECACERR MVI   ERROR,INVRCACT      INVALID RECORD ACTION ERROR                  
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
CLTRQERR MVI   ERROR,85            SPECIFIC CLT REQUIRED (SECURITY)             
         J     TRAPERR                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITIALZ NTR1  BASE=*,LABEL=*      INITIALIZE WORKING STORAGES                  
*                                                                               
         OI    GENSTAT4,NODELLST   NO DELETE ON LIST                            
*                                                                               
         LA    RE,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    RE,SPECS                                                         
         LA    RE,HOOK                                                          
         ST    RE,HEADHOOK                                                      
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(01),QMED                                                   
*                                                                               
         J     JUMPXIT1                                                         
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    J     JUMPXIT1                                                         
*                                                                               
HEDSPECS SSPEC H2,01,C'MEDIA'                                                   
         SSPEC H1,56,C' PGEST REPORT  '                                         
         SSPEC H2,56,C'-------------- '                                         
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H6,01,C'                CHARGE             PG      PG'           
         SSPEC H7,01,C'CLT  PRD  EST   PERIOD   ACCOUNT   BRAND   EST'          
         SSPEC H8,01,C'---  ---  ---   ------   -------   -----   ----'         
         SSPEC H6,51,C'EVENT    MULTI   NO      BRAND'                          
         SSPEC H7,51,C'CODE     BRAND   BRAND   SUFFIX'                         
         SSPEC H8,51,C'------   -----   -----   ------'                         
         DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREPLN NTR1  BASE=*,LABEL=*      R6 POINTS TO RECORD                          
*                                                                               
         LA    R5,P1                                                            
         USING REP_LINE,R5                                                      
*                                                                               
         MVC   R_CLT,04(R6)                                                     
         MVC   R_PRD,07(R6)                                                     
         EDIT  (B2,10(R6)),(3,R_EST),0,ALIGN=RIGHT,FILL=0                       
*                                                                               
         USING PGSTELEM,R6                                                      
         MVI   ELCODE,PGSTEIDQ     PGEST ELEM ID                                
*                                                                               
         BRAS  RE,GETEL                                                         
         JNE   *+10                                                             
         MVC   R_CHGRPD,PGSTDATA   CHARGE PERIOD                                
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+10                                                             
         MVC   R_ACCT,PGSTDATA     ACCOUNT                                      
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+10                                                             
         MVC   R_PGBRND,PGSTDATA   PG BRAND                                     
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+10                                                             
         MVC   R_PGEST,PGSTDATA    ESTIMATE #                                   
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+10                                                             
         MVC   R_EVENTC,PGSTDATA   EVENT CODE                                   
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+10                                                             
         MVC   R_MULTIB,PGSTDATA   MULTI-BRAND FLAG                             
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+10                                                             
         MVC   R_NOBRND,PGSTDATA   NO BRAND FLAG                                
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   *+10                                                             
         MVC   R_BRNDSF,PGSTDATA   BRAND SUFFIX                                 
*                                                                               
         J     JUMPXIT1                                                         
*                                                                               
         DROP  R6,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMCBD          PGEST MAINT SCREEN                           
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMCCD          PGEST LIST SCREEN                            
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGESTREC          DSECT FOR BOTH PGEST/GFEST                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND            FIELD INDICATOR EQUATES                      
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
CLTLSFLT DS    CL3                 CLT LIST FILTER                              
PRDLSFLT DS    CL3                 PRD LIST FILTER                              
ESTLSFLT DS    XL2                 EST LIST FILTER                              
SVWORK   DS    XL64                GENERAL WORKING STORAGE                      
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSCLT    DS    CL20                CLT CODE AND 16 CHARS NAME                   
         DS    CL5                                                              
LSPRD    DS    CL20                PRD CODE AND 16 CHARS NAME                   
         DS    CL5                                                              
LSEST    DS    CL20                EST CODE AND 16 CHARS NAME                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REP_LINE DSECT                     REPORT DSECT FOR PRINTING LINES              
*                                                                               
R_CLT    DS    CL3                 CLIENT CODE                                  
         DS    XL2                                                              
R_PRD    DS    CL3                 PRODUCT CODE                                 
         DS    XL2                                                              
R_EST    DS    CL3                 ESTIMATE NUMBER                              
         DS    XL3                                                              
R_CHGRPD DS    CL3                 CHARGE PERIOD                                
         DS    XL6                                                              
R_ACCT   DS    CL6                 ACCOUNT                                      
         DS    XL4                                                              
R_PGBRND DS    CL4                 PG BRAND                                     
         DS    XL4                                                              
R_PGEST  DS    CL4                 PG EST                                       
         DS    XL3                                                              
R_EVENTC DS    CL6                 EVENT CODE                                   
         DS    XL3                                                              
R_MULTIB DS    CL1                 MULTI-BRAND                                  
         DS    XL7                                                              
R_NOBRND DS    CL1                 NO BRAND                                     
         DS    XL7                                                              
R_BRNDSF DS    CL2                 BRAND SUFFIX                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047PRSFM23   04/27/04'                                      
         END                                                                    
