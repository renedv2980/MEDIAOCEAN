*          DATA SET SPCSO13    AT LEVEL 088 AS OF 05/01/02                      
*PHASE T21813A,*                                                                
         TITLE 'T21813 - CHILD SPOT WEEK MOVE'                                  
T21813   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21813                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MYOVNUM,X'03'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    MOVMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'03'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE AND DISPLAY REQUESTS                
         BE    VK                                                               
         DC    H'0'                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,MOVMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    MOVCLTH+4,X'DF'                                                  
         NI    MOVMKTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,MOVCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKMKT                                                            
         NI    MOVMKTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKMKT    LA    R2,MOVMKTH          VALIDATE MARKET FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKFEST                                                           
         GOTO1 ANY                                                              
         TM    4(R2),X'08'         IF NUMERIC IT'S A MARKET NUMBER              
         BZ    VKALL                                                            
         GOTO1 VALIMKT                                                          
         B     VKMKTX                                                           
*                                                                               
VKALL    CLC   8(3,R2),=C'ALL'     ELSE IF IT'S 'ALL' THEN MKTSTA = 0           
         BNE   VKSTA                                                            
         XC    BMKT,BMKT                                                        
         XC    BSTA,BSTA                                                        
         B     VKMKTX                                                           
*                                                                               
VKSTA    GOTO1 VALISTA             ELSE IT'S A STATION NAME                     
*                                                                               
VKMKTX   OI    4(R2),X'20'                                                      
*                                                                               
VKFEST   LA    R2,MOVFESTH         VALIDATE 'FROM' ESTIMATE FIELD               
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         MVC   SAVSTART,QSTART     SAVE START DATE FOR MATCH                    
         MVC   FROMEST,BMEST                                                    
*                                                                               
VKTEST   LA    R2,MOVTESTH         VALIDATE 'TO' ESTIMATE FIELD                 
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         CLC   SAVSTART,QSTART     START DATE MUST MATCH 'FROM' EST             
         BNE   ERREST                                                           
         MVC   TOEST,BMEST                                                      
*                                                                               
VKFWK    LA    R2,MOVFWKH          VALIDATE 'FROM' WEEK FIELD                   
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),THISDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BNZ   INVERR                                                           
         GOTO1 DATVAL,DMCB,(1,8(R2)),THISDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
         BAS   RE,VALDATE                                                       
         GOTO1 DATCON,DMCB,(0,THISDATE),(2,FROMDATE)                            
*                                                                               
VKTWK    LA    R2,MOVTWKH          VALIDATE 'TO' WEEK FIELD                     
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),THISDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BNZ   INVERR                                                           
         GOTO1 DATVAL,DMCB,(1,8(R2)),THISDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    INVERR                                                           
         BAS   RE,VALDATE                                                       
         GOTO1 DATCON,DMCB,(0,THISDATE),(2,TODATE)                              
*                                                                               
VKX      B     DR                  PROCESS RECORDS                              
         SPACE 3                                                                
* THIS ROUTINE MAKES SURE THE DATE IS A VALID START OF WEEK FOR THE             
* GIVEN ESTIMATE.                                                               
*                                                                               
VALDATE  NTR1                                                                   
         MVC   THISDATE(2),QSTART  FILL IN YEAR                                 
         CLC   THISDATE+2(4),QSTART+2                                           
         BNL   VD10                                                             
         MVC   THISDATE(2),QEND                                                 
*                                                                               
VD10     MVC   NEXTDATE,QSTART     START AT BEGIN OF EST                        
*                                                                               
VD20     CLC   NEXTDATE,THISDATE   IF THIS WEEK MATCHES THE WEEK GIVEN          
         BE    XIT                     THEN RETURN WITHOUT ERROR                
*                                                                               
*                                  BUMP TO NEXT WEEK                            
         GOTO1 ADDAY,DMCB,NEXTDATE,NEXTDATE,F'7'                                
*                                                                               
         CLC   NEXTDATE,QEND       REPEAT UNTIL END OF ESIMATE                  
         BL    VD20                                                             
*                                                                               
         B     ERRDATE             ERROR - INVALID DATE                         
         EJECT                                                                  
DR       XC    KEY,KEY             FILL KEY WITH AGYMD/CLT                      
         LA    R4,KEY                                                           
         USING CSORECD,R4                                                       
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
*                                                                               
         MVC   CSOKMKT,BMKT        PUT MKT AND STA IN KEY                       
         MVC   CSOKSTA,BSTA                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RECFOUND,C'N'                                                    
         XC    CURRSTA,CURRSTA                                                  
*                                                                               
DR10     CLC   KEY(5),KEYSAVE      IF END OF CLIENT THEN DONE                   
         BNE   DRX                                                              
*                                                                               
         OC    BMKT,BMKT           IF MARKET GIVEN                              
         BZ    DR20                                                             
         CLC   CSOKMKT,BMKT        THEN DONE IF END OF MARKET                   
         BNE   DRX                                                              
*                                                                               
         OC    BSTA,BSTA           IF STATION GIVEN                             
         BZ    DR20                                                             
         CLC   CSOKSTA,BSTA        THEN DONE IF END OF STATION                  
         BNE   DRX                                                              
*                                                                               
DR20     OC    CSOKSTA,CSOKSTA     SKIP STATION PERCENTAGES RECORD              
         BNZ   DR30                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     DR10                                                             
*                                                                               
DR30     CLC   CSOKEST,FROMEST     IF EST IS LESS THAN REQUESTED                
         BNL   DR40                                                             
         MVC   CSOKEST,FROMEST     THEN READ HIGH FOR ESTIMATE                  
         B     DR50                                                             
*                                                                               
DR40     BNH   DR60                ELSE IF EST IS GREATER THAN REQ              
         MVI   CSOKEST,X'FF'       THEN READ HIGH FOR NEXT STATION              
*                                                                               
DR50     MVI   RDUPDATE,C'N'       READ HIGH AND LOOP BACK                      
         GOTO1 HIGH                                                             
         B     DR10                                                             
*                                                                               
DR60     BE    DR100               ELSE PROCESS RECORD                          
         EJECT                                                                  
DR100    MVI   RDUPDATE,C'Y'       READ RECORD FOR UPDATE                       
         GOTO1 GETREC                                                           
         MVI   RECFOUND,C'Y'       SET RECORD FOUND FLAG                        
*                                                                               
         L     R6,AIO              R6 = A(DESCRIPTION ELEMENT)                  
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
*                                                                               
         MVI   TRDONLY,C'N'        IF PROGRAM WEIGHT IS ZERO                    
         CLI   DSCWGT,0                                                         
         BNE   *+8                                                              
         MVI   TRDONLY,C'Y'        THEN SET TRADE ONLY FLAG                     
*                                                                               
         CLC   CSOKSTA,CURRSTA     IF STATION CHANGED                           
         BE    *+16                                                             
         MVC   CURRSTA,CSOKSTA                                                  
         XC    LEFTOVER,LEFTOVER   THEN CLEAR LEFTOVER DOLLARS                  
*                                                                               
         LA    R1,FROMDATE         POINT TO 'FROM' WEEK ELEMENT                 
         BAS   RE,GETWKEL                                                       
         USING CSOWKEL,R6                                                       
         MVC   TSPOTS,WKTSPOTS     SAVE TRADE/CASH SPOTS, RATE                  
         MVC   CSPOTS,WKCSPOTS                                                  
         MVC   RATE,WKCOST                                                      
         XC    WKTSPOTS,WKTSPOTS   SUBTRACT TRADE/CASH SPOTS FROM               
         XC    WKCSPOTS,WKCSPOTS       'FROM' WEEK ELEMENT                      
*                                                                               
         CLC   FROMEST,TOEST       IF 'FROM' AND 'TO' EST ARE SAME              
         BNE   DR110                                                            
*                                                                               
         BAS   RE,ADDTHIS          THEN ADD SPOTS TO SAME RECORD                
         GOTO1 PUTREC              WRITE RECORD BACK                            
         B     DR120                                                            
*                                                                               
DR110    GOTO1 PUTREC              ELSE WRITE CURRENT RECORD BACK               
*                                                                               
         BAS   RE,ADDOTHER         ADD SPOTS TO OTHER ESTIMATE                  
*                                                                               
DR120    MVI   RDUPDATE,C'N'       READ NEXT KEY AND LOOP BACK                  
         GOTO1 SEQ                                                              
         B     DR10                                                             
*                                                                               
DRX      CLI   RECFOUND,C'N'       IF NO RECORDS FOUND THEN ERROR               
         BE    ERRPNF                                                           
         B     EXIT                ELSE EXIT WITH NO ERROR                      
         EJECT                                                                  
* THIS ROUTINE ADDS THE SPOTS TO THE THE 'TO' WEEK OF THE CURRENT               
* RECORD.                                                                       
*                                                                               
ADDTHIS  NTR1                                                                   
         LA    R1,TODATE           POINT TO 'TO' WEEK ELEMENT                   
         BAS   RE,GETWKEL                                                       
         BAS   RE,ADJCSPTS         ADJUST 'FROM' CASH SPOTS FOR VALUE           
         ICM   R1,3,WKCSPOTS       ADD TRADE/CASH SPOTS TO 'TO'                 
         AH    R1,CSPOTS               WEEK ELEMENT                             
         STCM  R1,3,WKCSPOTS                                                    
         ICM   R1,3,WKTSPOTS                                                    
         AH    R1,TSPOTS                                                        
         STCM  R1,3,WKTSPOTS                                                    
         B     XIT                                                              
         SPACE 3                                                                
* THIS ROUTINE ADDS THE SPOTS TO THE THE 'TO' WEEK OF THE RECORD FOR            
* THE 'TO' ESTIMATE.                                                            
*                                                                               
ADDOTHER NTR1                                                                   
         MVC   CSOKEST,TOEST       READ CSO KEY FOR 'TO' ESTIMATE               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     ERROR IF NOT FOUND                           
         BNE   ERRMATCH                                                         
*                                                                               
         MVI   RDUPDATE,C'Y'       READ RECORD FOR UPDATE                       
         GOTO1 GETREC                                                           
         BAS   RE,ADDTHIS          ADD SPOTS TO 'TO' WEEK                       
         GOTO1 PUTREC              WRITE RECORD BACK                            
*                                                                               
         MVC   CSOKEST,FROMEST     RESTORE KEY READING SEQUENCE                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE RETURNS IN R6 THE ADDRESS OF THE WEEK ELEMENT WHOSE DATE         
* IS POINTED TO BY R1                                                           
*                                                                               
GETWKEL  NTR1                                                                   
         L     R6,AIO              POINT TO FIRST WEEK ELEMENT                  
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
GW10     CLC   WKDATE,0(R1)        IF MATCH FOUND THEN RETURN R6                
         BE    GWX                                                              
         BAS   RE,NEXTEL           REPEAT UNTIL WEEKS EXAUSTED                  
         BE    GW10                                                             
         DC    H'0'                                                             
*                                                                               
GWX      XIT1  REGS=(R6)                                                        
         SPACE 3                                                                
* THIS ROUTINE CHECKS TO SEE IF THE RATE OF THE 'TO' WEEK ELEMENT IS            
* DIFFERENT FROM THE RATE OF THE 'FROM' ELEMENT.  IF IT IS, IT ADJUSTS          
* THE NUMBER OF CASH SPOTS TO END UP WITH THE SAME CASH VALUE.                  
*                                                                               
ADJCSPTS NTR1                                                                   
         USING CSOWKEL,R6                                                       
         CLC   RATE,WKCOST         DO NOTHING IF RATES ARE THE SAME             
         BE    XIT                                                              
*                                                                               
         CLI   TRDONLY,C'Y'        DO NOTHING IF TRADE ONLY PROGRAM             
         BNE   AS10                                                             
         OC    CSPOTS,CSPOTS       AND THERE AREN'T ANY CASH SPOTS              
         BZ    XIT                                                              
*                                                                               
AS10     SR    R1,R1               R1 = 'FROM' SPOTS * 'FROM' RATE              
         ICM   R1,3,CSPOTS              ---------------------------             
         M     R0,RATE                          'TO' RATE                       
         A     R1,LEFTOVER                                                      
         ICM   RF,15,WKCOST                                                     
         LTR   RF,RF               DON'T DIVIDE BY ZERO                         
         BZ    AS20                                                             
         DR    R0,RF                                                            
*                                                                               
         L     RF,RATE             IF REMAINDER (R0) > HALF OF RATE             
         SRL   RF,1                    THEN ADD ONE MORE SPOT                   
         CR    R0,RF                   (OVERSPENT BUT MORE ACCURATE)            
         BNH   *+12                                                             
         LA    R1,1(R1)                                                         
         S     R0,RATE                                                          
         B     AS30                                                             
*                                                                               
AS20     LR    R0,R1               IF RATE IS ZERO THEN NUMBER OF SPOTS         
         SR    R1,R1                   IS ZERO, EVERYTHING IS LEFTOVER          
*                                                                               
AS30     STH   R1,CSPOTS           SAVE NEW NUMBER OF SPOTS                     
         ST    R0,LEFTOVER         SAVE LEFTOVER FOR NEXT TIME                  
         B     XIT                                                              
         EJECT                                                                  
* GETEL AND EXIT POINTS                                                         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
ERREST   OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(37,R2),=C'** ERROR ESTIMATE START DATE MISMATCH'               
         GOTO1 ERREX2                                                           
*                                                                               
ERRDATE  OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(39,R2),=C'** ERROR DATE IS NOT VALID FOR THIS EST'             
         GOTO1 ERREX2                                                           
*                                                                               
ERRMATCH OI    MOVFESTH+6,X'40'    SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(29,R2),=C'** ERROR MISMATCH ON STATION '                       
         LA    R3,KEYSAVE+(CSOKMKT-CSORECD)                                     
         GOTO1 MSUNPK,DMCB,(R3),FULL,37(R2)                                     
         GOTO1 ERREX2                                                           
*                                                                               
ERRPNF   OI    MOVMEDH+6,X'40'     SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(34,R2),=C'** ERROR PROGRAM RECORDS NOT FOUND'                  
         GOTO1 ERREX2                                                           
*                                                                               
EXIT     OI    MOVMEDH+6,X'40'     SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(14,R2),=C'MOVE COMPLETED'                                      
         GOTO1 ERREX2                                                           
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPCSOFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOE3D                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SAVSTART DS    CL6                                                              
FROMEST  DS    X                                                                
TOEST    DS    X                                                                
FROMDATE DS    XL2                                                              
TODATE   DS    XL2                                                              
TSPOTS   DS    H                                                                
CSPOTS   DS    H                                                                
RATE     DS    XL4                                                              
LEFTOVER DS    F                                                                
RECFOUND DS    C                                                                
CURRSTA  DS    XL3                                                              
TRDONLY  DS    C                                                                
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088SPCSO13   05/01/02'                                      
         END                                                                    
