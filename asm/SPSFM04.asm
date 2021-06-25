*          DATA SET SPSFM04    AT LEVEL 170 AS OF 05/01/02                      
*PHASE T21704A                                                                  
         TITLE 'T21704  PRODUCT EXCLUSION RECORDS'                              
T21704   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21704                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                  START IN LIST RECS                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       EQU   *                                                                
*                                                                               
* MEDIA                                                                         
*                                                                               
         LA    R2,PXDMEDH                                                       
         GOTO1 VALIMED                                                          
         FOUT  PXDMEDXH,MEDNM      MOVE MEDIA NAME TO SCREEN                    
*                                                                               
* CLIENT                                                                        
*                                                                               
         LA    R2,PXDCLTH                                                       
         BAS   RE,INPREQ           INPUT REQUIRED?                              
         BNZ   VK0100              YES - SO CONTINUE                            
*                                                                               
         CLI   5(R2),0             ELSE - ANY INPUT?                            
         BE    VK0200              NO - SO SKIP VALIDATION                      
*                                                                               
VK0100   EQU   *                                                                
*                                                                               
         GOTO1 VALICLT             ELSE - VALIDATE THE CLIENT                   
         FOUT  PXDCLTXH,CLTNM      MOVE CLIENT NAME TO SCREEN                   
*                                                                               
VK0200   EQU   *                                                                
*                                                                               
* PRODUCT                                                                       
*                                                                               
         LA    R2,PXDPRDH                                                       
         BAS   RE,INPREQ           INPUT REQUIRED?                              
         BNZ   VK0300              YES - SO CONTINUE                            
*                                                                               
         CLI   5(R2),0             ELSE - ANY INPUT?                            
         BE    VK0350              NO - SO SKIP VALIDATION                      
*                                                                               
         MVC   QPRD,PXDPRD         ELSE - SAVE THE PRODUCT                      
         B     VK0350              AND SKIP THE VALIDATION                      
*                                                                               
VK0300   EQU   *                                                                
*                                                                               
         GOTO1 VALIPRD             ELSE - VALIDATE THE PRODUCT                  
         FOUT  PXDPRDXH,PRDNM,17   MOVE 1ST 17 CHARS OF PRD TO SCREEN           
*                                                                               
VK0350   EQU   *                                                                
*                                                                               
* ESTIMATE                                                                      
*                                                                               
         LA    R2,PXDESTH                                                       
         CLC   PXDEST,=C'ALL'      'ALL' ESTIMATE RECORD?                       
         BNE   VK0400              NO - SO CONTINUE                             
*                                                                               
         MVI   BEST,X'00'          ELSE - MOVE IN 'ALL ESTIMATES' CODE          
         B     VK0500              AND SKIP THE VALIDATION                      
*                                                                               
VK0400   EQU   *                                                                
*                                                                               
         BAS   RE,INPREQ           INPUT REQUIRED?                              
         BNZ   VK0450              YES - SO CONTINUE                            
*                                                                               
         CLI   5(R2),0             ELSE - ANY INPUT?                            
         BE    VK0500              NO - SO SKIP VALIDATION                      
*                                                                               
         ZIC   R0,5(R2)            GET L(FIELD INPUT)                           
         GOTO1 CASHVAL,DMCB,(C'N',PXDEST),(R0)                                  
         MVC   ERRNUM,=AL2(NOESTERR) SET ERROR J.I.C.                           
         CLI   DMCB,0              VALID NUMERIC?                               
         BNE   TRAPERR             NO - SO ERROR                                
*                                                                               
         MVC   BEST,DMCB+7         ELSE - STORE THE ESTIMATE                    
         B     VK0500              AND SKIP THE VALIDATION                      
*                                                                               
VK0450   EQU   *                                                                
*                                                                               
         GOTO1 VALIEST             ELSE - VALIDATE THE ESTIMATE                 
         FOUT  PXDESTXH,ESTNM      MOVE ESTIMATE NAME TO SCREEN                 
*                                                                               
VK0500   EQU   *                                                                
*                                                                               
* STATION                                                                       
*                                                                               
         LA    R2,PXDSTAH                                                       
         MVI   ERROR,INVSTAT                                                    
         CLI   PXDSTA,C'0'         NUMERIC (CABLE) STATION?                     
         BNL   TRAPERR2            YES - SO ERROR                               
*                                                                               
         XC    BSTA,BSTA                                                        
         CLC   PXDSTA(3),=C'ALL'   'ALL' STATION RECORD?                        
         BNE   VK0600              NO - SO CONTINUE                             
*                                                                               
         MVC   BSTA(3),=X'FFFFFF'  ELSE - MOVE IN 'ALL STATION' CODE            
         B     VK0800              AND SKIP THE VALIDATION                      
*                                                                               
VK0600   EQU   *                                                                
*                                                                               
         BAS   RE,INPREQ           INPUT REQUIRED?                              
         BNZ   VK0700              YES - SO CONTINUE                            
*                                                                               
         CLI   5(R2),0             ELSE - ANY INPUT?                            
         BE    VK0800              NO - SO CONTINUE                             
*                                                                               
VK0700   EQU   *                                                                
*                                                                               
         GOTO1 VALISTA             ELSE - VALIDATE THE STAION                   
* FILE PROGRAM USED A 'T' FOR MEDIA 'C' BECAUSE MSPACK WASN'T ABLE              
* TO HANDLE THE C. WE'RE GOING TO DO IT FOR COMPATABILITY                       
         CLI   QMED,C'C'                                                        
         BNE   *+8                                                              
         OI    BSTA+2,2                                                         
*                                                                               
VK0800   EQU   *                                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PXCKEY,R6                                                        
         MVC   PXCKTYP(2),=X'0D70'  BUILD THE BASE PXC KEY                      
         MVC   PXCKAGM,BAGYMD                                                   
         MVC   PXCKCLT,BCLT                                                     
         MVC   PXCKSTA,BSTA                                                     
         MVC   PXCKPRD,QPRD                                                     
         MVC   PXCKEST,BEST                                                     
*                                                                               
         MVC   MYKEYSV(13),KEY     SAVE THE WHOLE KEY TO START                  
*                                                                               
VKX      B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING PXCRECD,R6                                                       
*                                                                               
         GOTO1 CLUNPK,DMCB,PXCKCLT,PXDCLT UNPACK THE CLIENT                     
         OI    PXDCLTH+6,X'80'     SET TRANSMIT BIT                             
*                                                                               
         MVC   PXDPRD,PXCKPRD      MOVE OUT THE PRODUCT                         
         OI    PXDPRDH+6,X'80'     SET TRANSMIT BIT                             
*                                                                               
         CLI   PXCKEST,X'00'       ALL ESTIMATES?                               
         BNE   DK0100              NO - SO CONTINUE                             
*                                                                               
         MVC   PXDEST(3),=C'ALL'   MOVE OUT 'ALL'                               
         B     DK0200              AND CONTINUE                                 
*                                                                               
DK0100   EQU   *                                                                
*                                                                               
         EDIT  PXCKEST,PXDEST,ALIGN=LEFT EDIT THE ESTIMATE NUMBER               
*                                                                               
DK0200   EQU   *                                                                
*                                                                               
         OI    PXDESTH+6,X'80'     SET TRANSMIT BIT                             
*                                                                               
         CLC   PXCKSTA(3),=X'FFFFFF' ALL STATIONS?                              
         BNE   DK0300              NO - SO CONTINUE                             
*                                                                               
         MVC   PXDSTA(3),=C'ALL'   ELSE - MOVE OUT 'ALL'                        
         B     DK0400              AND CONTINUE                                 
*                                                                               
DK0300   EQU   *                                                                
*                                                                               
         XC    WORK(2),WORK        CLEAR THE HIGH ORDER 2 BYTES                 
         MVC   WORK+2(3),PXCKSTA   MOVE IN THE STATION                          
         GOTO1 MSUNPK,DMCB,WORK,WORK,PXDSTA UNPACK THE STATION                  
*                                                                               
DK0400   EQU   *                                                                
*                                                                               
         OI    PXDSTAH+6,X'80'     TRANSMIT                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       MVC   SVKEY,KEY           SAVE THE PRD EXCLUSION RECORD KEY            
*                                                                               
         MVI   ELCODE,X'01'        REMOVE LAST ACT DATE ELEMENT                 
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM           CLEAR THE ELEMENT BUILD AREA                 
         LA    R6,ELEM                                                          
         MVC   ELEM(2),=X'0105'    ELEM CODE AND LENGTH                         
         GOTO1 DATCON,DMCB,(5,0),(3,ELEM+2) NEW ACTIVITY DATE                   
         GOTO1 ADDELEM             ADD THE ELEMENT                              
*                                                                               
         MVI   ELCODE,X'05'        REMOVE ALL EXCLUSION ELEMENTS                
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,PXDFRSTH         A(FIRST SCREEN FIELD)                        
*                                                                               
VR0100   EQU   *                                                                
*                                                                               
         XC    ELEM,ELEM           BUILD NEW ELEMENTS                           
         LA    R6,ELEM             J.I.C.                                       
         L     R3,AIO                                                           
         LA    R3,24(R3)           GET PAST HEADER                              
         CLI   5(R2),0             TEST IF PRGM FIELD IS EMPTY                  
         BE    VR1000                                                           
         MVC   ELEM(2),=X'0517'                                                 
         MVC   ELEM+2(L'PXCPGM),8(R2)                                           
         SPACE 2                                                                
         CLC   ELEM+2(2),=C'P/'        TEST NTWK CODE                           
         BNE   VR0200                                                           
         MVC   TEMPAREA+3(6),10(R2)    SAVE NTWK PROGRAM CODE                   
         OC    TEMPAREA+3(6),SPACES                                             
         MVI   TEMPAREA+1,C'Y'                                                  
         LR    R5,R2                   SAVE HEADER ADDRESS                      
         B     VR0300                                                           
         SPACE                                                                  
VR0200   CLC   ELEM+2(2),=C'T/'    TIME EXPRESSION                              
         BE    TIMED                                                            
         CLC   ELEM+2(2),=C'D/'    DAY EXPRESSION                               
         BE    DAYED                                                            
VR0300   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             IS THERE A START/END DATE                    
         BNE   VR0400              YES                                          
         MVI   ERROR,MISSING       NO. ERROR.                                   
         B     TRAPERR2                                                         
VR0400   MVC   ERRNUM,=AL2(DATERR)                                              
         LA    R7,8(R2)                                                         
         GOTO1 DATVAL,DMCB,(0,(R7)),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    TRAPERR                                                          
         A     R7,0(R1)                                                         
         CLI   0(R7),C'-'                                                       
         BE    VR0500                                                           
         CLI   0(R7),C','                                                       
         BE    VR0500                                                           
         MVC   WORK+6(6),WORK                                                   
         B     VR0600                                                           
VR0500   LA    R7,1(R7)                                                         
         GOTO1 DATVAL,DMCB,(0,(R7)),WORK+6                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    TRAPERR                                                          
VR0600   GOTO1 DATCON,DMCB,(0,WORK),(2,ELEM+19)                                 
         GOTO1 (RF),(R1),(0,WORK+6),(2,ELEM+21)                                 
         CLC   ELEM+19(2),ELEM+21                                               
         BC    12,VR0700                    ST DTE EQU/LESS END DTE             
         MVC   ERRNUM,=AL2(SPDERR)                                              
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
VR0700   MVI   ELCODE,X'05'       * ROUTINE TO ADD ELEMS IN A/N ORDER           
         SPACE                                                                  
         CLI   TEMPAREA+1,C'Y'     TEST NTWK CODE                               
         BNE   VR0800                                                           
         XC    ERRNUM,ERRNUM       CLEAR ERRNUM                                 
         BAS   RE,NTWKRTN          GOTO NETWORK ROUTINE                         
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO TO 1ST IO AREA                   
*                                                                               
         CLC   ERRNUM,=H'0'        WAS ERROR SET IN NTWKRTN                     
         BE    TRAPERR             YES - GOTO ERROR                             
         SPACE                                                                  
VR0800   BAS   RE,NEXTEL                                                        
         BNE   VR0900                                                           
         CLC   ELEM(23),0(R3)                                                   
         BH    VR0800                                                           
         BE    DUPERRTN                                                         
VR0900   GOTO1 ADDELEM                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BE    VR1200              END OF SCREEN                                
         B     VR0100                                                           
*                                                                               
VR1000   LR    R7,R2               LOAD PGM FIELD INTO R7                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             IS START/END DATE ALSO BLANK                 
         BE    VR1100                                                           
         MVI   ERROR,MISSING       NO. ERROR.                                   
         LR    R2,R7               LOAD PGM FIELD INTO R2                       
         B     TRAPERR2                                                         
VR1100   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R1,PXDLSTH          A(LAST SCREEN FIELD)                         
         CR    R2,R1               PAST E.O.S.?                                 
         BL    VR0100              NO - SO LOOP BACK UP                         
*                                                                               
VR1200   EQU   *                                                                
*                                                                               
         BAS   RE,ANYCHK                                                        
         SPACE                                                                  
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       EQU   *                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         L     R3,AIO                                                           
         LA    R3,24(R3)          R3 POINTS TO 01 ELEMENT                       
         USING PXCEL05,R3                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,NEXTEL          R3 POINTS TO 05 ELEMENT                       
         BNE   DRNOTFND                                                         
*                                                                               
DR0100   EQU   *                                                                
*                                                                               
         LA    R2,PXDFRSTH         A(FIRST SCREEN FIELD)                        
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
*                                                                               
* GET THE 05 ELEMENTS                                                           
*                                                                               
         LA    R2,PXDFRSTH                                                      
         B     DR0300                                                           
DR0200   BAS   RE,NEXTEL                                                        
         BNE   EXIT                     NO MORE 05 ELEMENTS                     
DR0300   CLI   PXCPGM,0            TIME CODE                                    
         BE    DR0600                                                           
         CLI   PXCPGM,1            DAY CODE                                     
         BE    DR0700                                                           
         CLI   PXCPGM,2            TIME + DAY CODE                              
         BE    DR0800                                                           
         MVC   8(L'PXCPGM,R2),PXCPGM                                            
DR0400   FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 DATCON,DMCB,(2,PXCSTDTE),(5,8(R2))                               
         CLC   PXCSTDTE,PXCEDTE         IF START/END DATES ARE THE SAME         
         BE    DR0500                   DISPLAY ONLY ONE.                       
         GOTO1 (RF),(R1),(2,PXCEDTE),(5,17(R2))                                 
         MVI   16(R2),C'-'                                                      
DR0500   FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    DR0200                                                           
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
DR0600   DS    0H                                                               
         MVC   DMCB+4(4),=X'D9000A11'     * UNTIME                              
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+1,10(R2)                                        
         MVC   8(2,R2),=C'T/'                                                   
         B     DR0400                                                           
         SPACE                                                                  
*                                                                               
DR0700   DS    0H                                                               
         MVC   DMCB+4(4),=X'D9000A0F'     * DAYUNPK                             
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+1,10(R2)                                        
         MVC   8(2,R2),=C'D/'                                                   
         B     DR0400                                                           
         SPACE                                                                  
DR0800   DS    0H                  TIME + DAY                                   
         MVI   WORK,C' '                                                        
         MVC   WORK+1(25),WORK                                                  
         MVC   DMCB+4(4),=X'D9000A11'     * UNTIME                              
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+1,WORK                                          
         LA    R4,WORK+15          POINT WELL PAST END OF TIME                  
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'/'                                                       
         MVC   DMCB+4(4),=X'D9000A0F'     * DAYUNPK                             
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+5,2(R4)   DAY IS AFTER TIME                     
*                                                                               
         MVC   8(2,R2),=C'T/'                                                   
         MVC   10(17-2,R2),WORK    SCREEN FIELD LENGTH -2                       
         B     DR0400                                                           
         EJECT                                                                  
*                                                                               
DR900    B     EXIT                                                             
*                                                                               
DRNOTFND LA    R2,PXDMEDH                                                       
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR2                                                         
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
* NOTE: BOTH LISTRECS AND PRINTREP COME THROUGH THIS SECTION OF CODE            
* TO AVOID DUPLICATION OF THE HIGH/NEXT READING AND FILTERING AND THE           
* PRINTING OF THE 1ST LINE OF DATA FOR EACH RECORD.                             
*                                                                               
LR       LA    R2,PXLSELH                                                       
***      MVI   NLISTS,13           ONLY 13 LINES ON SCREEN                      
         OC    KEY(13),KEY         FIRST TIME?                                  
         BNZ   LR0050              NO - SO CONTINUE                             
*                                                                               
         MVC   KEY(2),=X'0D70'     ELSE - BUILD THE BASE KEY                    
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   AIO,AIO1                                                         
*                                                                               
LR0050   EQU   *                                                                
*                                                                               
         GOTO1 HIGH                                                             
         B     LR0150                                                           
*                                                                               
LR0100   GOTO1 SEQ                                                              
                                                                                
LR0150   CLC   KEY(3),MYKEYSV      TEST FOR ALL DONE                            
         BNE   LR1000                                                           
*                                                                               
         LA    R6,KEY                                                           
         USING PXCRECD,R6                                                       
*                                                                               
         CLI   PXDCLTH+5,0         CLIENT ENTERED?                              
         BE    LR0200              NO - SO CONTINUE                             
*                                                                               
         CLC   BCLT,PXCKCLT        ELSE - SAME CLIENT?                          
         BNE   LR0100              NO - SO GET NEXT KEY                         
*                                                                               
LR0200   EQU   *                                                                
*                                                                               
         CLI   PXDPRDH+5,0         PRODUCT ENTERED?                             
         BE    LR0300              NO - SO CONTINUE                             
*                                                                               
         CLC   QPRD,PXCKPRD        ELSE - SAME PRODUCT?                         
         BNE   LR0100              NO - SO GET NEXT KEY                         
*                                                                               
LR0300   EQU   *                                                                
*                                                                               
         CLI   PXDESTH+5,0         ESTIMATE ENTERED?                            
         BE    LR0400              NO - SO CONTINUE                             
*                                                                               
         CLC   BEST,PXCKEST        ELSE - SAME EST?                             
         BNE   LR0100              NO - SO GET NEXT KEY                         
*                                                                               
LR0400   EQU   *                                                                
*                                                                               
         CLI   PXDSTAH+5,0         STATION ENTERED?                             
         BE    LR0500              NO - SO CONTINUE                             
*                                                                               
         CLC   BSTA,PXCKSTA        ELSE - SAME STATION?                         
         BNE   LR0100              NO - SO GET NEXT KEY                         
*                                                                               
LR0500   EQU   *                                                                
         CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   LR0502                                                           
         CLI   QMED,C'C'           AND COMBINED MEDIA                           
         BNE   LR0502                                                           
         CLC   PXCKSTA,=3X'FF'     STATION=ALL                                  
         BE    LR0502              SKIP THE '02' CHECK                          
         CLI   PXCKSTA+2,X'02'     YES - IGNORE BAD RECORDS                     
         BNE   LR0100                                                           
*                                                                               
LR0502   MVI   LISTAR,C' '                                                      
         MVC   LISTAR+1(L'LISTAR-1),LISTAR                                      
         LA    R7,LISTAR                                                        
         USING LISTARD,R7                                                       
         GOTO1 GETREC              GET THE PXC RECORD                           
*                                                                               
         GOTO1 CLUNPK,DMCB,PXCKCLT,LICLT UNPACK THE CLIENT                      
*                                                                               
         MVC   LIPRD,PXCKPRD       MOVE OUT THE PRODUCT                         
*                                                                               
         CLI   PXCKEST,X'00'       ALL ESTIMATES?                               
         BNE   LR0600              NO - SO CONTINUE                             
*                                                                               
         MVC   LIEST(3),=C'ALL'    MOVE OUT 'ALL'                               
         B     LR0700              AND CONTINUE                                 
*                                                                               
LR0600   EQU   *                                                                
*                                                                               
         EDIT  PXCKEST,LIEST,ALIGN=LEFT EDIT OUT THE ESTIMATE NUMBER            
*                                                                               
LR0700   EQU   *                                                                
*                                                                               
         CLC   PXCKSTA(3),=X'FFFFFF' ALL STATIONS?                              
         BNE   LR0800              NO - SO CONTINUE                             
*                                                                               
         MVC   LISTA(3),=C'ALL'    ELSE - MOVE OUT 'ALL'                        
         B     LR0900              AND CONTINUE                                 
*                                                                               
LR0800   EQU   *                                                                
*                                                                               
         XC    WORK2(2),WORK2      CLEAR THE HIGH ORDER 2 BYTES                 
         MVC   WORK2+2(3),PXCKSTA  MOVE IN THE STATION                          
         GOTO1 MSUNPK,DMCB,WORK2,WORK2,LISTA UNPACK THE STATION                 
*                                                                               
LR0900   EQU   *                                                                
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BE    PR                  YES - SO GO TO PRINT REPORT                  
*                                                                               
         GOTO1 LISTMON             ELSE - MOVE DATA TO SCREEN                   
         B     LR0100              AND GET NEXT RECORD                          
*                                                                               
LR1000   EQU   *                                                                
*                                                                               
         B     EXIT                                                             
         DROP  R6,R7                                                            
*                                                                               
* PRINT REPORT                                                                  
*                                                                               
PR       EQU   *                                                                
*                                                                               
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         MVC   P1(LLISTARD),LISTAR MOVE SCREEN DATA TO PRINT LINE               
         GOTO1 SPOOL,DMCB,(R8)     PRINT THE LINE                               
*                                                                               
         LA    R7,P1                                                            
         USING PRINTD,R7                                                        
         L     R3,AIO                                                           
         LA    R3,24(R3)          R3 POINTS TO 01 ELEMENT                       
         USING PXCEL05,R3                                                       
*                                                                               
* GET THE 05 ELEMENTS                                                           
*                                                                               
         MVI   ELCODE,X'05'                                                     
         BAS   RE,NEXTEL          R3 POINTS TO 05 ELEMENT                       
         BNE   LR0100              NONE FOUND - GO GET NEXT REC                 
*                                                                               
         B     PR0300                                                           
*                                                                               
PR0200   BAS   RE,NEXTEL                                                        
         BE    PR0300              GOT ONE - CONTINUE                           
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     ELSE - PRINT THE LAST LINE                   
         B     LR0100              AND GET NEXT RECORD                          
*                                                                               
PR0300   EQU   *                                                                
*                                                                               
         LA    R2,PGM                                                           
         CLI   PXCPGM,0            TIME CODE                                    
         BE    PR0600                                                           
         CLI   PXCPGM,1            DAY CODE                                     
         BE    PR0700                                                           
         CLI   PXCPGM,2            TIME + DAY CODE                              
         BE    PR0800                                                           
         MVC   0(L'PXCPGM,R2),PXCPGM                                            
PR0400   EQU   *                                                                
         LA    R2,DATE                                                          
         GOTO1 DATCON,DMCB,(2,PXCSTDTE),(5,(R2))                                
         CLC   PXCSTDTE,PXCEDTE         IF START/END DATES ARE THE SAME         
         BE    PR0500                   DISPLAY ONLY ONE.                       
         GOTO1 (RF),(R1),(2,PXCEDTE),(5,9(R2))                                  
         MVI   8(R2),C'-'                                                       
*                                                                               
PR0500   EQU   *                                                                
*                                                                               
         LA    R7,LPRINTD(R7)      INC TO NEXT PRINT POSITION                   
         LA    R1,P1               A(P1)                                        
         LA    R1,LPRLINE(R1)      A(E.O.L.)                                    
         CR    R7,R1               PAST E.O.L.?                                 
         BL    PR0200              NO - SO GET NEXT NEXT ELEMENT                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     ELSE - PRINT THE LINE                        
         LA    R7,P1               RE-SET TO START OF LINE                      
         B     PR0200              AND GET NEXT ELEMENT                         
         SPACE                                                                  
*                                                                               
PR0600   DS    0H                                                               
         MVC   DMCB+4(4),=X'D9000A11'     * UNTIME                              
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+1,2(R2)                                         
         MVC   0(2,R2),=C'T/'                                                   
         B     PR0400                                                           
         SPACE                                                                  
*                                                                               
PR0700   DS    0H                                                               
         MVC   DMCB+4(4),=X'D9000A0F'     * DAYUNPK                             
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+1,2(R2)                                         
         MVC   0(2,R2),=C'D/'                                                   
         B     PR0400                                                           
         SPACE                                                                  
PR0800   DS    0H                  TIME + DAY                                   
         MVI   WORK,C' '                                                        
         MVC   WORK+1(25),WORK                                                  
         MVC   DMCB+4(4),=X'D9000A11'     * UNTIME                              
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+1,WORK                                          
         LA    R4,WORK+15          POINT WELL PAST END OF TIME                  
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'/'                                                       
         MVC   DMCB+4(4),=X'D9000A0F'     * DAYUNPK                             
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+5,2(R4)   DAY IS AFTER TIME                     
*                                                                               
         MVC   0(2,R2),=C'T/'                                                   
         MVC   2(17-2,R2),WORK    SCREEN FIELD LENGTH -2                        
         B     PR0400                                                           
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
HEDSPECS EQU   *                                                                
*                                                                               
         SSPEC H1,1,C'MEDIA     SPOT T.V.'                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,50,C'PRODUCT EXCLUSION RECORD LISTING'                        
         SSPEC H2,50,C'--------------------------------'                        
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H5,112,PAGE                                                      
         SSPEC H7,1,C'CLIENT'                                                   
         SSPEC H8,1,C'------'                                                   
         SSPEC H7,16,C'PRODUCT'                                                 
         SSPEC H8,16,C'-------'                                                 
         SSPEC H7,31,C'ESTIMATE'                                                
         SSPEC H8,31,C'--------'                                                
         SSPEC H7,45,C'STATION'                                                 
         SSPEC H8,45,C'-------'                                                 
         DC    X'00'                                                            
         EJECT                                                                  
CLRSCRN  NTR1                                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
*                                                                               
*** ROUTINES REFERRED TO IN PROGRAM ***                                         
*                                                                               
* NETWORK ROUTINE TO GET NTWK PRG NAME *                                        
         SPACE                                                                  
NTWKRTN  DS    0H                                                               
         NTR1                                                                   
         MVC   AIO,AIO2            USE 2ND IO AREA                              
         L     R3,AIO2             R3 = A(IO2 AREA)                             
         CLI   TEMPAREA,1          TEST FIRST TIME                              
         BE    NT10                                                             
         MVI   TEMPAREA,1                                                       
         SPACE                                                                  
NT10     XC    KEY,KEY             SET UP TO READ NTWK REC                      
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),SVKEY+2    AGY/MED                                      
         MVC   KEY+3(2),BMKT       MARKET                                       
         MVC   KEY+5(6),TEMPAREA+3 NTWK PRG CODE SAVED IN TEMPAREA+3            
         MVC   KEY+11(2),ELEM+19   START DATE(COMPRESSED FROM SCREEN)           
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   NONTWK                                                           
         SPACE                                                                  
         GOTO1 GETREC                                                           
         USING NPGELEM,R3                                                       
         LA    R3,24(R3)           R3 - 01 ELEM                                 
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM+2(16),NPGNAME                                               
         MVI   ELEM+18,C' '             PXCPGM=17 CHARS                         
         XC    8(17,R5),8(R5)           HEADER SAVED IN R5                      
         MVC   8(16,R5),NPGNAME                                                 
         OI    6(R5),X'80'              TRANSMIT BIT                            
         SPACE                                                                  
         MVC   KEY,SVKEY           RESTORE KEY                                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
NONTWK   DS    0H                                                               
         MVC   ERRNUM,=AL2(NTPGMERR)                                            
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
NEXTEL   CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R3)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         CLC   ELCODE,0(R3)                                                     
         BNE   NEXTEL                                                           
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT EQUAL                       
*                                                                               
TIMED    DS    0H                                                               
         XC    ELEM+2(21),ELEM+2   CLEAR ELEMENT                                
*                                  FIND LENGTH FOR TIME EDIT                    
         LA    R4,10(R2)           BYPASS T/                                    
*                                                                               
TM2      DS    0H                                                               
         CLI   0(R4),C'/'          END WITH /                                   
         BE    TM3                                                              
         CLI   0(R4),C' '          OR BLANK                                     
         BNH   TM3                                                              
         LA    R4,1(R4)                                                         
         B     TM2                                                              
*                                                                               
TM3      DS    0H                                                               
         LA    R0,10(R2)                                                        
         SR    R4,R0               R4 HAS LENGTH TIME PORTION                   
         XC    WORK2(4),WORK2                                                   
         MVC   DMCB+4(4),=X'D9000A0E'        *TIMVAL                            
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,((R4),10(R2)),WORK2                                    
         CLI   0(R1),X'FF'                                                      
         BNE   TM10                                                             
         MVC   ERRNUM,=AL2(INVERR)                                              
         B     TRAPERR                                                          
TM10     MVC   ELEM+3(4),WORK2     SET TIME                                     
         MVI   ELEM+2,0                                                         
*                                                                               
         LA    RF,8+16(R2)         END OF FIELD                                 
*                                  GET LENGTH OF POSSIBLE DAY PORTION           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    R0,10(R2,R4)        POINT 1 PAST TIME PART                       
         SR    RF,R0                                                            
         BNP   TM15                NO FURTHER INPUT                             
*                                                                               
         LA    R4,10+1(R2,R4)                                                   
         ST    R4,FULL             START OF DAY PART                            
         STC   RF,FULL                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A03'        *DAYPAK                            
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         MVC   DMCB,FULL           ADDR OF DAY                                  
         GOTO1 (RF),DMCB,,WORK2,WORK2+16                                        
         CLI   WORK2,0                                                          
         BNE   TM14                                                             
         MVC   ERRNUM,=AL2(INVERR)                                              
         B     TRAPERR                                                          
TM14     MVI   ELEM+2,2            TIME + DAY                                   
         MVC   ELEM+7(1),WORK2     DAY AFTER TIME                               
*                                                                               
TM15     DS    0H                                                               
         B     VR0300                                                           
         SPACE                                                                  
*                                                                               
DAYED    DS    0H                                                               
         XC    ELEM+2(21),ELEM+2                                                
         ZIC   R4,5(R2)                      L'INPUT TO R4                      
         SH    R4,=H'2'                      MINUS 2 (FOR 'D/')                 
         MVC   DMCB+4(4),=X'D9000A03'        *DAYPAK                            
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,((R4),10(R2)),WORK2,WORK2+16                           
         CLI   WORK2,0                                                          
         BNE   DY10                                                             
         MVC   ERRNUM,=AL2(INVERR)                                              
         B     TRAPERR                                                          
DY10     MVI   ELEM+2,1            DAY CODE                                     
         MVC   ELEM+3(1),WORK2                                                  
         B     VR0300                                                           
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE CHECKS IF INPUT IS REQUIRED BASED ON THE ACTION NUMBER.          
*                                                                               
* NOTES: 1)IT DOESN'T SAVE/RESTORE ANY REGISTERS.                               
*        2)THE RESULT OF THE 'CLI' INSTRUCTIONS SETS THE RETURN CC.             
*          0 = I/P NOT REQUIRED    <>0 = I/P REQUIRED                           
*                                                                               
INPREQ   EQU   *                                                                
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    IPRQEXIT            YES - SO EXIT (SETS CC)                      
         CLI   ACTNUM,ACTREP       ELSE - REPORT? (SETS CC)                     
*                                                                               
IPRQEXIT EQU   *                                                                
*                                                                               
         BR    RE                  AND RETURN TO CALLER                         
         SPACE 3                                                                
*                                                                               
ANYCHK   EQU   *                                                                
*                                                                               
         L     R2,AIO              A(RECORD)                                    
         CLC   13(2,R2),=X'0029'   WAS SCREEN LEFT BLANK ON CHA/ADD             
         BHR   RE                                                               
         LA    R2,PXDFRSTH         YES. ERROR.                                  
         MVI   ERROR,MISSING                                                    
         B     TRAPERR2                                                         
*                                                                               
DUPERRTN DS    0H                                                               
         ZIC   R0,0(R2)            MOVE CURSOR BACK TO PGM FIELD                
         SR    R2,R0                                                            
         MVC   ERRNUM,=AL2(DUPERR)                                              
         B     TRAPERR                                                          
*                                                                               
         PRINT GEN                                                              
*                                                                               
TRAPERR  EQU   *                                                                
*                                                                               
* FORCE TO USE SPOT SYSTEM MESSAGES                                             
*                                                                               
         OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVC   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
*                                                                               
TRAPERR2 EQU   *                                                                
*                                                                               
         GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
* SPOT ERROR MESSAGE EQUATES                                                    
*                                                                               
DATERR   EQU   20                  INVALID DATE                                 
NOESTERR EQU   42                  ESTIMATE NOT FOUND                           
SPDERR   EQU   103                 START/END DATE ERROR                         
INVERR   EQU   240                 INVALID INPUT                                
DUPERR   EQU   245                 DUPLICATE ENTRY                              
NTPGMERR EQU   480                 NTWK PGM REC NOT FOUND                       
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM79D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM7AD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
ERRNUM   DS    XL2                 SPOT ERROR NUMBER                            
TEMPAREA DS    CL10                TEMP STORAGE AREA                            
WORK2    DS    CL48                ANOTHER TEMP AREA                            
MYKEYSV  DS    CL20                ANOTHER TEMP KEY AREA                        
         SPACE 3                                                                
LISTARD  DSECT                                                                  
LICLT    DS    CL3                 CLIENT CODE                                  
         DS    CL12                SPACE                                        
LIPRD    DS    CL3                 PRODUCT CODE                                 
         DS    CL12                SPACE                                        
LIEST    DS    CL3                 ESTIMATE CODE                                
         DS    CL11                SPACE                                        
LISTA    DS    CL7                 STATION CALL LETTERS                         
LLISTARD EQU   *-LISTARD           L(DSECT)                                     
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE SPGENPXC                                                       
       ++INCLUDE SPGENPROG                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
PRINTD   DSECT                                                                  
         DS    CL2                 INDENT                                       
PGM      DS    CL17                PROGRAM NAME                                 
         DS    CL2                 SPACES                                       
DATE     DS    CL17                START/END DATES                              
LPRINTD  EQU   *-PRINTD            L(ENTRY)                                     
LPRLINE  EQU   LPRINTD*3           3 ENTRIES ON A LINE                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'170SPSFM04   05/01/02'                                      
         END                                                                    
