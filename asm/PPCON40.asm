*          DATA SET PPCON40    AT LEVEL 032 AS OF 11/05/03                      
*PHASE T40D40A                                                                  
*INCLUDE NUMED                                                                  
*INCLUDE XSORT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPCON40 - PRINTPAK CONTRACT ARATES DISPLAY/CHANGE'              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 09/20/01 REORGANIZED PPCONWRK                                            
*                                                                               
* BPLA  03/00   IF PRODUCT ENTERED AFTER CLIENT                                 
*               CHECK TO BE SURE IT IS A PRODUCT                                
*               CONTRACT FOR THAT PRODUCT                                       
*                                                                               
* BPLA  8/99    FIX DISPLAY OF LEVEL TYPE (IN CON50)                            
*                                                                               
* BPLA  2/99    FIX BUG IN DISP285 - IF NODISP WAS 255                          
*               R9 WAS NOT BEING SET PROPERLY                                   
*                                                                               
* BPLA  10/98   ALLOW PRODUCT LEVEL RATES FOR NEWSPAPERS                        
*               IF RATE ENTERED.                                                
*                                                                               
* BPLA  4/98    CHANGES FOR OPEN RATES                                          
*                                                                               
* BPLA 12/97    CHANGE TO ALLOW SHOWING UP TO 999 (FOR GRPS)                    
*                                                                               
* BPLA 10/97    CHANGES TO FIX BUGS (ROGER S)                                   
*                                                                               
* BPLA 1/97     NEW AOR CONTROL TO DENY AOR CONTRACT                            
*               ACCESS BY BRAND AGENCIES (SADVDATA+16 X'01')                    
*                                                                               
* BPLA 1/96     IF PRBLIND IS "N" DISPLAY AS "$"                                
*               AND PRECEED LEVEL WITH AN "N"                                   
*                                                                               
* SMYE 12/6/95  CHANGED VDTCNV TO VDATCON WITH "NEW" PARAM'S.                   
*                                                                               
* BPLA 5/95     DISPLAY /I OR /L FOR ALL UNIT RATES                             
*                                                                               
* BPLA 3/20/95  COPY OF T40D40 (LEVEL 5 8/18/94)                                
*               CHANGES FOR INCH RATES                                          
*                                                                               
* BPLA 8/18/94  FIX MESSAGE TO DISPLAY ADDITIONAL RATES                         
*               WHEN USING DISH AND DISL                                        
*                                                                               
* BPLA 1/13/94  ALLOW LEVEL IND = $ FOR RATE CODES                              
*               IF RATE IS ENTERED (NOT LOOKED-UP)                              
*                                                                               
* BPLA 10/21/92 IN AOR SITUATION AND I'M NOT THE AOR                            
*               MUST SWITCH TO THE AOR TO READ AOR CONTRACTS                    
*               IF CONTROLS SAY NO AGY CONTRACTS                                
* BPLA 2/24/92  CHANGES TO LOOK=UP ADV LEVELS AND/OR RATES                      
*                                                                               
* BPLA 1/24/92  INCLUDE PPCONWRKA (HAS ADVERTISER DATA)                         
*                                                                               
* BPLA 10/23/91 REQUIRE LEVEL IND IN ANY OTHER RATE LINE INFO                   
*               IS ENTERED                                                      
* BPLA 6/1/89  ALLOWS FOR OPEN RATE FOR NON-NEWSPAPERS              L06         
*                                                                 BUG01         
* ROSA 8/30/88 EXPAND RATE FIELDS TO 15 ALLOWING FOR FULL ONE     BUG01         
*          MILLION DOLLARS WITH TWO SPECIAL DESCRIPTORS (ST)      BUG01         
*                                                                 BUG01         
* ROSA 7/20/88 DO NOT ALLOW ADDL OR ADDH IF ELEMENTS EXITS // MUST  L05         
*           USE DISPLAY THEN CHANGE PATH.                           L05         
*                                                                   L05         
* ROSA  6/2/88  ALLOW ENTERING C RATES                              L04         
*                                                                   L04         
* ROSA 5/25/88  ALLOW ENTERING S RATES                              L03         
*                                                                   L03         
* ROSA 5/16/88 ALLOW ENTERING HIGHER AND LOWER LEVELS OF RATES      L02         
*                 ***************************************           L02         
*    ********NOTE USING TEST VERSION OF RATELOOK***************     L02         
*    *************SEE INCLUDE ABOVE MUST CHANGE WHEN LIVE******     L02         
*                 ***************************************           L02         
*                                                                   L01         
* ROSA 4/28/88 ALLOW ENTERING NET RATES // N FOLLOWED BY RATE       L01         
*                                                                   L01         
*                                                                               
T40D40   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40D40                                                         
         LA    R6,1(RB)                                                         
         LA    R6,4095(R6)                                                      
         USING T40D40+4096,R6                                                   
         USING T40DFFD,RA                                                       
         L     RC,0(R1)                                                         
         LA    R7,1(RC)                                                         
         LA    R7,4095(R7)                                                      
         USING GENOLD,RC,R7                                                     
         PRINT GEN                                                              
         RELOC RELO40                                                           
         PRINT NOGEN                                                            
         CLI   TWASTAT,X'FC'                                                    
         BE    CON50           SCREEN IN TWA                                    
         GOTO1 VCALLOV,DMCB,KBALAST,X'D9040DFC'                                 
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   TWASTAT,X'FC'                                                    
CON50    DS    0H                                                               
*                                                                   L02         
         MVI   AORCFND,0       INITIALIZE AOR CONTRACT FOUND SW                 
*                                                                               
         FOUT  RATDESCH                                            L02          
         MVC   RATDESC(46),=C'*******  PROCESSING CURRENT LEVELS ******C        
               *****'                                               L02         
         TM    TWAKIND,1       LOWER                                L02         
         BNO   *+10                                                 L02         
         MVC   RATDESC+20(15),=C'LOWER LEVELS  *'                 L02           
         TM    TWAKIND,2        HIGHER                              L02         
         BNO   *+10                                                 L02         
         MVC   RATDESC+20(15),=C'HIGHER LEVELS  '                               
         TM    TWAKIND,4        HIGHER                              L02         
         BNO   *+10                                                 L02         
         MVC   RATDESC+20(15),=C'   OPEN RATES  '                               
         NI    TWAKIND,X'7F'       SET OFF CONTRACT DISPLAYED                   
         CLC   KBAACT(3),=C'DIS'                                                
         BE    CON100                                                           
*              GET K FOR CHANGE                                                 
         LA    R2,KBANUMH                                                       
         LA    R3,12               INVALID ACTION                               
         TM    4(R2),X'20'         DID USER CHANGE CONTRACT NUMBER?             
         BZ    ERROR                                                            
         B     CON102                                                           
*                                                                               
CON100   FOUT  KBAPAGH,SPACES,3                                                 
         LA    R2,KBANUMH                                                       
         LA    R3,1                                                             
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    ERROR           NO NUMBER                                        
*                                                                               
         LA    R3,53                                                            
         STH   R0,HALF                                                          
         MVC   PCONKEY(13),SAVKKEY            READ CONTRACT                     
         MVC   PCONNUM,HALF                                                     
         MVC   KEY,PCONKEY                                                      
**                                                                              
CON102   DS    0H                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    CON103                                                           
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    CON103                                                           
         TM    SADVDATA+15,X'20'                                                
         BZ    CON103                                                           
*                                                                               
*        MUST SWITCH TO AOR                                                     
*        TO READ CONTRACT                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SADVDATA+14         AOR SE NUMBER                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    CON103                                                           
         MVC   KBAMSG,=CL60'** AOR SYSTEM NOT ACTIVE **'                        
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
CON103   CLC   KBAACT(3),=C'DIS'  SEE IF DISPLAY                                
         BNE   CON103I            NO                                            
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    CON103G                                                          
*                                                                               
CON103A  DS    0H                        HERE IF CONTRACT NOT FOUND             
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    CON103E                                                          
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    CON103E                                                          
         TM    SADVDATA+15,X'20'                                                
         BZ    CON103E                                                          
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
CON103E  B     ERROR                                                            
*                                                                               
*                                                                               
CON103G  DS    0H                                                               
         MVC   SAVKKEY,KEY                                                      
*                                                                               
CON103I  DS    0H                                                               
         MVC   KEY+27(4),SAVKKEY+27                                             
*                                                                               
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         CLI   SAVPRD,0       SEE IF LOOKING FOR A PRODUCT CONTRACT             
         BE    CON103I5                                                         
         CLC   PCONPRD,SAVPRD     MUST MATCH                                    
         BNE   CON103A        NOT FOUND                                         
*                                                                               
CON103I5 DS    0H                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    CON103L                                                          
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    CON103L                                                          
         TM    SADVDATA+15,X'20'                                                
         BZ    CON103L                                                          
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    SADVDATA+16,X'01'  SEE IF ACCESS TO AOR CONTRACT ALLOWED         
         BZ    CON103L                                                          
*                                                                               
         MVC   KBAMSG,=CL60'** ACCESS DENIED TO AOR CONTRACT **'                
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
CON103L  DS    0H                                                               
         LA    RF,IOAREA           RESTORE AREC                                 
         ST    RF,AREC                                                          
         MVC   DMWORK2,DMWORK                                                   
         MVI   ELCODE,X'21'              LOWER RATE EL  CODE       L02          
         MVI   TYPETRAN,C'L'             LOWER RATE EL  CODE       L02          
         CLC   KBAACT(4),=C'DISL'          DISPLAY RATES           L02          
         BE    DISPRATS                                            L02          
         MVI   ELCODE,X'22'             HIGHER RATE EL  CODE       L02          
         MVI   TYPETRAN,C'H'            HIGHER RATE EL  CODE       L02          
         CLC   KBAACT(4),=C'DISH'          DISPLAY RATES           L02          
         BE    DISPRATS                                            L02          
         MVI   ELCODE,X'24'             OPEN RATES                 L02          
         MVI   TYPETRAN,C'O'            OPEN RATE EL CODE          L02          
         CLC   KBAACT(4),=C'DISO'          DISPLAY RATES           L02          
         BE    DISPRATS                                            L02          
         MVI   TYPETRAN,C'R'             NORMAL ELEMENT CODE       L02          
         MVI   ELCODE,X'20'              NORMAL ELEMENT CODE       L02          
         CLC   KBAACT(4),=C'DISR'          DISPLAY RATES                        
         BE    DISPRATS                                                         
*                                                                   L05         
         OC    ELCODE,TWAKIND     LOW ORDER BITS ARE EITHER 01 FOR  L05         
         NI    ELCODE,X'27'       LOW / 02 FOR HIGH / 04 FOR OPEN   L05         
         CLC   KBAACT(3),=C'ADD'   SEE IF THIS IS AN ADD FUNCTION   L05         
         BNE   OKOKNT20            FOR HIGHER OR LOWER LEVELS       L05         
         LA    R2,PCONREC+33       SEE IF ELEMENT EXIST             L05         
         BAS   RE,GNEXTEL                                           L05         
         BNE   OKOKNT20            NONE FOUND                       L05         
         XC    KBAMSG,KBAMSG                                        L05         
         MVC   KBAMSG(L'KKMSG),KKMSG                                L05         
         FOUT  KBAMSGH                                                          
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,KBAACTH       CURSOR TO ACTION                                
         B     EXIT                                                 L05         
*                                                                               
KKMSG    DC    C'RATES ALREADY ESTABLISHED - DISPLAY THEN CHANGE'               
*                                                                               
GNEXTEL  SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    GNEXTELX            RETURN WITH CC =                             
         CLC   ELCODE,0(R2)                                                     
         BCR   8,RE                                                             
         B     GNEXTEL+2                                                        
GNEXTELX LTR   R2,R2              RETURN WITH CC NOT =                          
         BR    RE                                                               
**********                                                                      
OKOKNT20 DS    0H                                                   L05         
*                                                                               
         CLC   PCONMOD,TODAY                                                    
         BE    EDITCON                                                          
         SR    RE,RE                                                            
         IC    RE,PCONMODN                                                      
         LA    RE,1(RE)                                                         
         STC   RE,PCONMODN                                                      
         MVC   PCONMOD,TODAY                                                    
         B     EDITCON                                                          
         EJECT                                                                  
DISPRATS DS    0H                                                               
         MVC   ELSAVE,ELCODE     ELCODE MAY CHANGE IF NO RATES FOR  L02         
*                           HIGH OR LOWER RATES --WILL REVERT TO 20 L02         
         MVI   SAVPAG,1                                                         
         CLI   KBAACT+4,C' '                                                    
         BNH   DISP205                                                          
*                                                                               
         MVC   SAVPAG,KBAACT+4                                                  
         NI    SAVPAG,X'0F'                                                     
*                                                                               
DISP205  DS    0H                                                               
         ZIC   R4,SAVPAG                                                        
         BCTR  R4,R0                                                            
         MH    R4,=Y(RPSCRN)                                                    
         LA    R4,1(R4)                                                         
         STC   R4,RSTART                                                        
*                                                                               
DISP210  DS    0H                                                               
         MVI   ELNUM,1                                                          
*                                                                               
         GOTO1 VFOUTBLK,DMCB,RATLI1H,RATLAST            FOUT BLANKS             
*                                                                               
*              RATE BASIS LINES    GET X'20' RATE BASIS ELEMENTS                
*        GOTO1 VGETEL,DMCB,(X'20',PCONREC),DMCB+8                  L02          
         GOTO1 VGETEL,DMCB,(ELCODE,PCONREC),DMCB+8             L02              
*                                                                               
         CLI   DMCB,X'FF'                                                       
         BE    DISP349             NO RATES ELEMS FOUND                         
         LM    R3,R5,DMCB+8        BXLE                                         
         CLI   KBAMED,C'N'         NEWSPAPERS?                                  
         BNE   DISP200                                                          
         TM    16(R3),X'80'        OVERRIDE?                                    
         BO    DISP200                                                          
         CP    11(5,R3),=P'0'      TEST HAVE RATE                               
         BNE   DISP200                                                          
*                                                                               
*              LOOK UP RATE BASIS                                               
         L     RF,APUBIO                                                        
         CLI   0(RF),0             TEST PUBREC IN CORE                          
         BNE   DISP150                                                          
         MVC   KEY+27(4),SAVPUBA   PUBREC DISK ADDR                             
         BAS   RE,GETPUB                                                        
*                                                                               
DISP150  DS    0H                                                               
         CLI   NODISP,255          ONLY  DISPLAYING SPACE FOR H /L  L02         
         BE    DISP228             BYPASS                           L02         
         GOTO1 VRTLOOK,DMCB,0,(ELCODE,APUBIO),PCONREC,WORK2 L02                 
         LA    R2,RATRT1H                                                       
         SR    R3,R3                                                            
         CLI   DMCB,0              ERROR?                                       
         BE    *+12                                                             
         IC    R3,DMCB                                                          
         B     ERROR                                                            
*                                                                               
         LA    R3,WORK2                                                         
         LA    R5,WORK2+199                                                     
*                                                                               
DISP200  DS    0H                                                               
*                                                                               
         LA    R2,RATLI1H                                                       
         SR    R8,R8                                                            
DISP225  DS    0H                                                               
         CLI   NODISP,255          ONLY  DISPLAYING SPACE FOR H /L  L02         
         BE    DISP225P            BYPASS                           L02         
         CLC   ELNUM,RSTART        TEST                                         
         BL    DISP310                                                          
         LA    R9,RATEDGH                                                       
         CR    R2,R9               LAST RATE BASIS FIELD?                       
         BNH   *+12                                                             
         MVI   RTOVFL,C'Y'                                                      
         B     DISP350                                                          
*                                                                               
         MVC   8(1,R2),5(R3)       LBL IND                                      
         CLI   5(R3),C'N'         MEANS NET DOLLAR VOLUME                       
         BNE   *+8                                                              
         MVI   8(R2),C'$'          DISPLAY "N" AS "$"                           
         FOUT  (R2)                                                             
*                                                                               
DISP225P IC    R8,0(R2)            FIELD LEN                                    
         LA    R2,0(R8,R2)         NEXT FIELD                                   
         CLI   NODISP,255          ONLY  DISPLAYING SPACE FOR H /L  L02         
         BE    DISP228             BYPASS                           L02         
         FOUT  (R2)                                                             
*              LEVEL                                                            
         CLI   5(R3),C'N'              SEE IF NET $ VOLUME                      
         BNE   DISP225X                                                         
         MVI   8(R2),C'N'           PRECEED LEVEL WITH 'N'                      
*                                   AND LEVEL WITHOUT COMMAS                    
         EDIT  (P5,6(R3)),(7,9(R2)),ALIGN=LEFT                                  
         B     DISP227                                                          
*                                                                               
DISP225X DS    0H                                                               
         CP    6(5,R3),=P'999999'       SEE IF OVER 1,000,000                   
         BNH   DISP226                                                          
         EDIT  (P5,6(R3)),(8,8(R2)),ALIGN=LEFT                                  
         B     DISP227                                                          
*                                                                               
DISP226  EDIT  (P5,6(R3)),(8,8(R2)),ALIGN=LEFT,COMMAS=YES                       
DISP227  CP    6(5,R3),=P'0'                                                    
         BNE   DISP228                                                          
         CLI   5(R3),C'S'                                                       
         BE    DISP228                                                          
         MVC   8(4,R2),=C'OPEN'                                                 
         TM    16(R3),1            FLAT?                                        
         BZ    *+10                                                             
         MVC   8(4,R2),=C'FLAT'                                                 
DISP228  DS    0H                                                               
* NEXT FIELD - PERCENT                                                          
         IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)                                                      
         CLI   NODISP,255          ONLY  DISPLAYING SPACE FOR H /L  L02         
         BE    DISP230             BYPASS                           L02         
         FOUT  (R2)                                                             
         CLI   1(R3),42            OLD ELEM?                                    
         BL    DISP230                                                          
         EDIT  (P3,39(R3)),(5,8(R2)),2,ALIGN=LEFT                               
         CLC   8(3,R2),=C'.00'                                                  
         BNE   *+10                                                             
         MVC   8(3,R2),SPACES                                                   
         CLC   9(3,R2),=C'.00'                                                  
         BNE   *+10                                                             
         MVC   9(3,R2),SPACES                                                   
         CLC   10(3,R2),=C'.00'                                                 
         BNE   *+10                                                             
         MVC   10(3,R2),SPACES                                                  
*              RATE                                                             
DISP230  IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)                                                      
         CLI   NODISP,255          ONLY  DISPLAYING SPACE FOR H /L  L02         
         BE    DISP275             BYPASS                           L02         
         FOUT  (R2)                                                             
*****                                                                           
         CLC   17(2,R3),=C'R='                                                  
         BNE   DISP230A                                                         
         LA    R9,8(R2)                                                         
         B     DISP233                                                          
*****                                                                           
DISP230A TM    16(R3),X'40'        TOTAL RATE                                   
         BNZ   DISP250                                                          
         TM    16(R3),X'20'        UNIT RATE                                    
         BNZ   DISP232                                                          
*                                                                               
         CLI   KBAMED,C'N'         NEWSPAPERS?                                  
         BE    DISP232                                                          
         CLI   5(R3),C'I'                                                       
         BE    DISP232                                                          
         CLI   5(R3),C'L'                                                       
         BNE   DISP250                                                          
*        LINE RATE  5 DECIMALS                                                  
DISP232  DS    0H                                                               
         MVI   8(R2),C'U'          SET UNIT RATE IND                            
         LA    R9,9(R2)                                                         
         CLI   KBAMED,C'N'         IF NON-NEWS                                  
         BNE   DISP233                                                          
         CLI   17(R3),C' '         OR SPACE BUY                                 
         BH    DISP233                                                          
         LA    R9,8(R2)            ELSE ASSUMED TO BE UNIT RATE                 
DISP233  DS    0H                                                               
         TM    16(R3),X'10'     NET RATE                                        
         BNO   *+12                                                             
         MVI   0(R9),C'N'       NET INDICATIOR                                  
         LA    R9,1(R9)                                                         
         TM    16(R3),X'02'       S RATE                                        
         BNO   *+12                                                             
         MVI   0(R9),C'S'       NET INDICATIOR                                  
         LA    R9,1(R9)                                                         
         TM    16(R3),X'04'     SEE IF C RATE                                   
         BNO   *+12                                                             
         MVI   0(R9),C'C'                                                       
         LA    R9,1(R9)                                                         
*                                                                               
         EDIT  (P5,11(R3)),(13,(R9)),5,ALIGN=LEFT,FLOAT=-,COMMAS=YES            
*                                                                               
         CLI   5(R3),C'I'      INCH LEVEL MUST BE INCH RATE                     
         BE    DISP234                                                          
         TM    16(R3),X'08'    SEE IF INCH RATE                                 
         BZ    DISP235                                                          
DISP234  AR    R9,R0          ADD LENGTH OF EDIT OUTPUT                         
         MVC   0(2,R9),=C'/I'                                                   
         B     DISP275                                                          
*                                                                               
DISP235  AR    R9,R0          ADD LENGTH OF EDIT OUTPUT                         
         MVC   0(2,R9),=C'/L'   MUST BE LINE RATE                               
         B     DISP275                                                          
*                                                                               
*              2 DECIMAL RATE                                                   
DISP250  DS    0H                                                               
         CLI   KBAMED,C'N'                                                      
         BNE   DISP251                                                          
         CLI   17(R3),C' '                                                      
         BH    DISP251                                                          
         MVI   8(R2),C'T'                                                       
         LA    R9,9(R2)                                                         
         B     *+8                                                              
DISP251  DS    0H                                                               
         LA    R9,8(R2)                                                         
         CLI   NODISP,255          ONLY  DISPLAYING SPACE FOR H /L  L02         
         BE    DISP228             BYPASS                           L02         
         TM    16(R3),X'10'     NET RATE                            L01         
         BNO   *+12                                                 L01         
         MVI   0(R9),C'N'       NET INDICATIOR                      L01         
         LA    R9,1(R9)                                             L01         
         TM    16(R3),X'02'       S RATE                            L03         
         BNO   *+12                                                 L03         
         MVI   0(R9),C'S'       NET INDICATIOR                      L03         
         LA    R9,1(R9)                                             L03         
         TM    16(R3),X'04'     SEE IF C RATE                       L04         
         BNO   *+12                                                 L04         
         MVI   0(R9),C'C'                                           L04         
         LA    R9,1(R9)                                             L04         
*        CP    11(5,R3),=PL5'99999999' ONE MILLION                BUG01         
*        BH    OVER1MIL                                           BUG01         
*                                                                 BUG01         
         EDIT  (P5,11(R3)),(13,(R9)),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES            
**NOGOOD EDIT  (P5,11(R3)),(9,(R9)),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES 01          
*        B     DISP275                                            BUG01         
*VER1MIL EDIT  (P5,11(R3)),(9,(R9)),2,ALIGN=LEFT,FLOAT=-          BUG01         
*                                                                               
DISP275  IC    R8,0(R2)       DESCRIPTION                                       
         LA    R2,0(R8,R2)         NEXT FIELD                                   
         FOUT  (R2)                                                             
*****                                                                           
         CLC   17(2,R3),=C'R='                                                  
         BNE   DISP275F                                                         
         MVC   8(5,R2),17(R3)                                                   
         LA    R1,13(R2)                                                        
         CLI   21(R3),C' '                                                      
         BH    DISP275A                                                         
         BCTR  R1,0                                                             
         CLI   20(R3),C' '                                                      
         BH    DISP275A                                                         
         BCTR  R1,0                                                             
DISP275A MVI   0(R1),C','                                                       
         MVC   1(12,R1),22(R3)                                                  
         B     DISP285                                                          
*                                                                               
*****                                                                           
DISP275F MVC   8(17,R2),17(R3)     DESCRIPTION                                  
         CLI   17(R3),X'FF'                                                     
         BNE   DISP280                                                          
*                                  SPECIAL OUTDOOR FIELDS                       
         LA    RF,16(R2)                                                        
         MVC   8(8,R2),=C'SRI=SPC,'                                             
         CP    18(3,R3),=P'99999'                                               
         BE    DISP276                                                          
         LA    RF,12(R2)                                                        
         LA    R9,18(R3)                                                        
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
DISP276  DS    0H                                                               
         LA    R9,21(R3)                                                        
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    R9,24(R3)                                                        
         LA    RF,1(RF)                                                         
         BAS   RE,EDT                                                           
*                                                                               
DISP280  DS    0H                                                               
         LA    R9,WORK2+199                                                     
         CR    R5,R9               RATELOOK?                                    
         BNE   DISP285                                                          
         CP    6(5,R3),=P'0'       OPEN LEVEL?                                  
         BE    DISP285                                                          
         MVC   8(5,R2),=C'OPEN='                                                
         EDIT  (P5,34(R3)),(8,13(R2)),5,ALIGN=LEFT                              
DISP285  DS    0H                                                               
*        LR    RE,R2                                                L01         
*        LA    RF,128                                               L01         
*        STC   RF,TM+1                                                          
*MVI     MVI   8(RE),C'0'                                           L01         
*M       TM    16(R3),X'80'                                         L01         
*        BNO   *+8                                                  L01         
*        MVI   8(RE),C'1'                                           L01         
*        LA    RE,1(RE)                                             L01         
*        SRL   RF,1                                                 L01         
*        STC   RF,TM+1                                              L01         
*        CLI   TM+1,0                                               L01         
*        BNE   BMVI                                                 L01         
*                                                                   L01         
*              EFFECTIVE DATE                                                   
         IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)         NEXT FIELD                                   
         FOUT  (R2)                                                             
         LR    R9,R2                                                            
*                                                                               
         CLI   NODISP,255          ONLY  DISPLAYING SPACE FOR H /L  L02         
         BE    DISP290             BYPASS                           L02         
*                                                                               
         OC    2(3,R3),2(R3)       EFFECTIVE DATE?                              
         BZ    DISP290                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(3,2(R3)),(5,8(R2))                                 
         LA    R9,8(R9)        BUMP PAST DATE                                   
*                                                                               
DISP290  DS    0H                                                               
*******  CLI   KBAMED,C'N'                                                      
*******  BE    DISP300                                                          
         CLI   34(R3),C'A'             SEE IF PRODUCT IN PRBOPEN   L06          
         BL    DISP300                                             L06          
         MVI   8(R9),C'-'                                                       
         MVC   9(3,R9),34(R3)           PRODUCT IN PRBOPEN                      
*                                                                               
DISP300  DS    0H                                                               
         ZIC   R8,0(R2)                                                         
         LA    R2,0(R8,R2)         NEXT FIELD                                   
DISP310  DS    0H                                                               
         IC    R4,ELNUM            BUMP ELEM COUNTER                            
         LA    R4,1(R4)                                                         
         STC   R4,ELNUM                                                         
DISP315  DS    0H                                                               
         IC    R4,1(R3)            RATE BASIS ELEM LEN                          
         BXLE  R3,R4,*+8           NEXT ELEM                                    
         B     DISP350                                                          
*                                                                               
         CLC   ELCODE,0(R3)        RATE BASIS ELEM?                L02          
         BNE   DISP315                                                          
         B     DISP225                                                          
TYPETRAN DC    X'0'          MAY HAVE TO BE IN TWA                              
ELSAVE   DC    X'0'          MAY HAVE TO BE IN TWA                              
ELCODE   DC    X'0'          ELEMENT CODE                          L02          
NODISP   DC    X'0'   IF ON NO X'21' OR 22'  DISPLAY SPACE OF 20   L02          
         SPACE 3                                                                
*                                                                L02            
* NO ELEMENTS FOUND // CHECK TO SEE IF LOWER OR HIGHER LEVELS    L02            
*  WERE REQUESTED -- IF SO DISPLAY 20 ELEMENT DESCRIPTIONS       L02            
*                                                                L02            
DISP349  CLI   ELCODE,X'20'                                      L02            
         BE    DISP350                                           L02            
         MVI   ELCODE,X'20'   FORCE TO DISPLAY 20 DESCRIPTIONS   L02            
         MVI   NODISP,255                                        L02            
         B     DISP210                                           L02            
*                                                                L02            
*                                                                L02            
*                                                                L02            
         SPACE 3                                                                
DISP350  LA    R2,KBAACTH                                                       
         MVI   NODISP,0                                          L02            
         SR    R8,R8                                                            
         OI    4(R2),X'20'         VALID BIT                                    
         IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)                                                      
         CLI   0(R2),0                                                          
         BNE   *-16                                                             
         MVC   KBAMSG,SPACES                                                    
         LA    R2,KBAACTH                                                       
*                            SET OFF X'40' RATES DISPLAYED                      
*                            AND X'80' BIT - CONTRACT DISPLAYED                 
         NI    TWAKIND,X'3F'                                                    
*                                                                               
         CLC   KBAACT(3),=C'CHA'                                                
         BNE   DISP500                                                          
         OI    TWAKIND,X'40'        SET RATES DISPLAYED                         
*                                                                               
         MVC   KBAMSG(14),=C'RATES CHANGED '                        L02         
         LA    RF,KBAMSG+14                                         L02         
         B     DISP600                                                          
*                                                                               
DISP500  DS    0H                                                               
         CLC   KBAACT(3),=C'ADD'                                                
         BNE   DISP510                                                          
         OI    TWAKIND,X'40'        SET RATES DISPLAYED                         
         MVC   KBAMSG(12),=C'RATES ADDED '                          L02         
         LA    RF,KBAMSG+12                                         L02         
         B     DISP600                                                          
*                                                                               
DISP510  DS    0H                                                               
         MVC   KBAMSG(16),=C'RATES DISPLAYED '                      L02         
         LA    RF,KBAMSG+16                                         L02         
         OI    TWAKIND,X'40'        SET RATES DISPLAYED                         
DISP600  DS    0H                                                               
*                                                                               
*                                                                   L02         
         TM     TWAKIND,1           LOWER LEVEL                     L02         
         BNO    *+10                                                L02         
         MVC    0(15,RF),=C'FOR LOWER LEVEL'                        L02         
         TM     TWAKIND,2           HIGHER LEVEL                    L02         
         BNO    *+10                                                L02         
         MVC    0(16,RF),=C'FOR HIGHER LEVEL'                       L02         
         TM     TWAKIND,4           OPEN RATES                      L02         
         BNO    *+10                                                L02         
         MVC    0(16,RF),=C'FOR OPEN RATES  '                       L02         
*                                                                   L02         
*                                                                   L02         
         CLI   RTOVFL,C'Y'                                                      
         BNE   DISP610                                                          
         LA    R5,KBAMSG+L'KBAMSG-1                                             
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVC   2(24,R5),=C'(USE ''DISRN'' MORE RATES)'                          
         MVC   11(1,R5),TYPETRAN           TYPE TRANS ACTION     L02            
         ZIC   RF,SAVPAG                                                        
         LA    RF,1(RF)                                                         
         STC   RF,12(R5)                                                        
         OI    12(R5),X'F0'                                                     
*                                                                               
DISP610  DS    0H                                                               
         CLC   KBAACT(3),=C'DIS'                                                
         BE    DISP630                                                          
*                                  AFTER CHANGE SORT RATE ELEMS ON DATE         
         LA    R2,PCONREC+33                                                    
         SR    R4,R4                                                            
         XC    FULL,FULL                                                        
DISP612  DS    0H                                                               
         CLC   ELCODE,0(R2)                                                     
         BE    DISP616                                                          
         B     DISP618                                                          
*                                                                               
DISP614  DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     DISP612                                                          
*                                                                               
DISP616  DS    0H                                                               
         OC    FULL,FULL                                                        
         BNZ   *+8                                                              
         ST    R2,FULL             A(FIRST RATE ELEM)                           
         LA    R4,1(R4)            BUMP ELEM COUNT                              
         B     DISP614                                                          
*                                                                               
DISP618  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    DISP620                                                          
         LTR   R4,R4               TEST HAVE REACHED FIRST RATE ELEM            
         BZ    DISP614                                                          
*                                                                               
DISP620  DS    0H                                                               
         LTR   R4,R4                                                            
         BNP   DISP625                                                          
         MVC   DMCB(4),FULL                                                     
         GOTO1 =V(XSORT),DMCB,,(R4),42,5,0,RR=RELO40                            
*                                                                               
DISP625  DS    0H                                                               
         MVC   DMWORK(96),DMWORK2                                               
         MVC   KEY+27(4),SAVKKEY+27                                             
         LA    RF,PCONREC          READ INTO CONREC                             
         ST    RF,AREC                                                          
         BAS   RE,PUTREC                                                        
         LA    RF,IOAREA           RESTORE AREC                                 
         ST    RF,AREC                                                          
DISP630  DS    0H                                                               
         LA    R2,KBAACTH                                                       
         FOUT  KBAACTH,SPACES,8                                                 
         B     EXIT                                                             
*                                                                               
         TITLE 'PPCON40 - PRINTPAK CONTRACT RATE BASIS LINE EDIT'               
EDITCON  DS    0H                                                               
*              VALIDATE RATE BASIS FIELDS                                       
*              EDIT RATE BASIS FIELDS - BUILD ELEMENTS                          
RATE100  LA    R2,RATLI1H          FIRST FIELD                                  
         LA    R3,2                INVALID INPUT ERROR                          
         XC    HALF2,HALF2         USED TO COUNT RATES                          
         SR    R8,R8                                                            
*                                                                  L02          
* MUST RE-ESTABLISH ELCODE                                         L02          
*                                                                  L02          
         MVI   ELCODE,X'20'   DEFAULT                              L02          
         OC    ELCODE,TWAKIND     LOW ORDER 2 BITS ARE EITHER      L02          
         NI    ELCODE,X'27'       01 FOR LOW  02 FOR HIGH OR 04    L02          
*                                 FOR OPEN                                      
*        ABOVE TURN OFF ANY OTHER BITS                             L02          
*                                                                  L02          
*                                                                               
         BAS   RE,DELELEM                                                       
*                                  SEE IF ANY RATE DATA PRESENT                 
         LA    R4,RATEDGH                                                       
         SR    RE,RE                                                            
RATE101  CLI   5(R2),0             ANY                                          
         BNE   RATE102             YES                                          
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,R4                                                            
         BNH   RATE101                                                          
*                                  NO RATE DATA GIVEN                           
         CLC   KBAACT(3),=C'ADD'                                                
         BNE   EDITSTD                                                          
         LA    R3,1                MISSING DATA ERROR                           
         LA    R2,RATLI1H                                                       
         B     ERROR                                                            
RATE102  DS    0H                                                               
         LA    R2,RATLI1H                                                       
         LA    R4,WORK2            ELEMENT AREA                                 
*              EDIT RATE BASIS LINE                                             
*        MVC   0(2,R4),=X'202A'    RATE BASIS ELEM CODE AND LEN                 
*                                                                  L02          
         MVI   1(R4),X'2A'     LENGTH                              L02          
         MVC   0(1,R4),ELCODE  ELEMENT                             L02          
*                                                                  L02          
RATE200  XC    WORK2+2(40),WORK2+2                                              
         CLI   5(R2),0             CHK FOR INPUT IN LEVEL FLD                   
         BNE   RATE220             YES - PROCESS RATE LINE                      
*                                                                               
*                               NO INPUT IN LEVEL IND                           
*                               CHECK FOR INPUT OTHER RATE FIELDS               
*                               IF FOUND ERROR - LEVEL MUST BE GIVEN            
         LR    R5,R2                                                            
         ZIC   R8,0(R5)         BUMP TO LEVEL FIELD                             
         AR    R5,R8                                                            
         CLI   5(R5),0                                                          
         BNE   RATE205                                                          
         ZIC   R8,0(R5)         BUMP TO PCT FIELD                               
         AR    R5,R8                                                            
         CLI   5(R5),0                                                          
         BNE   RATE205                                                          
         ZIC   R8,0(R5)         BUMP TO RATE FIELD                              
         AR    R5,R8                                                            
         CLI   5(R5),0                                                          
         BNE   RATE205                                                          
         ZIC   R8,0(R5)         BUMP TO SPACE FIELD                             
         AR    R5,R8                                                            
         CLI   5(R5),0                                                          
         BNE   RATE205                                                          
         ZIC   R8,0(R5)         BUMP TO EFFECTIVE DATE FIELD                    
         AR    R5,R8                                                            
         CLI   5(R5),0          CHECK FOR INPUT                                 
         BE    RATE210                                                          
RATE205  LA    R3,1                  MISSING INPUT                              
         B     ERROR                                                            
*                                                                               
RATE210  LA    R2,RATELEN(R2)         BUMP TO NEXT RATE LINE                    
         LA    R5,RATLIGH          LAST LINE                                    
         CR    R2,R5                                                            
         BH    RTLOOK                                                           
         B     RATE200             GO DO NEXT LINE                              
*                                                                               
*              LVL IND                                                          
RATE220  ZAP   34(5,R4),=P'0'      OPEN RATE                                    
         MVC   5(1,R4),8(R2)       LVL IND                                      
         CLI   8(R2),C'L'                                                       
         BE    RATE250                                                          
         CLI   8(R2),C'$'                                                       
         BE    RATE250                                                          
         CLI   8(R2),C'P'                                                       
         BE    RATE250                                                          
         CLI   8(R2),C'S'          SPECIAL?                                     
         BE    RATE250                                                          
         CLI   8(R2),C'X'                                                       
         BE    RATE250                                                          
         CLI   8(R2),C'I'                                                       
         BE    RATE250                                                          
         CLI   8(R2),C'U'                                                       
         BNE   ERROR                                                            
*              VALID LVL IND                                                    
*              EDIT LEVEL                                                       
RATE250  IC    R8,0(R2)            FIELD LEN                                    
         LA    R2,0(R8,R2)         NEXT FIELD                                   
         ST    R2,LEVADDR          SAVE ADDRESS OF LEVEL FIELD                  
         ZAP   6(5,R4),=P'0'                                                    
         SR    R1,R1                                                            
         CLC   8(4,R2),=C'FLAT'                                                 
         BNE   *+12                                                             
         OI    16(R4),1                                                         
         B     RATE275                                                          
         CLC   8(4,R2),=C'OPEN'                                                 
         BE    RATE275                                                          
         CLI   5(R2),0             LEN                                          
         BE    RATE275                                                          
*                                                                               
         CLC   8(3,R2),=C'ADV'                                                  
         BNE   RATE270                                                          
         CLC   SADVDATA(2),AGYALPHA         DISALLOW IF I AM THE AOR            
         BE    ERROR                                                            
         TM    SADVDATA+15,X'04'            CHK LEVEL LOOK-UP ALLOWED           
         BZ    ERROR                                                            
         ZAP   6(5,R4),=P'-2'              TRIGGER ADV LEVEL LOOK-UP            
         B     RATE280                                                          
*                                                                               
RATE270  DS    0H                                                               
         IC    R8,5(R2)            LEN                                          
         LR    R9,R2                                                            
         CLI   8(R2),C'N'    ALLOW "N" BEFORE LEVEL IF PRBLIND IS $             
         BNE   RATE270D                                                         
         CLI   5(R4),C'$'                                                       
         BNE   ERROR                                                            
         MVI   5(R4),C'N'       SWITCH $ TO "N"                                 
         SH    R8,=H'1'                                                         
         BNP   ERROR                                                            
         LA    R9,1(R9)                                                         
RATE270D DS    0H                                                               
         GOTO1 VCASHVAL,DMCB,8(R9),(R8)                                         
*                                                                               
         CLI   DMCB,0              VALID?                                       
         BNE   ERROR                                                            
         L     R1,DMCB+4           LEVEL                                        
         LTR   R1,R1                                                            
         BM    ERROR                                                            
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
RATE275  CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   6(5,R4),DUB+3       LEVEL TO RATE BASIS ELEMENT                  
* NEXT FIELD - PERCENT                                                          
RATE280  IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)                                                      
         IC    R8,5(R2)            INPUT LENGTH                                 
         ZAP   39(3,R4),=P'0'                                                   
         CLI   5(R2),0                                                          
         BE    RATE285                                                          
         GOTO1 VCASHVAL,DMCB,8(R2),(R8)   PERCENT                               
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
         L     R9,DMCB+4                                                        
         LTR   R9,R9                                                            
         BM    ERROR                                                            
         C     R9,=F'9999'                                                      
         BH    ERROR                                                            
         CVD   R9,DUB                                                           
         MVC   39(3,R4),DUB+5                                                   
*              NEXT FIELD - RATE                                                
RATE285  IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)         NEXT FIELD - RATE                            
         ST    R2,RATADDR          SAVE ADDRESS OF RATE FIELD                   
         IC    R8,5(R2)            INPUT LENGTH                                 
         ZAP   11(5,R4),=P'0'                                                   
         SR    R9,R9                                                            
         CLI   5(R2),0             NO INPUT?                                    
         BE    RATE350                                                          
*                                                                               
         CLC   8(3,R2),=C'ADV'                                                  
         BNE   RATE285D                                                         
         CLC   SADVDATA(2),AGYALPHA  SEE IF I AM THE AOR                        
         BE    ERROR                                                            
         TM    SADVDATA+15,X'02'    CHK ADV RATE LOOK-UP ALLOWED                
         BZ    ERROR                                                            
         ZAP   11(5,R4),=P'-2'      TRIGGER RATE LOOK-UP                        
         B     RATE380                                                          
*                                                                               
RATE285D OI    16(R4),X'80'        SET ON MANUAL RATE INPUT                     
         CLI   8(R2),C'N'       WAS NET FIRST                       L01         
         BNE   *+8                                                  L01         
         OI    16(R4),X'10'     NET INDICATIOR                      L01         
*                                                                   L01         
         CLI   9(R2),C'N'       WAS NET SECOND                      L01         
         BNE   *+8                                                  L01         
         OI    16(R4),X'10'     NET INDICATIOR                      L01         
*                                                                   L01         
         CLI   8(R2),C'S'       WAS   S FIRST                       L03         
         BNE   *+8                                                  L03         
         OI    16(R4),X'02'     NET INDICATIOR                      L03         
*                                                                   L03         
         CLI   9(R2),C'S'       WAS NET SECOND                      L03         
         BNE   *+8                                                  L03         
         OI    16(R4),X'02'     NET INDICATIOR                      L03         
         CLI   8(R2),C'C'       WAS  C  FIRST                       L04         
         BNE   *+8                                                  L04         
         OI    16(R4),X'04'     COMMISSION ONLY                     L04         
*                                                                   L04         
         CLI   9(R2),C'C'       WAS C   SECOND                      L04         
         BNE   *+8                                                  L04         
         OI    16(R4),X'04'     COMMISSION ONLY                     L04         
*                                                                   L01         
CLI9R2T  CLI   9(R2),C'T'       TOTAL RATE                          L01         
         BNE   *+8                                                  L01         
         OI    16(R4),X'40'      TOTAL                              L01         
         CLI   9(R2),C'U'        UNIT RATE                          L01         
         BNE   *+8                                                  L01         
         OI    16(R4),X'20'                                         L01         
*                                                                   L01         
         CLI   8(R2),C'T'       TOTAL RATE                          L01         
         BNE   *+8                                                  L01         
         OI    16(R4),X'40'      TOTAL                              L01         
         CLI   8(R2),C'U'        UNIT RATE                          L01         
         BNE   *+8                                                  L01         
         OI    16(R4),X'20'                                         L01         
         TM    16(R4),X'60'      CANNOT HAVE T AND U                L01         
         BO    ERROR                                                L01         
         TM    16(R4),X'12'      CANNOT HAVE S AND N                L03         
         BO    ERROR                                                L03         
         TM    16(R4),X'06'      CANNOT HAVE S AND C                L04         
         BO    ERROR                                                L04         
         TM    16(R4),X'14'      CANNOT HAVE N AND C                L04         
         BO    ERROR                                                L04         
*                                                                   L01         
         TM    16(R4),X'40'   WAS A 'T' ENCOUNTERED                 L01         
         BO    RATE300                                              L01         
         TM    16(R4),X'20'   WAS A U ENCOUNTERED                   L01         
         BO    RATE286                                              L01         
*                                                                   L01         
*******                                                             L01         
*                                                                               
         CLI   KBAMED,C'N'         IF NON-NEWS ASSUME TOTAL REATE               
         BNE   RATE300                                                          
         ZIC   RF,0(R2)                                                         
         LA    RF,0(RF,R2)         POINT TO NEXT FIELD (SPACE)                  
*****                                                                           
         CLC   8(2,RF),=C'R='                                                   
         BE    RATE286                                                          
*****                                                                           
         CLI   5(RF),0             TEST ANY INPUT                               
         BNE   RATE300             YES - RATE WILL BNE'TOTAL'                   
*                                                                               
*              LINE RATE - 5 DECIMALS                                           
RATE286  DS    0H                                                               
         LA    R9,8(R2)                                                         
         CLI   0(R9),C'.'                                                       
         BE    RATE288                                                          
         CLI   0(R9),X'EF'   IS IT NUMERIC                          L01         
         BH    RATE288                                              L01         
         BCTR  R8,R0               YES - ADTRST LENGTH                          
         LA    R9,1(R9)                                                         
         CLI   0(R9),C'.'                                                       
         BE    RATE288                                                          
         CLI   0(R9),X'EF'                                          L01         
         BH    RATE288                                              L01         
         BCTR  R8,0                                                             
         LA    R9,1(R9)                                                         
RATE288  DS    0H                                                               
         OI    16(R4),X'20'        SET UNIT RATE                                
*                              MOVE REMAINING INPUT TO MYTEMP                   
         XC    MYTEMP,MYTEMP                                                    
         BCTR  R8,0                                                             
         EX    R8,MVRAT                                                         
         B     *+10                                                             
*                                                                               
MVRAT    MVC   MYTEMP(0),0(R9)    EXECUTED                                      
*                                                                               
         SR    R1,R1           USED FOR INPUT LENGTH FOR CASHVAL                
*                                                                               
         LA    R5,MYTEMP                                                        
RATE290  CLI   0(R5),0                                                          
         BE    RATE294                                                          
         CLI   0(R5),C'/'                                                       
         BE    RATE292                                                          
         LA    R1,1(R1)                                                         
         LA    R5,1(R5)                                                         
         B     RATE290                                                          
*                                                                               
RATE292  CLI   1(R5),C'L'      SEE IF LINE RATE                                 
         BNE   RATE293                                                          
         CLI   5(R4),C'I'   DISALLOW LINE RATE FOR INCH LEVEL IND               
         BE    ERROR                                                            
         B     RATE294                                                          
*                                                                               
RATE293  DS    0H                                                               
         CLI   1(R5),C'I'                                                       
         BNE   ERROR                                                            
         CLI   5(R4),C'L'    DISALLOW INCH RATE FOR LINE LEVEL IND              
         BE    ERROR                                                            
         OI    16(R4),X'08'     INCH RATE INPUT                                 
*                                                                               
RATE294  DS    0H                                                               
         LR    R8,R1                                                            
         MVC   0(2,R5),=C'    '   JUST IN CASE                                  
*                                                                               
         GOTO1 VCASHVAL,DMCB,(5,MYTEMP),(R8)                                    
******** GOTO1 VCASHVAL,DMCB,(5,(R9)),(R8)                                      
         B     RATE325                                                          
*              NON-LINE RATE - 2 DECIMALS                                       
RATE300  DS    0H                                                               
         LA    R9,8(R2)                                                         
         CLI   0(R9),C'.'                                                       
         BE    RATE301                                                          
         CLI   0(R9),X'EF'   IS IT NUMERIC                          L01         
         BH    RATE301                                              L01         
         BCTR  R8,R0               YES - SHORTEN LENGTH                         
         LA    R9,1(R9)                                                         
         CLI   0(R9),C'.'                                                       
         BE    RATE301                                                          
         CLI   0(R9),X'EF'                                          L01         
         BH    RATE301                                              L01         
         BCTR  R8,0                                                             
         LA    R9,1(R9)                                                         
RATE301  DS    0H                                                               
         OI    16(R4),X'40'        SET TOTAL RATE IND                           
RATE302  DS    0H                                                               
         GOTO1 VCASHVAL,DMCB,(R9),(R8)                                          
*                                                                               
*                                                                               
RATE325  CLI   DMCB,0              VALID?                                       
         BNE   ERROR                                                            
         L     R9,DMCB+4           RATE                                         
         LTR   R9,R9                                                            
         BNZ   RATE330                                                          
         LA    R9,1                SET ZERO TO -.01                             
         LCR   R9,R9               TO PREVENT RATELOOK                          
         B     RATE350                                                          
*                                                                               
RATE330  DS    0H                                                               
         CLI   8(R2),C'P'          TAKE PCT DISCOUNT OFF RATE                   
         BNE   RATE350                                                          
         ZAP   DUB,39(3,R4)        PCT                                          
         CVB   R5,DUB                                                           
         LH    R0,=H'10000'                                                     
         SR    R0,R5                                                            
         LR    R1,R9                                                            
         MR    R0,R0                                                            
         LH    RF,=H'10000'                                                     
         BAS   RE,RTDIV                                                         
         LR    R9,R1                                                            
*                                                                               
*                                                                               
RATE350  CVD   R9,DUB                                                           
         MVC   11(5,R4),DUB+3      RATE                                         
*              DESCRIPTION                                                      
RATE380  IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)         NEXT FIELD - DESCRIPTION                     
RATE400  BAS   RE,MOVE                                                          
*****                                                                           
         CLC   WORK(2),=C'R='                                                   
         BNE   RATE425                                                          
*                                                                               
         LA    R1,17(R4)                                                        
         LA    R9,WORK                                                          
         LA    R8,6                                                             
RATE410  CLI   0(R9),C','                                                       
         BE    RATE415                                                          
         CLI   0(R9),0                                                          
         BE    RATE415                                                          
         MVC   0(1,R1),0(R9)                                                    
         LA    R1,1(R1)                                                         
         LA    R9,1(R9)                                                         
         BCT   R8,RATE410                                                       
         B     ERROR                   RCODE TOO LONG                           
*                                                                               
RATE415  MVC   22(12,R4),1(R9)         MOVE DESCRIPTION                         
         OC    17(17,R4),SPACES                                                 
         CLC   19(3,R4),SPACES                                                  
         BE    ERROR                  NO RATE CODE                              
*                                                                               
         CLI   5(R4),C'L'                                                       
         BE    RATE425A                                                         
         CLI   5(R4),C'I'                                                       
         BE    RATE425A                                                         
*                                                                               
         CLI   5(R4),C'$'           $ VOLUME                                    
         BE    RATE416                                                          
         CLI   5(R4),C'N'           NET $ VOLUME                                
         BE    RATE416                                                          
         CLI   5(R4),C'P'           PAGES                                       
         BE    RATE416                                                          
         CLI   5(R4),C'X'           TIMES                                       
         BNE   RATE417                                                          
*                                                                               
RATE416  CP    11(5,R4),=P'0'        SEE IF RATE ENTERED                        
         BNE   RATE425A              ALLOW $,P,X                                
*                                                                               
RATE417  MVI   ERRAREA,X'FF'                                                    
         MVC   KBAMSG,SPACES                                                    
         MVC   KBAMSG(32),=C'INVALID LVL IND - MUST BE L OR I'                  
         LH    R4,=AL2(CONDS1H-CONLI1H)                                         
         SR    R2,R4                                                            
         B     EXIT                                                             
*****                                                                           
RATE425  MVC   17(17,R4),WORK      DESCRIPTION                                  
RATE425A CLI   KBAMED,C'O'                                                      
         BNE   RATE450                                                          
         LA    R3,2                                                             
         BAS   RE,CHKOUT                                                        
         BNZ   ERROR                                                            
*              EFFECTIVE DATE                                                   
RATE450  IC    R8,0(R2)            FIELD LEN                                    
         LA    R2,0(R8,R2)         NEXT FIELD - EFF. DATE                       
         CLI   5(R2),0             INPUT?                                       
         BE    RATE550                                                          
*                                                                               
         GOTO1 VDATVAL,DMCB,8(R2),DUB   EFFECTIVE DATE                          
         LA    R9,8(R2)                                                         
         CLI   0(R9),C'-'                                                       
         BE    RATE475                                                          
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,DUB),(3,2(R4))                                   
*                                                                               
RATE460  CLI   0(R9),0                                                          
         BE    RATE500                                                          
         CLI   0(R9),C' '                                                       
         BE    RATE500                                                          
         CLI   0(R9),C'-'                                                       
         BE    RATE475                                                          
         LA    R9,1(R9)                                                         
         B     RATE460                                                          
*                                                                               
RATE475  DS    0H                                                               
         CLI   KBAMED,C'N'        NON NEWS                                      
         BNE   RATE476                                                          
         CP    11(5,R4),=P'0'    SEE IF RATE ENTERED                            
         BE    ERROR             MUST BE TO ACCOMDATE PRODUCT                   
*                                FOR NEWSPAPERS                                 
*              RATELOOK WILL TRY TO FIND THE OPEN RATE                          
*              AND STORE IT IN RPBOPEN  - I CAN'T ALLOW THAT                    
*              WHEN THEY ENTER A PRODUCT FOR NEWSPAPERS                         
*                                                                               
RATE476  XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),KBAMED                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),SAVKKEY+4          CLIENT                               
         MVC   KEY+7(3),1(R9)                                                   
         OC    KEY+7(3),=C'   '                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(10),KEYSAVE                                                  
         BE    RATE480                                                          
*                                                                               
         CLI   SAVCLTPR+5,C'1'        SEE IF MASTER CLIENT                      
         BE    RATE480                I WONT FIND THE PRODUCT                   
*                                                                               
         LA    R3,15        PRODUCT NOT FOUND                                   
         B     ERROR                                                            
*                                                                               
RATE480  MVC   34(3,R4),KEYSAVE+7  PRODUCT IN PRBOPEN                           
RATE500  DS    0H                                                               
*              ADD RATE BASIS ELEMENT                                           
RATE550  DS    0H                                                               
         CP    6(5,R4),=P'-2'   CHK ADV LEVEL AND/OR RATE LOOK-UP               
         BE    RATE560                                                          
         CP    11(5,R4),=P'-2'                                                  
         BE    RATE560                                                          
         B     RATE600                                                          
*                                                                               
RATE560  DS    0H                                                               
         BAS   RE,CKADVC                                                        
         CLI   ERRAREA,X'FF'                                                    
         BNE   RATE600                                                          
         L     R2,LEVADDR                                                       
         CP    6(5,R4),=P'-2'                                                   
         BE    EXIT                                                             
         L     R2,RATADDR                                                       
         B     EXIT                                                             
*                                                                               
RATE600  DS    0H                                                               
         ST    R4,ELADDR                                                        
         SPACE 2                                                                
         BAS   RE,ADELEM    ELEMENTS ARE ADDED TO THE CONTRACT RECORD           
*                           HERE BECAUSE RATELOOK WHICH IS BRANCHED TO          
*                           WHEN ALL LINES ARE PROCESSED REQUIRES ALL           
*                           ELEMENTS TO BE PRESENT WHEN DOING AUTO RATE         
*                           LOOKUPS                                             
         SPACE 2                                                                
         CLI   DMCB,X'FF'                                                       
         BE    OVFERR                                                           
         LH    R0,HALF2                                                         
         AH    R0,=H'1'                                                         
         STH   R0,HALF2            BUMP RATE LINE COUNTER                       
         IC    R8,0(R2)                                                         
         LA    R2,0(R8,R2)         NEXT FIELD                                   
*                                                                               
         LA    R5,RATEDGH                                                       
         CR    R2,R5               LAST RATE BASIS LINE?                        
         BH    RTLOOK                                                           
         B     RATE200             GO DO NEXT LINE                              
         EJECT                                                                  
         SPACE 3                                                                
RTLOOK   DS    0H                                                               
         MVC   KEY+27(4),SAVPUBA   RE-READ PBU REC                              
         BAS   RE,GETPUB                                                        
         LA    R3,MAXRTS                                                        
*        GOTO1 VRTLOOK,DMCB,0,APUBIO,PCONREC,((R3),ARTLKWRK)     L02            
         GOTO1 VRTLOOK,DMCB,0,(ELCODE,APUBIO),PCONREC,((R3),ARTLKWRK)           
*                                                                               
         CLI   DMCB,0                                                           
         BE    RTL6                                                             
         CLI   DMCB,208            TOO MANY RATES                               
         BE    RTL4B                                                            
*                                  POINT CURSOR TO ERROR ELEM                   
         LA    R2,RATLI1H                                                       
         LA    R0,RATLI2H-RATLI1H                                               
         LA    R3,PCONREC+33                                                    
*                                                                               
RTL2     DS    0H                                                               
         CLI   5(R2),0             TEST INPUT THIS LINE                         
         BE    RTL3                                                             
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ELCODE,0(R3)                                     L02             
         BNE   RTL2                                                             
*                                                                               
         CLI   PRBIND-PRBELEM(R3),X'FF'    TEST ERROR                           
         BE    RTL4                          YES                                
RTL3     DS    0H                                                               
         AR    R2,R0                      BUMP CURSOR                           
         B     RTL2                                                             
*                                                                               
RTL4     DS    0H                                                               
         LA    R2,RATRT1H-RATLI1H(R2)                                           
RTL4B    DS    0H                                                               
         IC    R3,DMCB                                                          
         B     ERROR                                                            
*                                                                               
RTL6     DS    0H                                                               
         MVC   ARTLKWRK(1),DMCB+12     SAVE ELEM COUNT                          
*                                                                               
*                                  DELETE RATE ELEMS IN RECORD                  
*                                                                               
         GOTO1 VDELELEM,DMCB,(ELCODE,PCONREC)                    L02            
         SPACE 3                                                                
*                             NOW ADD RATE ELEMS TO RECORD                      
*                             FROM RTLKWRK                                      
RTADD    DS    0H                                                               
         L     R5,ARTLKWRK                                                      
RTADD2   DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    RTADD4                                                           
*                                            SET -.01 BACK TO ZERO              
         CP    PRBRATE-PRBELEM(5,R5),=P'-1'                                     
         BNE   *+10                                                             
         ZAP   PRBRATE-PRBELEM(5,R5),=P'0'                                      
*                                                                               
         GOTO1 VADDELEM,DMCB,PCONREC,0(R5)                                      
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     RTADD2                                                           
*                                                                               
RTADD4   DS    0H                                                               
*                                                                               
*                                                                               
EDITSTD   B     DISP210         GO REDISPLAY NEW RATES                          
         TITLE 'PPCON40 - PRINTPAK CONTRACT STANDARD COMMENT EDIT'              
         SPACE 3                                                                
CHKOUT   NTR1                                                                   
         SPACE 2                                                                
         CLC   WORK(4),=C'SRI='                                                 
         BNE   COX                                                              
         XC    17(17,R4),17(R4)                                                 
         MVI   17(R4),X'FF'                                                     
         ZAP   18(3,R4),=P'99999'                                               
         LA    R5,WORK+7                                                        
         CLC   WORK+4(3),=C'SPC'                                                
         BE    CO4                                                              
         GOTO1 =V(NUMED),DMCB,WORK+4,DUB,RR=RELO40                              
*                                                                               
         CP    DUB,=P'999'          SHOWING CAN'T EXCEED 100                    
         BH    COERR                ALLOW UP TO 999                             
*                                   FOR GRPS                                    
         ZAP   18(3,R4),DUB                                                     
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BE    COERR                                                            
CO4      DS    0H                                                               
         GOTO1 =V(NUMED),DMCB,1(R5),DUB,RR=RELO40                               
*                                                                               
         ZAP   21(3,R4),DUB                                                     
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BE    COERR                                                            
         GOTO1 (RF),(R1),1(R5)                                                  
*                                                                               
         ZAP   24(3,R4),DUB                                                     
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BH    COERR                                                            
COX      DS    0H                                                               
         SR    R0,R0               SET CC OK                                    
COXX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
COERR    DS    0H                                                               
         LTR   RE,RE                                                            
         B     COXX                                                             
         EJECT                                                                  
EDT      DS    0H                                                               
         MVI   0(RF),C'0'                                                       
         LA    R0,1                                                             
         CP    0(3,R9),=P'0'                                                    
         BE    EDT2                                                             
         EDIT  (P3,0(R9)),(4,0(RF)),ALIGN=LEFT                                  
*                                                                               
EDT2     DS    0H                                                               
         AR    RF,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
RTDIV    DS    0H                                                               
         LTR   RF,RF                                                            
         BP    *+8                                                              
         SR    R1,R1                                                            
         BR    RE                                                               
*                                                                               
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
*                                                                               
OVFERR   DS    0H                                                               
         LA    R3,208                                                           
         B     ERROR                                                            
         SPACE 3                                                                
*                             DELETE ELEMS DISPLAYED                            
DELELEM  NTR1                                                                   
         SPACE 2                                                                
         XC    ELPUT,ELPUT                                                      
         CLC   KBAACT(4),=C'ADD '  DONT ACTUALLY DELETE ELEMS ON ADD            
         BNE   DL2                                                              
         MVI   RSTART,X'FF'                                                     
         B     DL3                                                              
DL2      DS    0H                                                               
         ZIC   R4,SAVPAG                                                        
         BCTR  R4,R0                                                            
         MH    R4,=Y(RPSCRN)                                                    
         LA    R4,1(R4)                                                         
         STC   R4,RSTART           FIRST                                        
         LA    R4,RPSCRN-1(R4)                                                  
         STC   R4,BYTE             LAST                                         
*                                                                               
DL3      DS    0H                                                               
         MVI   ELNUM,1                                                          
         MVI   BYTE3,0                                                          
         LA    R2,PCONREC+33                                                    
         B     DL6                                                              
DL4      DS    0H                                                               
         CLC   ELCODE,0(R2)                                       L02           
         BE    DL8                                                              
         CLI   0(R2),0                                                          
         BE    DL20                                                             
DL6      DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     DL4                                                              
*                                                                               
DL8      DS    0H                                                               
         CP    11(5,R2),=P'0'      SET ZERO                                     
         BNE   *+10                                                             
         ZAP   11(5,R2),=P'-1'     TO -1 TO PREVENT LOOK-UP                     
         MVC   BYTE2,ELNUM                                                      
         IC    RF,ELNUM            BUMP ELEM COUNT                              
         LA    RF,1(RF)                                                         
         STC   RF,ELNUM                                                         
         CLC   BYTE2,RSTART        TES VS START                                 
         BL    DL6                 LOW -SKIP                                    
         CLC   BYTE2,BYTE          TEST VS END                                  
         BH    DL20                HI - DONE                                    
         GOTO1 VRECUP,DMCB,(1,PCONREC),(R2)                                     
         MVI   BYTE3,1             SET HAVE FOUND ELEMS                         
         B     DL4                                                              
*                                                                               
DL20     DS    0H                                                               
         OC    ELPUT,ELPUT                                                      
         BNZ   *+8                                                              
         ST    R2,ELPUT           WHERE TO START ADDING CHANGED ELS             
         CLI   BYTE3,1             IF NO ELEMS DISPLAYED                        
         BE    DL22                                                             
         MVC   RSTART,ELNUM        ADD NEW ELEMS AFTER LAST                     
DL22     DS    0H                                                               
         CLI   0(R2),0                                                          
         BNE   DL6                                                              
         XIT1                                                                   
         SPACE 2                                                                
*                                  ADD AN ELEM                                  
ADELEM   NTR1                                                                   
         MVC   HALF,PCONREC+25                                                  
         LH    R1,HALF                                                          
         L     R3,ELADDR                                                        
         ZIC   R0,1(R3)                                                         
         AR    R1,R0                                                            
         CH    R1,=H'2976'                                                      
         BL    AL4                                                              
         MVI   DMCB,X'FF'                                                       
         B     AL20                                                             
AL4      DS    0H                                                               
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PCONREC),ELADDR,ELPUT                             
*                                                                               
         L     R3,ELPUT                                                         
         AR    R3,R0                                                            
         ST    R3,ELPUT           WHERE TO ADD NEXT ELEM                        
AL20     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
MYTEMP   DS    CL20             USED IN RATE EDIT                               
*                                                                               
LEVADDR  DS    F                                                                
RATADDR  DS    F                                                                
*                                                                               
         EJECT                                                                  
*        AND STORES THEM IN THE CONTRACT RATE ELEM                              
*                            R4 POINTS TO CONTRACT RATE ELEM                    
*                            R2 STILL POINTS TO FIELD                           
CKADVC   NTR1                                                                   
         CLI   AORCFND,2            SEE IF I ALREADY READ CONTRACT              
         BE    CKADVC29                                                         
*                                                                               
         XC    IOAREA(100),IOAREA     FIRST I MUST FIND AOR CONTRACT            
         MVC   IOAREA+32(64),KEY        SAVE KEY AND KEYSAVE                    
*                                                                               
         MVC   IOAREA(2),SADVDATA      AOR                                      
         MVC   IOAREA+2(1),SAVKMED                                              
         MVI   IOAREA+3,X'10'                                                   
         MVC   IOAREA+4(3),SADVDATA+2     ADV                                   
         MVC   IOAREA+7(6),SAVKPUB                                              
*                                                                               
         TM    SADVDATA+15,X'01'       PUB LINK REQUIRED                        
         BZ    CKADVC10                                                         
         MVI   KEY,X'FE'                                                        
         MVC   KEY+1(1),SAVKMED                                                 
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(3),SADVDATA+2        ADV                                   
         MVC   KEY+7(2),SADVDATA+0        AOR                                   
         MVC   KEY+9(6),SAVKPUB                                                 
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CKADVCX5                   ERROR                                 
         MVC   IOAREA+7(6),KEY+15                                               
*                                                                               
CKADVC10 DS    0H                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SADVDATA+14  SE NUMBER                                   
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    CKADVC20                                                         
         MVC   KBAMSG,=CL60'*** AGENCY FILE NOT ACTIVE ***'                     
         MVI   ERRAREA,X'FF'                                                    
         B     CKADVCXX                                                         
*                                                                               
CKADVC20 DS    0H                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 HIGH                                                             
         B     CKADVC27                                                         
CKADVC25 GOTO1 SEQ                                                              
CKADVC27 CLC   KEY(13),KEYSAVE     CHK AGY/MED/CLT/PUB                          
         BNE   CKADVCX5            AOR CONTRACT NOT FOUND                       
         MVC   AREC,APUBIO         READ INTO PUBIO                              
         GOTO1 GETREC                                                           
         L     RF,APUBIO                                                        
         CLC   PCONSDT,PCONSDT-PCONREC(RF)      CHK DATES                       
         BL    CKADVC25                                                         
         CLC   PCONEDT,PCONEDT-PCONREC(RF)                                      
         BH    CKADVC25                                                         
         MVI   AORCFND,3                 SET AOR CONTRACT JUST READ             
*                                                                               
CKADVC29 DS    0H                                                               
         L     RF,APUBIO                                                        
         LA    R5,33(RF)                                                        
CKADVC30 CLI   0(R5),X'20'       FIND FIRST RATE ELEM                           
         BE    CKADVC35                                                         
CKADVC33 ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BNE   CKADVC30                                                         
         B     CKADVCX5          NO RATE ELEMS                                  
*                                                                               
CKADVC35 DS    0H                                                               
         OC    2(3,R5),2(R5)     CHK FOR EFFECTIVE DATE IN AOR ELEM             
         BZ    CKADVC37                                                         
         OC    2(3,R4),2(R4)     CHK FOR EFFECTIVE DATE IN MY ELEM              
         BNZ   CKADVC36                                                         
         CLC   2(3,R5),PCONEDT   AOR EFFECTIVE AFTER MY END                     
         BH    CKADVC33          IGNORE THIS ELEM                               
         B     CKADVC37                                                         
*                                                                               
CKADVC36 CLC   2(3,R5),2(R4)     CHK AOR EFF DATE VS. MY EFF DATE               
         BH    CKADVC33          SKIP                                           
*                                                                               
CKADVC37 CLC   5(1,R4),5(R5)     SEE IF LEVEL IND MATCH                         
         BNE   CKADVC33          NO - THEN IGNORE THIS ELEM                     
         CP    6(5,R4),=P'-2'    SEE IF LOOKING-UP LEVEL                        
         BNE   CKADVC40                                                         
*                                                                               
         ZAP   6(5,R4),6(5,R5)   LEVEL FROM AOR CONTRACT                        
*                                                                               
CKADVC40 CP    11(5,R4),=P'-2'   SEE IF LOOKING-UP RATE                         
         BNE   CKADVC50                                                         
         CLC   17(17,R4),17(R5)   MATCH DESCRIPTIONS                            
         BNE   CKADVC33           SKIP THIS AOR ELEM                            
*                                                                               
         ZAP   11(5,R4),11(5,R5)  USE AOR RATE                                  
         MVC   16(1,R4),16(R5)    AND INDICATOR                                 
*                                                                               
CKADVC50 DS    0H                                                               
         CP    6(5,R4),=P'-2'                                                   
         BE    CKADVCX5                                                         
         CP    11(5,R4),=P'-2'                                                  
         BE    CKADVCX5                                                         
         B     CKADVCX7                                                         
*                                                                               
CKADVCX5 MVC   KBAMSG,=CL60'*** ADVERTISER DATA NOT FOUND ***'                  
         MVI   ERRAREA,X'FF'                                                    
         CLI   KEYSAVE,X'FE'          SEE IF LINK NOT FOUND                     
         BE    CKADVCXX               DON'T SWITCH BACK                         
*                                                                               
CKADVCX7 CLI   AORCFND,2              SEE IF I ALREADY HAD AOR CONTRACT         
         BE    CKADVCXX                                                         
         CLI   AORCFND,3              SEE IF JUST FOUND                         
         BNE   *+8                                                              
         MVI   AORCFND,2                                                        
*                                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                TROUBLE SWITCHING BACK                       
*                                                                               
CKADVCXX MVC   KEY(64),IOAREA+32   RESTORE KEY AND KEYSAVE                      
         XIT                                                                    
*                                                                               
       ++INCLUDE PPGENEROL         IN-LINE CODES                                
*                                                                               
         LTORG                                                                  
SPACES   DC    CL70' '                                                          
PATCH    DS    CL30                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PPCONWRK                                                       
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032PPCON40   11/05/03'                                      
         END                                                                    
