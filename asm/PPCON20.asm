*          DATA SET PPCON20    AT LEVEL 021 AS OF 11/05/03                      
*PHASE T40D20A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPCON20 - PRINTPAK CONTRACT DISPLAY'                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BPLA  09/02    FIX PROBLEM WHEN DISPLAYING PRBOPEN WHEN A                     
*                PRODUCT CODE IS PRESENT THERE.                                 
*                                                                               
* KWAN 09/20/01 REORGANIZED PPCONWRK                                            
*                                                                               
* KWAN 08/99     ADD CODES FOR NEW FIELDS (TEL AND FAX)                         
*                                                                               
* BPLA 10/98     CHANGE TO DISPLAY PRODUCT LEVEL RATES FOR                      
*                NEWSPAPERS                                                     
*                                                                               
* BPLA 1/97      NEW AOR CONTROL TO DENY AOR CONTRACT ACCESS                    
*                BY BRAND AGENCIES (SADVDATA+16 X'01')                          
*                                                                               
* BPLA 1/96      DISPLAY PRBLIND OF "N" AS "$" AND INSERT                       
*                "N" BEFORE LEVEL                                               
*                                                                               
* BPLA 1/96      CODE TO DISPLAY LATEST CONTRACT                                
*                                                                               
* BPLA 5/95      DISPLAY /I OR /L FOR ALL UNIT RATES                            
*                                                                               
* BPLA 3/20/95   COPY OF T40D20 (LEVEL 3 6/8/93)                                
*                CHANGES FOR INCH RATES                                         
*                                                                               
* BPLA 10/14/92  CHANGES FOR NEW AOR FEATURES                                   
*                BUYING AGENCY ACCESS TO AOR CONTRACT DISPLAY                   
*                                                                               
* BPLA 1/24/92 INCLUDE PPCONWRKA (HAS ADVERTISER DATA)                          
*                                                                               
* BPLA 6/1/89  ALLOW FOR NON-NEWSPAPER OPEN RATE AND PRODUCT        L04         
*                                                                               
*                                                                 BUG01         
* ROSA 8/31/88 ALLOW DISPLAY OF 1,000,000 FOR RATES               BUG01         
*                                                                 BUG01         
* ROSA 6/2/88  ALLOW ENTERING C RATES // COMMISSION ONLY            L03         
*                                                                   L02         
* ROSA 5/26/88 ALLOW ENTERING S RATES // NO AGENCY COMMISSION       L02         
*                                                                   L01         
* ROSA 4/28/88 ALLOW ENTERING NET RATES // N FOLLOWED BY RATE       L01         
*                                                                   L01         
         EJECT                                                                  
T40D20   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40D20,R8                                                      
         USING T40DFFD,RA                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    R7,1(RC)                                                         
         LA    R7,4095(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         RELOC RELO20                                                           
*                                                                               
         CLI   TWASTAT,X'FE'       K SCREEN IN TWA?                             
         BNE   DISP25                                                           
         GOTO1 VFOUTBLK,DMCB,CONSDTH,CONLAST                                    
         B     DISP50                                                           
DISP25   DS    0H                                                               
         GOTO1 VCALLOV,DMCB,KBALAST,X'D9040DFE'                                 
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   TWASTAT,X'FE'                                                    
*                                                                               
DISP50   CLC   KBAACT(3),=C'DIS'                                                
         BNE   DISP100                                                          
*              GET CONTRACT                                                     
         FOUT  KBAPAGH,SPACES,3                                                 
         NI    TWAKIND,X'3F'       NO K                                         
         LA    R2,KBANUMH                                                       
         LA    R3,53               NOT FOUND                                    
         XC    HALF,HALF                                                        
         XC    SVLEND,SVLEND                                                    
         XC    SVLCON,SVLCON                                                    
*                                                                               
         CLC   8(4,R2),=C'LAST'                                                 
         BE    DISP50L                                                          
         CLC   8(6,R2),=C'LATEST'                                               
         BE    DISP50L                                                          
*                                                                               
         DS    0H                                                               
         LA    R3,2                INVALID NUMBER                               
         CLI   5(R2),3             MAX LENGTH FOR NUMBER                        
         BH    ERROR                                                            
         LA    R3,1                MISSING INPOUT                               
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
*              BUILD K KEY                                                      
         LA    R3,53               REC NOT FOUND                                
         STH   R0,HALF                                                          
DISP50L  MVC   PCONKEY(13),SAVKKEY                                              
         MVC   PCONNUM,HALF                                                     
         MVC   KEY,PCONKEY                                                      
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    DSP53                                                            
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    DSP53                                                            
         TM    SADVDATA+15,X'20'          OR I HAVE MY OWN CONTRACTS            
         BZ    DSP53                                                            
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
         BE    DSP53                                                            
         MVC   KBAMSG,=CL60'** AOR SYSTEM NOT ACTIVE **'                        
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
DSP53    BAS   RE,HIGH                                                          
         B     DSP53A5                                                          
DSP53A   BAS   RE,SEQ                                                           
DSP53A5  CLC   KEY(25),KEYSAVE                                                  
         BE    DSP53G                     CONTRACT FOUND                        
         CLC   HALF,=X'0000'              SEE IF LOOKING FOR LATEST             
         BNE   DSP53C                     IF NOT,SEND NOT FOUND                 
         CLC   KEY(13),KEYSAVE            JUST CHECK THROUGH PUB                
         BNE   DSP53A8                                                          
*                                                                               
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
         BAS   RE,GETREC                                                        
         CLC   SVLEND,PCONEND                                                   
         BH    DSP53A                                                           
         MVC   SVLEND,PCONEND        SAVE HIGHEST END DATE                      
         MVC   SVLCON,PCONNUM       AND NUMBER                                  
         B     DSP53A                                                           
*                                                                               
DSP53A8  DS    0H                                                               
         OC    SVLEND,SVLEND             NO CONTRACTS FOUND                     
         BZ    DSP53C                                                           
         MVC   HALF,SVLCON      GO BACK AND READ LATEST CONTRACT                
         XC    KEY,KEY                                                          
         MVC   KEY(25),PCONREC                                                  
         MVC   KEY+13(2),HALF                                                   
         XC    KBANUM,KBANUM                                                    
         EDIT  (2,SVLCON),(3,KBANUM),ALIGN=LEFT                                 
         FOUT  KBANUMH                                                          
         B     DSP53                                                            
                                                                                
*                                                                               
DSP53C   DS    0H                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    DSP53E                                                           
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    DSP53E                                                           
         TM    SADVDATA+15,X'20'        OR IF I HAVE MY OWN CONTRACTS           
         BZ    DSP53E                                                           
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
DSP53E   B     ERROR                                                            
*                                                                               
*                                                                               
DSP53G   DS    0H                                                               
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    DSP53L                                                           
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    DSP53L                                                           
         TM    SADVDATA+15,X'20'       OR IF I HAVE MY OWN CONTRACTS            
         BZ    DSP53L                                                           
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
         BZ    DSP53L                                                           
*                                                                               
         MVC   KBAMSG,=CL60'** ACCESS DENIED TO AOR CONTRACT **'                
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
DSP53L   DS    0H                                                               
         LA    RF,IOAREA           RESTORE AREC                                 
         ST    RF,AREC                                                          
         CLI   SAVPRD,0            SEE IF LOOKING FOR A PRD CONTRACT            
         BE    DISP55              NO                                           
         CLC   PCONPRD,SAVPRD      YES - MUST MATCH                             
         BNE   ERROR               NOT FOUND                                    
         B     DISP60                                                           
*                                                                               
DISP55   CLI   PCONPRD,C'A'        IS THIS A PRD CONTRACT                       
         BNL   ERROR               SEND NOT FOUND                               
*                                                                               
DISP60   MVC   SAVKKEY,KEY                                                      
         EJECT                                                                  
*                                                                               
* DISPLAY CONTRACT FIELDS                                                       
*                                                                               
DISP100  DS    0H                                                               
         MVI   CONSPR,C' '         CLEAR AUTO SPC RES FIELDS                    
         FOUT  CONSPRH                                                          
         XC    CONLDT,CONLDT                                                    
         FOUT  CONLDTH                                                          
*                                                                               
         LA    R4,PCONREC+33                                                    
DISP101  CLI   0(R4),X'00'                                                      
         BE    DISP110                                                          
         CLI   0(R4),X'85'         DOES AUTO SPACE EL EXIST                     
         BE    DISP103                                                          
DISP102  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DISP101                                                          
*                                                                               
         USING PASRELEM,R4                                                      
DISP103  CLC   PASRCLT,SPACES                                                   
         BH    DISP102                                                          
         CLI   CONSPR,C'*'                                                      
         BE    DISP104                                                          
         MVI   CONSPR,C'Y'         IF X'85' EL EXISTS DISPLAY Y                 
DISP104  FOUT  CONSPRH                                                          
         GOTO1 VDATCON,DMCB,(5,0),(3,SVTODAY)                                   
         CLC   PASRCDAT,SVTODAY                                                 
         BE    DISP105                                                          
         OC    PASRCDAT,PASRCDAT                                                
         BZ    DISP110                                                          
         GOTO1 VDATCON,DMCB,(3,PASRCDAT),(5,CONLDT+12)                          
         B     DISP106                                                          
DISP105  OC    PASRLDAT,PASRLDAT         IF A LAST RUN ON DATE EXISTS           
         BZ    DISP110                   PUT TO SCREEN                          
         GOTO1 VDATCON,DMCB,(3,PASRLDAT),(5,CONLDT+12)                          
DISP106  MVC   CONLDT(11),=C'LAST RUN ON'                                       
         FOUT  CONLDTH                                                          
*                                                                               
*                                                                               
*                                                                               
DISP110  DS    0H                                                               
         USING PCTFELEM,R4         CONTRACT TEL & FAX ELEM                      
*                                                                               
         XC    CONTEL,CONTEL       CLEAR TEL FIELD                              
         FOUT  CONTELH                                                          
         XC    CONFAX,CONFAX       CLEAR FAX FIELD                              
         FOUT  CONFAXH                                                          
*                                                                               
         LA    R4,PCONREC+33                                                    
DISP10D  CLI   0(R4),X'55'                                                      
         BE    DISP110M            ELEM FOUND, GO DISPLAY TEL & FAX             
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'00'                                                      
         BE    DISP110Z            DONE WITH TEL & FAX, CHK NEXT ELEM           
         B     DISP10D                                                          
*                                                                               
DISP110M DS    0H                                                               
         MVC   CONTEL,PCONTELE                                                  
         FOUT  CONTELH                                                          
         MVC   CONFAX,PCONFAX                                                   
         FOUT  CONFAXH                                                          
*                                                                               
*                                                                               
*                                                                               
DISP110Z DS    0H                                                               
         USING PCATELEM,R4                                                      
         LA    R4,PCONREC+33                                                    
DISP111  CLI   0(R4),X'50'                                                      
         BE    DISP123                                                          
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'00'                                                      
         BE    DISP124                                                          
         B     DISP111                                                          
*                                                                               
DISP123  DS    0H                                                               
         LA    R5,CONATTNH                                                      
         MVC   CONATTN,PCATNAM                                                  
         FOUT  (R5)                                                             
         LA    R5,CONCDPH                                                       
         OC    PCATPCT,PCATPCT                                                  
         BZ    DISP123F                                                         
         CLC   PCATPCT,=2X'FF'                                                  
         BNE   DISP123A                                                         
         MVC   CONCDP(3),=C'0.0'                                                
         B     DISP123B                                                         
DISP123A EDIT  PCATPCT,(4,CONCDP),1                                             
DISP123B FOUT  (R5)                                                             
*                                                                               
DISP123F LA    R5,CONMAXH                                                       
         OC    PCATMAX,PCATMAX                                                  
         BZ    DISP123H                                                         
         EDIT  (B2,PCATMAX),(4,CONMAX),0,ALIGN=LEFT                             
         FOUT  (R5)                                                             
*                                                                               
DISP123H DS    0H                                                               
         LA    R5,CONMIZEH                                                      
         OC    PCATMAXZ,PCATMAXZ                                                
         BZ    DISP124                                                          
         EDIT  (B2,PCATMAXZ),(4,CONMIZE),0,ALIGN=LEFT                           
         FOUT  (R5)                                                             
*                                                                               
*******  GOTO1 VDTCNV,DMCB,(1,PCONSDT),(3,CONSDT) START DATE                    
DISP124  GOTO1 VDATCON,DMCB,(3,PCONSDT),(5,CONSDT) START DATE                   
*                                                                               
******** GOTO1 (RF),(R1),(1,PCONEDT),(3,CONEDT)   END DATE                      
         GOTO1 (RF),(R1),(3,PCONEDT),(5,CONEDT)   END DATE                      
*                                                                               
         OC    PCONREV,PCONREV                                                  
         BZ    DISP125                                                          
******** GOTO1 (RF),(R1),(1,PCONREV),(3,CONREV)   REVISION DATE                 
         GOTO1 (RF),(R1),(3,PCONREV),(5,CONREV)   REVISION DATE                 
*              CONTRIBUTING VOLUME                                              
DISP125  DS    0H                                                               
         MVC   CONREQ,PCONREQ                                                   
         EDIT  (P5,PCONCON),(8,CONCON),ALIGN=LEFT,COMMAS=YES                    
*              TYPE                                                             
         MVC   CONTYP,PCONTYP                                                   
         EJECT                                                                  
*              RATE BASIS LINES    GET X'20' RATE BASIS ELEMENTS                
         GOTO1 VGETEL,DMCB,(X'20',PCONREC),DMCB+8                               
*                                                                               
         CLI   DMCB,X'FF'                                                       
         BE    DISP350             NO RATES ELEMS FOUND                         
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
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    DISP140                                                          
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    DISP140                                                          
*                                                                               
*        MUST SWITCH TO AOR                                                     
*        TO READ PUB                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SADVDATA+14         AOR SE NUMBER                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    DISP140                                                          
         MVC   KBAMSG,=CL60'** AOR SYSTEM NOT ACTIVE **'                        
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
DISP140  DS    0H                                                               
         BAS   RE,GETPUB                                                        
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    DISP142                                                          
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    DISP142                                                          
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
DISP142  DS    0H                                                               
*                                                                               
*                                                                               
DISP150  DS    0H                                                               
         GOTO1 VRTLOOK,DMCB,0,APUBIO,PCONREC,WORK2                              
         LA    R2,CONRT1H                                                       
         SR    R3,R3                                                            
         CLI   DMCB,0              ERROR?                                       
         BE    *+12                                                             
         IC    R3,DMCB                                                          
         B     ERROR                                                            
*                                                                               
         LA    R3,WORK2                                                         
         LA    R5,WORK2+199                                                     
*                                                                               
*              DISPLAY RATE BASIS LINE                                          
DISP200  LA    R2,CONLI1H          1ST RATE BASIS LINE                          
         SR    R1,R1                                                            
DISP225  DS    0H                                                               
         LA    R9,CONED4H                                                       
         CR    R2,R9               LAST RATE BASIS FIELD?                       
         BNH   *+12                                                             
         MVI   RTOVFL,C'Y'                                                      
         B     DISP350                                                          
*                                                                               
         MVC   8(1,R2),5(R3)       LVL IND                                      
*                                                                               
         CLI   5(R3),C'N'          SEE IF NET $ VOLUME                          
         BNE   *+8                                                              
         MVI   8(R2),C'$'          DISPLAY AS "$"                               
*                                  AND DISPLAY A "N" BEFORE THE LEVEL           
*                                                                               
         ZIC   R1,0(R2)            FIELD LEN                                    
         LA    R2,0(R1,R2)         NEXT FIELD                                   
*              LEVEL                                                            
         CLI   5(R3),C'N'              "N" IN PRBLIND                           
         BNE   DISP225X                MEANS LEVEL IS NET $                     
         MVI   8(R2),C'N'              NO COMMAS                                
         EDIT  (P5,6(R3)),(7,9(R2)),ALIGN=LEFT                                  
         B     DISP227                                                          
*                                                                               
DISP225X CP    6(5,R3),=P'999999'      SEE IF OVER 1 MILLION                    
         BNH   DISP226                 DISPLAY WITHOUT COMMAS                   
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
         ZIC   R1,0(R2)                                                         
         LA    R2,0(R1,R2)                                                      
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
DISP230  ZIC   R1,0(R2)                                                         
         LA    R2,0(R1,R2)                                                      
*                                                                               
         CLC   17(2,R3),=C'R='                                                  
         BNE   DISP230A                                                         
         LA    R9,8(R2)                                                         
         B     DISP233                                                          
*                                                                               
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
         TM    16(R3),X'10'     IS THIS A NET RATE                  L01         
         BNO   *+12                                                 L01         
         MVI   0(R9),C'N'       NET INDICATIOR                      L01         
         LA    R9,1(R9)                                             L01         
         TM    16(R3),X'02'     IS THIS A S   RATE                  L02         
         BNO   *+12                                                 L02         
         MVI   0(R9),C'S'       NET INDICATIOR                      L02         
         LA    R9,1(R9)                                             L01         
*                                                                               
         TM    16(R3),X'04'     IS THIS A 'C' RATE                  L03         
         BNO   *+12                                                 L03         
         MVI   0(R9),C'C'                                           L03         
         LA    R9,1(R9)                                             L03         
*                                                                 BUG01         
         EDIT  (P5,11(R3)),(13,(R9)),5,ALIGN=LEFT,FLOAT=-,COMMAS=YES 01         
*                                                                   L01         
         CLI   5(R3),C'I'       INCH LEVEL MUST BE INCH RATE                    
         BE    DISP234                                                          
         TM    16(R3),X'08'     SEE IF THIS IS AN INCH RATE                     
         BZ    DISP235          FOR NON-INCH LEVEL                              
DISP234  AR    R9,R0         ADD LENGTH OF EDIT OUTPUT                          
         MVC   0(2,R9),=C'/I'                                                   
         B     DISP275                                                          
*                            MUST BE A LINE RATE                                
DISP235  DS    0H                                                               
         AR    R9,R0         ADD LENGTH OF EDIT OUTPUT                          
         MVC   0(2,R9),=C'/L'                                                   
         B     DISP275                                                          
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
         TM    16(R3),X'10'     IS THIS A NET RATE                  L01         
         BNO   *+12                                                 L01         
         MVI   0(R9),C'N'       NET INDICATIOR                      L01         
         LA    R9,1(R9)                                             L01         
         TM    16(R3),X'02'     IS THIS A S   RATE                  L02         
         BNO   *+12                                                 L02         
         MVI   0(R9),C'S'       NET INDICATIOR                      L02         
         LA    R9,1(R9)                                             L02         
*                                                                               
         TM    16(R3),X'04'    IS THIS A 'C' RATE                   L03         
         BNO   *+12                                                 L03         
         MVI   0(R9),C'C'                                           L03         
         LA    R9,1(R9)                                             L03         
*                                                                               
*        CP    11(5,R3),=PL5'99999999'      ONE MILLION           BUG01         
*        BNH   OKNTMILL                                           BUG01         
*        EDIT  (P5,11(R3)),(9,(R9)),2,ALIGN=LEFT,FLOAT=-          BUG01         
*        B     DISP275                                            BUG01         
         EDIT  (P5,11(R3)),(13,(R9)),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES            
**NOGOOD EDIT  (P5,11(R3)),(9,(R9)),2,ALIGN=LEFT,FLOAT=-,COMMAS=YES L01         
*                                                                   L01         
*                                                                               
DISP275  ZIC   R1,0(R2)                                                         
         LA    R2,0(R1,R2)         NEXT FIELD                                   
*                                                                               
         CLC   17(2,R3),=C'R='                                                  
         BNE   DISP275F                                                         
         MVC   8(5,R2),17(R3)                                                   
         LA    R1,13(R2)                                                        
         CLI   21(R3),C' '                                                      
         BNE   DISP275A                                                         
         BCTR  R1,0                                                             
         CLI   20(R3),C' '                                                      
         BNE   DISP275A                                                         
         BCTR  R1,0                                                             
DISP275A MVI   0(R1),C','                                                       
         MVC   1(12,R1),22(R3)                                                  
         B     DISP285                                                          
*                                                                               
DISP275F MVC   8(17,R2),17(R3)     DESCRIPTION                                  
         CLI   17(R3),X'FF'                                                     
         BNE   DISP280                                                          
*                                  SPECIAL OUTDOOR FIELDS                       
         LA    RF,16(R2)                                                        
         MVC   8(8,R2),=C'SRI=SPC,'                                             
         CP    18(3,R3),=P'99999'                                               
         BE    DISP276                                                          
         LA    RF,12(R2)                                                        
         LA    R6,18(R3)                                                        
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
DISP276  DS    0H                                                               
         LA    R6,21(R3)                                                        
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    R6,24(R3)                                                        
         LA    RF,1(RF)                                                         
         BAS   RE,EDT                                                           
*                                                                               
DISP280  DS    0H                                                               
         LA    R9,WORK2+199                                                     
         CR    R5,R9               RATELOOK?                                    
         BNE   DISP285                                                          
         CP    6(5,R3),=P'0'       OPEN LEVEL?                                  
         BE    DISP285                                                          
         CLI   34(R3),C'A'        SEE IF PRODUCT IN PRBOPEN 34(R3)              
         BNL   DISP285            SKIP OPEN DISPLAY                             
         MVC   8(5,R2),=C'OPEN='                                                
         EDIT  (P5,34(R3)),(8,13(R2)),5,ALIGN=LEFT                              
DISP285  DS    0H                                                               
*              EFFECTIVE DATE                                                   
         ZIC   R1,0(R2)                                                         
         LA    R2,0(R1,R2)         NEXT FIELD                                   
         LR    R9,R2                                                            
         OC    2(3,R3),2(R3)       EFFECTIVE DATE?                              
         BZ    DISP290                                                          
*                                                                               
*******  GOTO1 VDTCNV,DMCB,(1,2(R3)),(3,8(R2))                                  
         GOTO1 VDATCON,DMCB,(3,2(R3)),(5,8(R2))                                 
         LA    R9,8(R9)        BUMP PAST DATE                                   
*                                                                               
DISP290  DS    0H                                                               
*******  CLI   KBAMED,C'N'   ALL MEDIA MAY NOW HAVE A PRODUCT                   
*******  BE    DISP300       IN PRBOPEN                                         
         CLI   34(R3),C'A'             CHECK FOR PRODUCT          L04           
         BL    DISP300                                            L04           
         MVI   8(R9),C'-'                                                       
         MVC   9(3,R9),34(R3)           PRODUCT IN PRBOPEN                      
*                                                                               
DISP300  DS    0H                                                               
         IC    R4,1(R3)            RATE BASIS ELEM LEN                          
         BXLE  R3,R4,*+8           NEXT ELEM                                    
         B     DISP350                                                          
*                                                                               
         CLI   0(R3),X'20'         RATE BASIS ELEM?                             
         BNE   DISP350                                                          
         ZIC   R1,0(R2)                                                         
         LA    R2,0(R1,R2)         NEXT FIELD                                   
         B     DISP225                                                          
         EJECT                                                                  
*              STANDARD COMMENTS - GET FIRST ELEM                               
DISP350  GOTO1 VGETEL,DMCB,(X'30',PCONREC),DMCB+8                               
         CLI   DMCB,X'FF'          NONE?                                        
         BE    DISP400                                                          
         FOUT  CONSTDH                                                          
         SR    R9,R9                                                            
         IC    R9,CONSTDH                                                       
         LA    R9,CONSTDH-7(R9)    FIELD END-7                                  
         LM    R3,R5,DMCB+8        BXLE                                         
         LA    R6,CONSTD-1                                                      
         B     *+8                                                              
*                                                                               
DISP375  MVI   0(R6),C','                                                       
         MVC   1(6,R6),2(R3)       STANDARD COMMENT                             
         LA    R6,7(R6)                                                         
         CR    R6,R9               END OF FIELD?                                
         BH    DISP400                                                          
         IC    R4,1(R3)            ELEM LEN                                     
         BXLE  R3,R4,*+8                                                        
         B     DISP400             LAST                                         
         LA    RE,CONSTD+61        END-7                                        
         CR    R6,RE                                                            
         BH    DISP400                                                          
*                                                                               
         CLI   0(R3),X'30'         STD COMMENT ELEM?                            
         BE    DISP375                                                          
*        EJECT                                                                  
*****                                                                           
*              DISPLAY STANDARD COMMENTS FROM CLIENT HEADER                     
*ISP380  DS    0H                                                               
*        GOTO1 VGETEL,DMCB,(X'10',PCLTREC),DMCB+8                               
*        CLI   DMCB,X'FF'                                                       
*        BE    DISP400                                                          
*        L     R3,DMCB+8                                                        
*        MVC   CONCM1(6),2(R3)                                                  
*        FOUT  CONCM1H                                                          
*****                                                                           
         EJECT                                                                  
*              DISPLAY SPECIAL COMMENTS                                         
DISP400  GOTO1 VGETEL,DMCB,(X'40',PCONREC),DMCB+8                               
         CLI   DMCB,X'FF'          NONE?                                        
         BE    DISP500                                                          
*                                                                               
         LA    R6,CONCM1H          1ST COMMENT                                  
         LM    R3,R5,DMCB+8        BXLE                                         
******                                                                          
         CLC   CONCM1(6),SPACES                                                 
         BNH   DISP425                                                          
         LA    R6,CONCM2H                                                       
******                                                                          
*                                                                               
DISP425  IC    R4,1(R3)            ELEM LEN                                     
         SR    R9,R9                                                            
         IC    R9,0(R6)                                                         
* TEST IF COMMENT GREATER THAN FIELD LENGTH                                     
         SH    R9,=H'6'                                                         
         CR    R4,R9                                                            
         BNH   *+6                                                              
         LR    R4,R9                                                            
         CH    R4,=H'3'                                                         
         BL    DISP430                                                          
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R6),2(R3)       MOVE SPECIAL COMMENT                         
DISP430  FOUT  (R6)                                                             
         IC    R4,0(R6)            FIELD LEN                                    
         LA    R6,0(R4,R6)         NEXT FIELD                                   
         CLI   0(R6),0             LAST?                                        
         BE    DISP500                                                          
*                                                                               
         IC    R4,1(R3)            ELEM LEN                                     
         BXLE  R3,R4,*+8           NEXT ELEM                                    
         B     DISP500             LAST                                         
*                                                                               
         CLI   0(R3),X'40'         COMMENT?                                     
         BE    DISP425                                                          
         EJECT                                                                  
*              DISPLAY COMPLETE - TURN ON VALID BITS                            
DISP500  LA    R2,KBAACTH                                                       
DISP501  OI    4(R2),X'20'         VALID BIT                                    
         ZIC   R1,0(R2)                                                         
         LA    R2,0(R1,R2)         NEXT FIELD                                   
         CLI   0(R2),0             LAST?                                        
         BNE   DISP501                                                          
         MVC   KBAMSG,SPACES                                                    
         LA    R2,KBAACTH          CURSOR                                       
         OI    TWAKIND,X'80'       VALID K                                      
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
         CLC   KBAACT(3),=C'ADD'                                                
         BNE   DISP600                                                          
*              ADD RECORD                                                       
         LH    R3,NEXTNUM          LAST K NUMBER                                
         LA    R3,1(R3)                                                         
         STH   R3,HALF                                                          
         MVC   PCONNUM,HALF                                                     
         MVC   IOAREA+13(2),HALF   NEW K NUMBER                                 
*                                                                               
         BAS   RE,ADDREC           ADD CONTRACT REC                             
         MVC   SAVKKEY+27(4),KEY   SAVE K DISK ADDR                             
         EDIT  (2,PCONNUM),(8,KBANUM),ALIGN=LEFT                                
         FOUT  KBANUMH                                                          
         MVC   KBAMSG(27),=C'CONTRACT ADDED. NOTE NUMBER'                       
*                                                                               
DISP550  DS    0H                                                               
         LA    RF,IOAREA           RESTORE AREC                                 
         ST    RF,AREC                                                          
         CLI   RTOVFL,C'Y'                                                      
         BNE   DISP552                                                          
*                                  FLOAT ADD'L RATES MESSAGE                    
         LA    R6,KBAMSG+L'KBAMSG-1                                             
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVC   2(28,R6),=C'(USE ''DISR'' FOR ADD''L RATES)'                     
*                                                                               
DISP552  DS    0H                                                               
         TM    PCONLIND,X'80'      TEST LOCKED                                  
         BZ    DISP555                                                          
         FOUT  KBAACTH,=C'LOCKED',6                                             
         B     EXIT                                                             
*                                                                               
DISP555  FOUT  KBAACTH,SPACES,8                                                 
         B     EXIT                                                             
DISP600  CLC   KBAACT(3),=C'DIS'                                                
         BE    DISP700                                                          
         MVC   DMWORK(96),DMWORK2  CHANGE                                       
         MVC   KEY+27(4),SAVKKEY+27     DISK ADDR                               
*                                                                               
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    DISP610                                                          
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    DISP610                                                          
         TM    SADVDATA+15,X'20'        OR IF I HAVE MY OWN CONTRACTS           
         BZ    DISP610                                                          
*                                                                               
*     MUST SWITCH TO AOR                                                        
*     TO WRITE CONTRACT (SINCE LOCK/UNLOCK IS ALLOWED FOR THE ADV)              
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SADVDATA+14         AOR SE NUMBER                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    DISP610                                                          
         MVC   KBAMSG,=CL60'** AOR SYSTEM NOT ACTIVE **'                        
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
DISP610  DS    0H                                                               
         BAS   RE,PUTREC           PRTFILE                                      
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    DISP620                                                          
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    DISP620                                                          
         TM    SADVDATA+15,X'20'        OR IF I HAVE MY OWN CONTRACTS           
         BZ    DISP620                                                          
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
DISP620  DS    0H                                                               
         MVC   KBAMSG(16),=C'CONTRACT CHANGED'                                  
         CLC   KBAACT(3),=C'CHA'                                                
         BE    DISP550                                                          
         MVC   KBAMSG+9(7),=C'LOCKED  '                                         
         CLC   KBAACT(4),=C'LOCK'                                               
         BE    DISP550                                                          
         MVC   KBAMSG+9(8),=C'UNLOCKED'                                         
         B     DISP550                                                          
*                                                                               
*              DISPLAY                                                          
DISP700  MVC   KBAMSG(18),=C'CONTRACT DISPLAYED'                                
         B     DISP550                                                          
         SPACE 2                                                                
EDT      DS    0H                                                               
         MVI   0(RF),C'0'                                                       
         LA    R0,1                                                             
         CP    0(3,R6),=P'0'                                                    
         BE    EDT2                                                             
         EDIT  (P3,0(R6)),(4,0(RF)),ALIGN=LEFT                                  
*                                                                               
EDT2     DS    0H                                                               
         AR    RF,R0                                                            
         BR    RE                                                               
SVTODAY  DS    CL3                                                              
*                                                                               
SVLEND   DS    XL3                 USED FOR SEARCHING FOR LATEST                
SVLCON   DS    XL2                 CONTRACT NUMBER OF LATEST CONTRACT           
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
**PAN#1  DC    CL21'021PPCON20   11/05/03'                                      
         END                                                                    
