*          DATA SET PPCON60    AT LEVEL 016 AS OF 11/05/03                      
*PHASE T40D60A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPCON60 - PRINTPAK CONTRACT COPY - DISPLAY'                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 09/20/01 REORGANIZED PPCONWRK AND SAVCLTOF (CLT OFFICE CODE)             
*                                                                               
* KWAN 08/99    ADD CODES FOR NEW FIELDS (TEL AND FAX)                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
T40D60   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40D60,R8                                                      
         USING T40DFFD,RA                                                       
*                                                                               
         L     RC,0(R1)                                                         
         LA    R7,1(RC)                                                         
         LA    R7,4095(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         RELOC RELO20                                                           
*                                                                               
         CLI   TWASTAT,X'FA'       COPY SCREEN IN TWA?                          
         BNE   DISP25                                                           
         GOTO1 VFOUTBLK,DMCB,CPYSDTH,CPYLAST                                    
         B     DISP50                                                           
DISP25   DS    0H                                                               
         GOTO1 VCALLOV,DMCB,KBALAST,X'D9040DFA'                                 
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   TWASTAT,X'FA'                                                    
*                                                                               
DISP50   CLC   KBAACT(4),=C'COPY'  SPECIAL CODE FOR TESTING COPY                
         BE    *+6                                                              
         DC    H'0'                SOMETHING SCREWY                             
*              GET CONTRACT                                                     
         FOUT  KBAPAGH,SPACES,3                                                 
         NI    TWAKIND,X'3F'       NO K                                         
         LA    R2,KBANUMH                                                       
         LA    R3,53               RECORD NOT FOUND                             
         XC    SVLEND,SVLEND                                                    
         XC    SVLCON,SVLCON                                                    
         XC    HALF,HALF                                                        
         CLC   8(4,R2),=C'LAST'    SEE IF LOOKING FOR LATEST CONTRACT           
         BE    DISP50L                                                          
         CLC   8(6,R2),=C'LATEST'                                               
         BE    DISP50L                                                          
*                                                                               
         LA    R3,2                INVALID NUMBER                               
         CLI   5(R2),3             MAX LENGHT FOR NUMBER                        
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
         LA    R2,KBAACTH                                                       
         LA    R3,12                                                            
         B     ERROR            NO COPYING AOR CONTRACTS                        
*                                                                               
DSP53    BAS   RE,HIGH                                                          
         B     DSP53A5                                                          
*                                                                               
DSP53A   BAS   RE,SEQ                                                           
DSP53A5  CLC   KEY(25),KEYSAVE                                                  
         BE    DSP53G                                                           
         CLC   HALF,=X'0000'              SEE IF LOOKING FOR LAST               
         BNE   DSP53C                     IF NOT,SEND NOT FOUND                 
         CLC   KEY(13),KEYSAVE            JUST CHECK THROUGH PUB                
         BNE   DSP53A8                                                          
*                                                                               
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
         BAS   RE,GETREC                                                        
         OC    SAVPRD,SAVPRD     SEE IF LOOKING FOR PRODUCT CONTRACTS           
         BZ    DSP53A6                                                          
         CLC   PCONPRD,SAVPRD                                                   
         BNE   DSP53A                WRONG PRODUCT SO IGNORE                    
*                                                                               
DSP53A6  CLC   SVLEND,PCONEND                                                   
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
*                                                                               
DSP53E   B     ERROR                                                            
*                                                                               
DSP53G   DS    0H                                                               
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
         BAS   RE,GETREC                                                        
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
*              DISPLAY CONTRACT FIELDS                                          
DISP100  DS    0H                                                               
         MVI   CPYSPR,C' '          CLEAR AUTO SPC RES FIELDS                   
         FOUT  CPYSPRH                                                          
         XC    CPYLDT,CPYLDT                                                    
         FOUT  CPYLDTH                                                          
*                                                                               
         LA    R4,PCONREC+33                                                    
DISP101  CLI   0(R4),X'00'                                                      
         BE    DISP110                                                          
         CLI   0(R4),X'85'               DOES AUTO SPACE EL EXIST               
         BE    DISP103                                                          
DISP102  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DISP101                                                          
*                                                                               
         USING PASRELEM,R4                                                      
DISP103  CLC   PASRCLT,SPACES                                                   
         BH    DISP102                                                          
         CLI   CPYSPR,C'*'                                                      
         BE    DISP104                                                          
         MVI   CPYSPR,C'Y'               IF X'85' EL EXISTS DISPLAY Y           
DISP104  FOUT  CPYSPRH                                                          
         GOTO1 VDATCON,DMCB,(5,0),(3,SVTODAY)                                   
         CLC   PASRCDAT,SVTODAY                                                 
         BE    DISP105                                                          
         OC    PASRCDAT,PASRCDAT                                                
         BZ    DISP110                                                          
         GOTO1 VDATCON,DMCB,(3,PASRCDAT),(5,CPYLDT+12)                          
         B     DISP106                                                          
DISP105  OC    PASRLDAT,PASRLDAT         IF A LAST RUN ON DATE EXISTS           
         BZ    DISP110                   PUT TO SCREEN                          
         GOTO1 VDATCON,DMCB,(3,PASRLDAT),(5,CPYLDT+12)                          
DISP106  MVC   CPYLDT(11),=C'LAST RUN ON'                                       
         FOUT  CPYLDTH                                                          
*                                                                               
*                                                                               
*                                                                               
DISP110  DS    0H                                                               
         USING PCTFELEM,R4         CONTRACT TEL & FAX ELEM                      
*                                                                               
         XC    CPYTEL,CPYTEL       CLEAR TEL FIELD                              
         FOUT  CPYTELH                                                          
         XC    CPYFAX,CPYFAX       CLEAR FAX FIELD                              
         FOUT  CPYFAXH                                                          
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
         MVC   CPYTEL,PCONTELE                                                  
         FOUT  CPYTELH                                                          
         MVC   CPYFAX,PCONFAX                                                   
         FOUT  CPYFAXH                                                          
*                                                                               
*                                                                               
*                                                                               
DISP110Z DS    0H                                                               
*                                                                               
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
         LA    R5,CPYATTNH                                                      
         MVC   CPYATTN,PCATNAM                                                  
         FOUT  (R5)                                                             
         LA    R5,CPYCDPH                                                       
         OC    PCATPCT,PCATPCT                                                  
         BZ    DISP123F                                                         
         CLC   PCATPCT,=2X'FF'                                                  
         BNE   DISP123A                                                         
         MVC   CPYCDP(3),=C'0.0'                                                
         B     DISP123B                                                         
DISP123A EDIT  PCATPCT,(4,CPYCDP),1                                             
DISP123B FOUT  (R5)                                                             
*                                                                               
DISP123F LA    R5,CPYMAXH                                                       
         OC    PCATMAX,PCATMAX                                                  
         BZ    DISP123H                                                         
         EDIT  (B2,PCATMAX),(4,CPYMAX),0,ALIGN=LEFT                             
         FOUT  (R5)                                                             
*                                                                               
DISP123H DS    0H                                                               
         LA    R5,CPYMIZEH                                                      
         OC    PCATMAXZ,PCATMAXZ                                                
         BZ    DISP124                                                          
         EDIT  (B2,PCATMAXZ),(4,CPYMIZE),0,ALIGN=LEFT                           
         FOUT  (R5)                                                             
*                                                                               
DISP124  GOTO1 VDATCON,DMCB,(3,PCONSDT),(5,CPYSDT) START DATE                   
*                                                                               
         GOTO1 (RF),(R1),(3,PCONEDT),(5,CPYEDT)   END DATE                      
*                                                                               
         OC    PCONREV,PCONREV                                                  
         BZ    DISP125                                                          
         GOTO1 (RF),(R1),(3,PCONREV),(5,CPYREV)   REVISION DATE                 
*              CONTRIBUTING VOLUME                                              
DISP125  DS    0H                                                               
         MVC   CPYREQ,PCONREQ                                                   
         EDIT  (P5,PCONCON),(8,CPYCON),ALIGN=LEFT,COMMAS=YES                    
*              TYPE                                                             
         MVC   CPYTYP,PCONTYP                                                   
         EJECT                                                                  
*              RATE BASIS LINES    GET X'20' RATE BASIS ELEMENTS                
***      DO NOT DISPLAY RATES - JUST CLEAR COPY RATES FIELD                     
*                                                                               
         FOUT  CPYCPYRH,SPACES,1                                                
         EJECT                                                                  
*              STANDARD COMMENTS - GET FIRST ELEM                               
DISP350  GOTO1 VGETEL,DMCB,(X'30',PCONREC),DMCB+8                               
         CLI   DMCB,X'FF'          NONE?                                        
         BE    DISP400                                                          
         FOUT  CPYSTDH                                                          
         SR    R9,R9                                                            
         IC    R9,CPYSTDH                                                       
         LA    R9,CPYSTDH-7(R9)    FIELD END-7                                  
         LM    R3,R5,DMCB+8        BXLE                                         
         LA    R6,CPYSTD-1                                                      
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
         LA    RE,CPYSTD+61        END-7                                        
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
*        MVC   CPYCM1(6),2(R3)                                                  
*        FOUT  CPYCM1H                                                          
*****                                                                           
         EJECT                                                                  
*              DISPLAY SPECIAL COMMENTS                                         
DISP400  GOTO1 VGETEL,DMCB,(X'40',PCONREC),DMCB+8                               
         CLI   DMCB,X'FF'          NONE?                                        
         BE    DISP500                                                          
*                                                                               
         LA    R6,CPYCM1H          1ST COMMENT                                  
         LM    R3,R5,DMCB+8        BXLE                                         
******                                                                          
         CLC   CPYCM1(6),SPACES                                                 
         BNH   DISP425                                                          
         LA    R6,CPYCM2H                                                       
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
         B     DISP700                                                          
*              DISPLAY                                                          
DISP700  MVC   KBAMSG(27),=C'CONTRACT DISPLAYED FOR COPY'                       
         FOUT  KBAMSGH                                                          
         B     EXIT                                                             
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
SVLEND   DS    XL3        HIGHEST END DATE FOUND                                
SVLCON   DS    XL2        CONTRACT NUMBER OF ABOVE                              
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
**PAN#1  DC    CL21'016PPCON60   11/05/03'                                      
         END                                                                    
