*          DATA SET PPCON30    AT LEVEL 018 AS OF 06/16/10                      
*PHASE T40D30A                                                                  
*INCLUDE PPBYOUT                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPCON30 - PRINTPAK MULTIPLE DISPLAY'                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SMYE 6/16/10  DELETE REFERENCE TO POWER CODES NO LONGER ON DDS SYSTEM         
*                                                                               
* SMYE  11/02   MODIFY DISB (DISPLAY BUYS) TO SWITCH TO BUY PROGRAM             
*               AND ADD PF7/PF8 "SCROLLING" AND                                 
*    (YKAP)     ISSUE NAME DISPLAY                                              
*                                                                               
* KWAN 09/20/01 REORGANIZED PPCONWRK                                            
*                                                                               
* SMYE 11/17/97 DISPLAY "H" BEFORE INS DATE IF BUY HELD AND NOT TEST            
*                                                                               
* SMYE 11/7/97  MODIFY DISB LOGIC (AT M4.. PROC) TO EXCLUDE SFH "HELD"          
*               BUYS OR INCLUDE THEM WITH "NEW" DISB.. ACTIONS                  
*                                                                               
* SMYE 10/8/97  GETINS MADE CORE-RESIDENT                                       
*                                                                               
* BPLA 2/11/97  DISPLAY INCHES UNLESS LEVEL INDICATOR IS LINES                  
*               + DON'T DISPLAY L* IN TOTALS IF NO DATA EXISTS                  
*                                                                               
* SMYE 12/6/95  CHANGED VDTCNV TO VDATCON WITH "NEW" PARAM'S.                   
*                                                                               
* BPLA 3/17/94  FIX BUG IN DISB AOR LOGIC                                       
*               SWITCH CHECK FOR MASTER CLIENT AND NEW AOR                      
* BPLA 9/29/93  MOVE SETTING PUBCNV TO M310 FROM M304B - IT                     
*               WASN'T GETTING SET FOR DUPONT SINCE DP IS NOT A                 
*               MASTER CLIENT                                                   
*                                                                               
* BPLA 9/93     USE PUBCNV TO SAVE PUB AOR PUB CNV IND                          
*               ALSO IF LINK NOT FOUND SKIP TO NEXT AOR AGY                     
*                                                                               
* BPLA 1/4/93   CHANGE TO PREVENT PACK FROM DYING WHEN THE IDIOTS               
*               PUT A VERY LARGE NUMBER IN KBANUM                               
*               ALSO REQUIRE INPUT IN KBANUM WHEN DISPLAYING BUYS               
*               FOR DISC DON'T ALLOW DATES IN KBANUM                            
*                                                                               
* BPLA 10/21/92 CHANGES FOR NEW AOR FEATURES                                    
*                IF AOR SITUATION AND I'M NOT THE AOR                           
*                I MUST SWITCH TO THE AOR TO READ CONTRACTS                     
*                                                                               
* BPLA 1/2/92 ADD LOGIC FOR ADVERTISER SCHEME (FOR DISB)                        
*                                                                               
T40D30   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40D30                                                         
         L     RC,0(R1)                                                         
         LA    R7,1(RC)                                                         
         LA    R7,4095(R7)                                                      
         USING GENOLD,RC,R7                                                     
         USING T40DFFD,RA                                                       
*                                                                               
         RELOC RELO30                                                           
*                                                                               
         USING PLINE,R2                                                         
         LA    R8,T40D30+4095                                                   
         LA    R8,1(R8)                                                         
         USING T40D30+4096,R8                                                   
         NI    TWAKIND,X'3F'       NO CONTRACT                                  
         CLI   TWASTAT,X'FD'       MULTIPLE SCREEN IN TWA?                      
         BE    M100                                                             
* CALL SCREEN                                                                   
         GOTO1 VCALLOV,DMCB,KBALAST,X'D9040DFD'                                 
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   TWASTAT,X'FD'                                                    
         B     M150                                                             
* FOUT NON-ZERO LINES WITH ZEROES                                               
M100     LA    R2,MULINFOH                                                      
*                                                                               
         OC    8(79,R2),8(R2)                                                   
         BE    M110                                                             
         XC    8(79,R2),8(R2)                                                   
         FOUT  (R2)                                                             
M110     LA    R2,MULHDLNH                                                      
         OC    8(79,R2),8(R2)                                                   
         BE    M120                                                             
         XC    8(79,R2),8(R2)                                                   
         FOUT  (R2)                                                             
*                                                                               
M120     LA    R2,MULSEL1H        POINT TO 1ST SEL FIELD                        
         SR    R4,R4                                                            
         LA    R3,MULSELFH        POINT TO LAST SEL FIELD                       
*                                                                               
M130     DS    0H                                                               
         IC    R4,0(R2)                                                         
         AR    R2,R4              NEXT FIELD (MULLIN..)                         
         CR    R2,R3              LAST LINE ?                                   
         BNL   M150               YES                                           
         OC    8(75,R2),8(R2)                                                   
         BE    M135                                                             
         XC    8(75,R2),8(R2)                                                   
         FOUT  (R2)                                                             
M135     IC    R4,0(R2)                                                         
         AR    R2,R4              NEXT FIELD (MULSEL..)                         
         CLI   0(R2),0            LAST ?                                        
         BNE   M130               NO                                            
*                                                                               
M150     DS    0H                 PROTECT ALL "SEL" FIELDS                      
         SR    R4,R4                                                            
         LA    R2,MULSEL1H        1ST SEL FIELD                                 
         LA    R3,MULSELFH        LAST SEL FIELD                                
M150F    DS    0H                                                               
         OI    1(R2),X'20'        PROTECT AND XMIT FIELD                        
         OI    6(R2),X'80'                                                      
         IC    R4,0(R2)                                                         
         AR    R2,R4              POINT TO MULLIN..                             
         IC    R4,0(R2)                                                         
         AR    R2,R4              POINT TO NEXT MULSEL..                        
         CR    R2,R3                                                            
         BL    M150F              PROTECT NEXT MULSEL.. FIELD                   
*                                                                               
         XC    RPAGE,RPAGE                                                      
         LA    R2,KBAPAGH          PAGE                                         
         BAS   RE,PACK                                                          
         LTR   R0,R0                                                            
         BP    *+8                                                              
         LA    R0,1                NO PAGE                                      
         ST    R0,RPAGE             SAVE REQUESTED PAGE                         
         CLI   KBAPAG,C'T'         TOTAL PAGE?                                  
         BNE   M151                                                             
         MVC   RPAGE,=F'1000'                                                   
         B     M152                                                             
*                                                                               
M151     DS    0H                                                               
         CLC   KBAACT(4),=C'DISB'                                               
         BNE   M152                                                             
*                                                                               
         L     RF,ATIOB            SET CURSOR INF FROM TIOB                     
         USING TIOBD,RF                                                         
         MVC   PFKEY,TIOBAID       SAVE PFKEY DATA                              
         DROP  RF                                                               
         SR    R0,R0                                                            
         IC    R0,PFKEY                                                         
         CLI   PFKEY,12            TEST FOR PF 13 - 24                          
         BNH   *+8                                                              
         SH    R0,=H'12'           CONVERT TO 1 - 12                            
         STC   R0,PFKEY                                                         
*                                                                               
         CLI   PFKEY,7            GO "BACK" 1 PAGE ?                            
         BE    *+12               YES                                           
         CLI   PFKEY,8            GO "FORWARD" 1 PAGE ?                         
         BNE   M152               NO                                            
*                                                                               
         L     RE,RPAGE           CURRENT "PAGE NUMBER"                         
         CLI   PFKEY,7            GO "BACK" 1 PAGE ?                            
         BNE   *+12               NO                                            
         AHI   RE,-1                                                            
         B     *+8                                                              
         AHI   RE,1                                                             
         LTR   RE,RE                                                            
         BZ    M152                                                             
         ST    RE,RPAGE                                                         
         XC    KBAPAG,KBAPAG      CLEAR PAGE NUMBER ON SCREEN                   
         EDIT  (RE),(3,WORK2),ALIGN=LEFT                                        
         LR    RE,R0              NO OF SIGNIFICANT CHARACTERS                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KBAPAG(0),WORK2    PAGE NUMBER                                   
         MVI   PFKEY,0                                                          
*                                                                               
M152     DS    0H                                                               
         XCEFL SBUYTAB,378        CLEAR TABLE OF KEYS OF BUYS DISPLAYED         
         MVC   SBUYTABX,=X'FFFF'   AND THEIR DISPLACEMENT INTO SCREEN           
*                                                                               
         FOUT  MULINFOH                                                         
         FOUT  MULHDLNH                                                         
*                                                                               
*                                                                               
* GET CONTRACT                                                                  
         LA    R2,KBANUMH          K NUMBER                                     
         XC    HALF,HALF                                                        
         LA    R3,2                INVALID ERROR                                
         CLI   5(R2),0             SEE IF NUMBER MISSING                        
         BE    M154                                                             
*                                                                               
         TM    KBANUMH+4,X'08'     SEE IF NUMERIC                               
         BNO   M155                NO - THEN EDIT FOR DATES                     
*                                                                               
         CLI   5(R2),5                                                          
         BH    ERROR               THIS SHOULD PREVENT PACK FROM DYING          
         BAS   RE,PACK                                                          
         STH   R0,HALF                                                          
         CH    R0,=H'999'          MAX CONTRACT NUMBER                          
         BH    ERROR                                                            
*                                                                               
M154     CLC   KBAACT(4),=C'DISC'                                               
         BE    M200                                                             
* DISPLAY BUYS                                                                  
         LA    R3,1                FIELD MISSING                                
         CLI   5(R2),0             CHECK FOR INPUT                              
         BE    ERROR                                                            
         B     M160                DISPLAY BUYS                                 
* SEE IF VALID DATES GIVEN                                                      
*                                                                               
M155     DS    0H                                                               
         CLC   KBAACT(4),=C'DISC'  NO DATES FOR DISC                            
         BE    ERROR                                                            
         LA    R3,20               DATE ERROR                                   
         GOTO1 VDATVAL,DMCB,KBANUM,WORK                                         
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         L     R4,DMCB             LEN                                          
         LA    R4,KBANUM+1(R4)     NEXT DATE                                    
         GOTO1 (RF),(R1),(R4),WORK+6                                            
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
* PUT DATES IN K AREA                                                           
         GOTO1 VDATCON,(R1),(0,WORK),(3,PCONSDT)                                
         GOTO1 VDATCON,(R1),(0,WORK+6),(3,PCONEDT)                              
         MVC   KEY(13),SAVKKEY                                                  
         CLC   PCONSDT,PCONEDT                                                  
         BH    ERROR                                                            
         B     M170                                                             
         SPACE 1                                                                
M160     DS    0H                                                               
         LA    R3,1                MISSING INPUT                                
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         LA    R3,53               REC NOT FOUND                                
         MVC   KEY(13),SAVKKEY     K KEY                                        
         MVC   KEY+13(2),HALF                                                   
******                                                                          
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    M163                                                             
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    M163                                                             
         TM    SADVDATA+15,X'20'                                                
         BZ    M163                                                             
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
         BE    M163                                                             
         MVC   KBAMSG,=CL60'** AOR SYSTEM NOT ACTIVE **'                        
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
M163     BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    M163G                                                            
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    M163E                                                            
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    M163E                                                            
         TM    SADVDATA+15,X'20'                                                
         BZ    M163E                                                            
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
M163E    B     ERROR                                                            
*                                                                               
*                                                                               
M163G    DS    0H                                                               
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
         BAS   RE,GETREC           PRTFILE -K                                   
         LA    RF,IOAREA           RESTORE AREC                                 
         ST    RF,AREC                                                          
*                                                                               
         OC    SADVDATA(18),SADVDATA     SEE IF AOR SITUATION                   
         BZ    M170                       DON'T NEED TO SWITCH BACK             
         CLC   SADVDATA(2),AGYALPHA      SEE IF I AM THE AOR                    
         BE    M170                      DON'T NEED TO SWITCH BACK              
         TM    SADVDATA+15,X'20'                                                
         BZ    M170                                                             
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
*NOP*M170     LA    R2,MULHDLNH         DSECT                                   
M170     LA    R2,MULHDLNH+4       DSECT                                        
         LA    R9,1                PAGE NUMBER                                  
         ST    R9,PAGENM                                                        
         LA    R9,1                MAX DATA LINES CTR                           
*                                                                               
         CLI   ADVSW,0                                                          
         BNE   M170C                                                            
*                                                                               
         CLI   AGYAGYR,0                                                        
         BE    *+10                                                             
M170C    MVC   PAGY(3),=C'AGY'                                                  
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BNZ   *+10               SWAP TO BUY NOT ALLOWED                       
         MVC   PAGY-4(3),=C'SEL'                                                
         CLI   SAVCLTPR+5,C'1'     MASTER CLIENT?                               
         BNE   *+10                                                             
         MVC   PCLT,=C'CLT'                                                     
         MVC   PPRD(65),=C'PRD  EST   INS DATE AD. NO.  SPACE DESCRIPTIX        
               ON         GROSS COST'                                           
         CLC   KBAACT+4(2),=C'GL'                                               
         BNE   *+14                                                             
         MVC   PGRS,=C' GROSS LESS CD'                                          
         B     M171                                                             
         CLC   KBAACT+4(2),=C'NL'                                               
         BNE   *+14                                                             
         MVC   PGRS,=C'   NET LESS CD'                                          
         B     M171                                                             
         CLI   KBAACT+4,C'N'                                                    
         BNE   *+10                                                             
         MVC   PGRS,=C'      NET COST'                                          
M171     DS    0H                                                               
*                                                                               
         CLI   KBAMED,C'N'                                                      
         BE    M172                                                             
         CLI   KBAMED,C'O'                                                      
         BNE   M300                                                             
         MVC   PSPACE(16),=C'SHOW  REG  ILLUM'                                  
         B     M300                                                             
M172     DS    0H                                                               
         MVC   PSPACE,=C'SPACE    RATE       '                                  
         B     M300                                                             
         EJECT                                                                  
* DISPLAY CONTRACTS                                                             
M200     DS    0H                 PROTECT ALL "SEL" FIELDS                      
         SR    R4,R4                                                            
         LA    R2,MULSEL1H        1ST SEL FIELD                                 
         LA    R3,MULSELFH        LAST SEL FIELD                                
M200L    OI    1(R2),X'20'        PROTECT AND XMIT FIELD                        
         OI    6(R2),X'80'                                                      
         IC    R4,0(R2)                                                         
         AR    R2,R4              POINT TO MULLIN..                             
         IC    R4,0(R2)                                                         
         AR    R2,R4              POINT TO NEXT MULSEL..                        
         CR    R2,R3                                                            
         BL    M200L              PROTECT NEXT MULSEL.. FIELD                   
*                                                                               
         MVC   MULHDLN(28),=C'    NUMBER         START-END'                     
         MVC   KEY(13),SAVKKEY                                                  
         LA    R2,MULLIN1H         FIRST DATA LINE                              
         MVC   KEY+13(2),HALF      START K NUMBER                               
         XC    MULLINF,MULLINF                                                  
         FOUT  MULLINFH            LAST LINE                                    
*                                                                               
***                                                                             
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    M203                                                             
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    M203                                                             
         TM    SADVDATA+15,X'20'                                                
         BZ    M203                                                             
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
         BE    M203                                                             
         MVC   KBAMSG,=CL60'** AOR SYSTEM NOT ACTIVE **'                        
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
M203     BAS   RE,HIGH                                                          
         B     *+8                                                              
M250     BAS   RE,SEQ              NEXT K                                       
         CLC   KEY(13),KEYSAVE                                                  
         BE    M275                                                             
M255     MVC   KBAMSG(L'KBAMSG),SPACES                                          
         MVC   KBAMSG(19),=C'CONTRACTS DISPLAYED'                               
         FOUT  KBAPAGH,SPACES,3                                                 
M260     DS    0H                                                               
         CLC   KBAACT(4),=C'DISB'                                               
         BNE   M260F                                                            
         OI    KBAACTH+1,X'01'    SET MODIFIED                                  
         FOUT  KBAACTH            AND XMIT                                      
         B     M260M                                                            
M260F    DS    0H                                                               
         FOUT  KBAACTH,SPACES,8                                                 
M260M    OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BZ    M263                                                             
         CLC   SADVDATA(2),AGYALPHA       SEE IF I AM THE AOR                   
         BE    M263                                                             
         TM    SADVDATA+15,X'20'                                                
         BZ    M263                                                             
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
M263     DS    0H                                                               
         CLC   KBAACT(4),=C'DISB'                                               
         BNE   M270                                                             
*                                                                               
         L     RF,ATIOB            SET CURSOR INF FROM TIOB                     
         USING TIOBD,RF                                                         
         MVC   PFKEY,TIOBAID       SAVE PFKEY DATA                              
         DROP  RF                                                               
         SR    R0,R0                                                            
         IC    R0,PFKEY                                                         
         CLI   PFKEY,12            TEST FOR PF 13 - 24                          
         BNH   *+8                                                              
         SH    R0,=H'12'           CONVERT TO 1 - 12                            
         STC   R0,PFKEY                                                         
         CLI   PFKEY,2            GO TO BUY ?                                   
         BE    M263E              YES                                           
*                                 SEE IF LINE (S)ELECTED                        
         LA    R2,MULSEL1H        POINT TO 1ST SEL FIELD                        
         SR    R4,R4                                                            
         LA    R3,MULSELFH        POINT TO LAST SEL FIELD                       
*                                                                               
M263C    DS    0H                                                               
         CLI   8(R2),C'S'         GO TO BUY ?                                   
         BNE   M263D              NO                                            
         XC    8(2,R2),8(R2)                                                    
         FOUT  (R2)                                                             
         B     M263E                                                            
M263D    IC    R4,0(R2)                                                         
         AR    R2,R4              NEXT FIELD (MULLIN..)                         
         IC    R4,0(R2)                                                         
         AR    R2,R4              NEXT FIELD (MULSEL..)                         
         CR    R2,R3              LAST ?                                        
         BNL   M270               YES                                           
         B     M263C                                                            
*                                                                               
M263E    DS    0H                                                               
         MVI   PFKEY,0                                                          
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BNZ   M270               SWAP TO BUY NOT ALLOWED                       
         LA    R6,SBUYTAB         POINT TO START OF TABLE                       
         CLI   2(R6),C' '         ANY BUYS TO DISPLAY IN BUY PGM ?              
         BNH   M270               NO                                            
*                                                                               
*        FIND ENTRY (BUY KEY) CORRESPONDING TO CURSOR POSITION                  
*                                                                               
M263G    DS    0H                                                               
         L     RF,ATIOB            SET CURSOR INF FROM TIOB                     
         USING TIOBD,RF                                                         
         CLC   TIOBCURD,0(R6)      MUST BE AFTER START OF ENTRY                 
         BL    M270                CURSOR NOT IN BUY DISPLAY AREA               
         CLC   TIOBCURD,27(R6)     AND BEFORE START OF NEXT ENTRY               
         BL    M263J                                                            
         DROP  RF                                                               
         LA    R6,L'SBUYTAB(R6)            BUMP TO NEXT TABLE ENTRY             
         B     M263G                                                            
*                                                                               
M263J    XC    WORK2,WORK2                                                      
         MVC   WORK2(25),2(R6)      SAVE THE ENTRY (BUY KEY)                    
*                                                                               
*            **********  TRANSFER TO BUY  **********                            
*                                                                               
         XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'PRI'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'CON'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'PRI'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'BUY'    SET TO   PROGRAM                             
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
*                                                                               
         GOTO1 GOGLOB,DMCB,=C'PUTD',WORK,13,GLVXCTL    SEND XCTL ELM            
         GOTO1 GOGLOB,DMCB,=C'PUTD',WORK2,25,GLVPRMAT   SEND BUY KEY            
*                                                                               
         GOTO1 GOGLOB,DMCB,=C'PUTF',KBAMEDH,,GLVPRMD         MEDIA              
         GOTO1 GOGLOB,DMCB,=C'PUTF',KBAPUBH,,GLVPRPUB        PUB                
*                                                                               
         LA    R6,WORK2           BUY KEY HERE                                  
         USING PBUYKEY,R6                                                       
         GOTO1 GOGLOB,DMCB,=C'PUTD',PBUYKCLT,3,GLVPRCLT      CLT                
         GOTO1 GOGLOB,DMCB,=C'PUTD',PBUYKPRD,3,GLVPRPRD      PRD                
         GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(4,WORK2+40)  MMMDD                    
         CLI   PBUYKLIN,1                                                       
         BE    M263M                                                            
         MVI   WORK2+45,C'-'                                                    
         EDIT  (1,PBUYKLIN),(2,WORK2+46),ALIGN=LEFT                             
M263M    GOTO1 GOGLOB,DMCB,=C'PUTD',WORK2+40,8,GLVPRPER      DATE               
*                                                                               
         EDIT  (2,PBUYKEST),(3,WORK2+50),ALIGN=LEFT                             
         CLI   WORK2+52,C'0'      NUMERIC ?                                     
         BNL   *+8                YES                                           
         MVI   WORK2+52,0         REPLACE WITH NULL                             
         CLI   WORK2+51,C'0'      NUMERIC ?                                     
         BNL   *+8                YES                                           
         MVI   WORK2+51,0         REPLACE WITH NULL                             
         GOTO1 GOGLOB,DMCB,=C'PUTD',WORK2+50,4,GLVPREST      EST                
*                                                                               
         DROP  R6                                                               
*NOP*    XC    WORK2,WORK2                                                      
*NOP*    LA    R6,WORK2                                                         
*NOP*    USING CONXFERD,R6                                                      
*NOP*    MVC   CONXACT(L'CONXACT),KBAACT       8-BYTE ACTION FIELD              
*NOP*    MVC   CONXNUM(L'CONXNUM),KBANUM      17-BYTE "NUMBER" FIELD            
*NOP*    MVC   CONXPAG(L'CONXPAG),KBANUM       3-BYTE PAGE NUMBER               
*NOP*    GOTO1 GOGLOB,DMCB,=C'PUTD',WORK2,28,GLVPRDTA   RETURN DATA             
*NOP*    DROP  R6                                                               
*NOP*    GOTO1 GOGLOB,DMCB,=C'PUTD',=C'CON',3,GLVPRREQ  GLOBR INDICATOR         
*                                                                               
*NOP*    MVC   KBAMSG(15),=C'*BACK FROM BUY*'                                   
*                                                                               
M270     DS    0H                                                               
         LA    R2,KBAACTH          CURSOR                                       
         B     EXIT                                                             
*                                                                               
M275     DS    0H                                                               
         LA    RF,PCONREC          READ INTO CONTRACT AREA                      
         ST    RF,AREC                                                          
         BAS   RE,GETREC                                                        
         LA    RF,IOAREA           RESTORE AREC                                 
         ST    RF,AREC                                                          
*                                                                               
         CLI   SAVPRD,0            SEE IF PRD SPECIFIED                         
         BE    M277                NO - DISPALY ALL CONS                        
         CLI   PCONPRD,C'A'        YES - IS THIS A PRD CONTRACT                 
         BL    M277                NO - OK TO DISPLAY                           
         CLC   PCONPRD,SAVPRD      YES - MUST MATCH PRD                         
         BE    M277                                                             
         B     M250                NO - GO CHECK NEXT                           
*                                                                               
M277     DS    0H                                                               
         EDIT  (2,KEY+13),(4,8(R2))                                             
*                                                                               
         CLI   PCONPRD,C'A'        SEE IF A PRD-CONTRACT                        
         BL    *+10                                                             
         MVC   15(3,R2),PCONPRD                                                 
         GOTO1 VDATCON,DMCB,(3,PCONSDT),(5,20(R2))                              
         MVI   28(R2),C'-'                                                      
         GOTO1 VDATCON,(R1),(3,PCONEDT),(5,29(R2))                              
         FOUT  (R2)                                                             
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         AR    R2,R4              POINT TO MULSEL..                             
         IC    R4,0(R2)                                                         
         AR    R2,R4              POINT TO MULLIN..                             
*                                                                               
         LA    R3,MULLINFH        LAST DATA LINE                                
         CR    R2,R3                                                            
         BNL   M255                                                             
*                                                                               
         CLI   0(R2),0                                                          
         BE    M255                                                             
         B     M250                                                             
         EJECT                                                                  
* DISPLAY BUY LINES FOR K                                                       
M300     GOTO1 VDATCON,DMCB,(3,PCONSDT),(5,MULINFO)                             
         MVI   MULINFO+8,C'-'                                                   
         GOTO1 VDATCON,(R1),(3,PCONEDT),(5,MULINFO+9)                           
         LA    R2,MULLIN1H         FIRST O/P LINE                               
*                                                                               
         XC    AGYLST,AGYLST                                                    
         MVC   AGYLST(2),AGYALPHA       THIS AGY                                
         CLI   AGYAGYR,0           TEST ADVERTISER (DUPONT SCHEME)              
         BE    M303                NO                                           
*                                  VES - BUILD AGYLST                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),KBAMED                                                  
         MVI   KEY+3,X'02'                                                      
         LA    R4,15               MAX       WAS 11                             
         LA    R5,AGYLST                                                        
         BAS   RE,HIGH                                                          
         B     *+8                                                              
M302     DS    0H                                                               
         BAS   RE,SEQ                                                           
         CLC   KEY(4),KEYSAVE                                                   
         BNE   M303                                                             
         BCT   R4,*+6                                                           
         DC    H'0'                TOO MANY AGY'S                               
         BAS   RE,GETREC                                                        
         TM    PCLTACTL,X'10'      BYPASS ADVERTISER ITLELF                     
         BNZ   M302                                                             
         MVC   0(2,R5),PCLTKCLT                                                 
         MVC   2(1,R5),PCLTACTL                                                 
         LA    R5,4(R5)             WAS 3                                       
         B     M302                                                             
*                                                                               
M303     DS    0H                                                               
***ADV                                                                          
         CLI   ADVSW,0             SEE IF NEW ADV SCHEME                        
         BE    M303X                                                            
*                                 BUILD AGYLST FROM AADVCTAB                    
         XC    AGYLST,AGYLST                                                    
*                                                                               
         L     R1,AADVCTAB                                                      
         USING GETADVCD,R1                                                      
         LA    R5,AGYLST                                                        
         XC    FULL,FULL                                                        
M303B    CLC   0(2,R1),=X'FFFF'    END OF TABLE                                 
         BE    M303X                                                            
         CLC   FULL(2),GETVAGY    SEE IF SAME AGY                               
         BE    M303N                                                            
         MVC   0(2,R5),GETVAGY                                                  
         MVC   3(1,R5),GETVSE      STORE SE NUMBER                              
         LA    R5,4(R5)                                                         
         MVC   FULL(2),GETVAGY     SAVE AGY I JUST DID                          
*                                                                               
M303N    LA    R1,GETVLEN(R1)                                                   
         B     M303B                                                            
*                                                                               
         DROP  R1                                                               
*                                                                               
M303X    XC    RPTTOTS,RPTTOTS                                                  
         LA    R5,AGYLST                                                        
         ST    R5,AGYPNTR                                                       
*                                                                               
M304     DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    M370                END OF REPORT                                
         L     RE,ACLTLST          POINT TO CLTLST                              
         LA    RF,1500             500 SUB-CLIENT CODES X 3 PER                 
         XCEFL                                                                  
         L     RE,ACLTLST          POINT TO CLTLST                              
         AHI   RE,-8                                                            
         MVC   0(8,RE),=C'*CLTLST*'                                             
*NOP*    XC    CLTLST,CLTLST         SLAVE CLIENT LIST                          
*NOP*    MVC   CLTLST(3),SAVCLT      ENTERED CLIENT                             
         L     RE,ACLTLST          POINT TO CLTLST                              
         MVC   0(3,RE),SAVCLT      ENTERED CLIENT                               
         SPACE 1                                                                
         CLI   AGYAGYR,0           IF ADVERTISER                                
         BE    M304B                                                            
         MVI   SAVCLTPR+5,C'0'                                                  
         TM    2(R5),X'04'         TEST IF MAST/SLAVE AGY                       
         BZ    *+8                                                              
         MVI   SAVCLTPR+5,C'1'     YES                                          
M304B    DS    0H                                                               
*                                                                               
         CLI   ADVSW,0             SEE IF NEW ADV SCHEME                        
         BNE   M304C               YES                                          
*                                                                               
         CLI   SAVCLTPR+5,C'1'     MASTER CLIENT?                               
         BNE   M309                                                             
         B     M304X             BUILD SLAVE CLT LIST AS BEFORE                 
*                                                                               
*                               BUILD CLTLST FROM AADVCTAB                      
M304C    L     R1,AADVCTAB                                                      
         USING GETADVCD,R1                                                      
*NOP*    LA    R3,CLTLST                                                        
         L     R3,ACLTLST          POINT TO CLTLST                              
M304F    CLC   0(2,R1),=X'FFFF'                                                 
         BE    M309                   END OF LIST                               
         CLC   0(2,R5),GETVAGY        CHECK RIGHT AGENCY                        
         BNE   M304N                                                            
*                                                                               
         TM    GETVCNTL,X'01'         SEE IF PUB CON VERSION REQ                
         BZ    *+8                                                              
         MVI   PUBCNV,C'Y'                                                      
*                                                                               
         MVC   0(3,R3),GETVACLT                                                 
         OC    0(3,R3),SPACES                                                   
         LA    R3,3(R3)                                                         
*                                                                               
M304N    LA    R1,GETVLEN(R1)                                                   
         B     M304F                                                            
*                                                                               
***ADV                                                                          
* GET SLAVE CLIENT LIST                                                         
M304X    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),0(R5)                                                     
         MVC   KEY+2(1),SAVKMED                                                 
         MVI   KEY+3,2                                                          
*NOP*    LA    R5,CLTLST+3         LEAVE MASTER IN LIST                         
         L     R5,ACLTLST          POINT TO CLTLST                              
         LA    R5,3(R5)            LEAVE MASTER IN LIST                         
*NOP*    LA    R4,82               BCT (MAX SUB CLTS -1)                        
         LA    R4,499              BCT (MAX SUB CLTS -1)                        
         BAS   RE,HIGH                                                          
         B     M307                                                             
         SPACE 1                                                                
M305     BCT   R4,M306                                                          
         LA    R3,188              MAX CLIENTS EXCEEDED                         
         LA    R2,KBAACTH                                                       
         B     ERROR                                                            
         SPACE 1                                                                
M306     BAS   RE,SEQ                                                           
M307     CLC   KEY(4),KEYSAVE      SAME AGENCY-MEDIA?                           
         BNE   M309                                                             
         BAS   RE,GETREC                                                        
         CLC   PCLTPROF+6(3),SAVKCLT   SAME MASTER?                             
         BNE   M306                                                             
         MVC   0(3,R5),KEY+4       SAVE SLAVE CLIENT                            
         LA    R5,3(R5)                                                         
         B     M305                                                             
         SPACE 1                                                                
M309     DS    0H                                                               
         XC    AGYTOTS,AGYTOTS                                                  
*                                                                               
         SPACE 1                                                                
*                                  READ BUYS FOR AGY/CLT                        
M310     DS    0H                                                               
         XC    KEYSAVE,KEYSAVE                                                  
         L     R5,AGYPNTR                                                       
***ADV                                                                          
         CLI   ADVSW,0           SEE IF NEW ADV SCHEME                          
         BE    M310P             NO                                             
*                                 MUST SWITCH TO AGENCIES FILE                  
*                                                                               
         MVI   PUBCNV,0              ZERO PUB CONVERSION IND                    
*                                                                               
*                     SEE IF PUB CONVERSION REQUIRED FOR THIS AGY               
         L     R1,AADVCTAB                                                      
         USING GETADVCD,R1                                                      
M310F    CLC   0(2,R1),=X'FFFF'                                                 
         BE    M310H                  END OF LIST                               
         CLC   0(2,R5),GETVAGY        CHECK RIGHT AGENCY                        
         BNE   M310G                                                            
*                                                                               
         TM    GETVCNTL,X'01'         SEE IF PUB CON VERSION REQ                
         BZ    *+8                                                              
         MVI   PUBCNV,C'Y'                                                      
         B     M310H                                                            
*                                                                               
M310G    LA    R1,GETVLEN(R1)                                                   
         B     M310F                                                            
*                                                                               
M310H    DS    0H                                                               
         CLC   LASTSE,3(R5)       SEE IF ALREADY THERE                          
         BE    M310P                                                            
         MVC   LASTSE,3(R5)                                                     
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),3(R5)        SE NUMBER                                   
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    M310P                                                            
         MVC   KBAMSG,=CL60'*** AGENCY FILE NOT ACTIVE ***'                     
         LA    R2,KBAMEDH                                                       
         NI    KBAMEDH+4,X'DF'      UNVALIDATE MEDIA                            
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
M310P    MVC   KEYSAVE(2),0(R5)                                                 
         MVC   KEYSAVE+2(1),SAVKMED                                             
         MVI   KEYSAVE+3,X'21'                                                  
         MVC   KEYSAVE+7(6),SAVPUB                                              
         MVC   KEYSAVE+13(3),SAVPRD                                             
*                                                                               
***ADV                                                                          
         CLI   ADVSW,0             SEE IF NEW ADV SCHEME                        
         BE    M310W               NO                                           
*                                                                               
         CLI   PUBCNV,0            SEE IF PUB CONVERSION REQUIRED               
         BE    M312                                                             
*                                                                               
         MVC   WORK2(64),KEY       SAVE KEY AND KEYSAVE                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'FD'                                                        
         MVC   KEY+1(1),SAVKMED                                                 
         MVC   KEY+2(3),SAVKCLT    ADV - MASTER CLIENT CODE                     
         MVC   KEY+5(2),AGYALPHA   AOR                                          
         MVC   KEY+7(2),0(R5)      AGENCY                                       
         MVC   KEY+9(6),SAVPUB     ADV PUB CODE                                 
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(15),KEYSAVE                                                  
         BNE   M310Q                                                            
         MVC   WORK2+70(6),KEY+15   SAVE AGY PUB CODE                           
         MVC   KEY(64),WORK2                                                    
         MVC   KEYSAVE+7(6),WORK2+70                                            
         B     M312                                                             
*                                                                               
M310Q    MVC   KEY(64),WORK2       LINK NOT FOUND  - SKIP TO NEXT AGY           
         B     M360                                                             
*                                                                               
*        SEARCH FOR PUBLINK PASSIVE POINTER TO GET AGENCY PUB                   
*                                                                               
*                                                                               
M310W    TM    2(R5),X'80'         TEST PUB CONVERSION NEEDED                   
         BZ    M312                NO                                           
*                                  YES- FIND *AG 14 ELEM                        
*                                       IN AGY OF REC PUB                       
         MVC   KEY+27(4),SAVPUBA                                                
         BAS   RE,GETPUB                                                        
         L     R6,APUBIO                                                        
         USING PUBREC,R6                                                        
         LA    R4,PUBREC+33                                                     
         SR    R0,R0                                                            
M311     DS    0H                                                               
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    M312                IF NO ELEM, USE SAME PUB NO.                 
         CLI   0(R4),X'14'                                                      
         BNE   M311                                                             
         USING PUBREPEL,R4                                                      
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   M311                                                             
         CLC   PUBRPOFF+1(2),0(R5)                                              
         BNE   M311                                                             
         MVC   KEYSAVE+7(6),PUBCVEN                                             
*                                                                               
M312     DS    0H                                                               
*                                                                               
*NOP*    LA    R5,CLTLST           GET FIRST CLIENT                             
         L     R5,ACLTLST          GET FIRST CLIENT                             
         ST    R5,CLTPNTR                                                       
*                                                                               
M314     DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    M360                END OF AGY                                   
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+4(3),0(R5)      NEXT CLIENT                                  
         BAS   RE,HIGH             FIRST BUY FOR CLIENT                         
         B     *+8                                                              
M350     BAS   RE,SEQ              NEXT BUY                                     
         CLC   KEY(13),KEYSAVE     SAME CLIENT?                                 
         BNE   M351                                                             
         CLI   SAVPRD,0            SEE IF DOING ONE PRD                         
         BE    M400                NO                                           
         CLC   KEY(16),KEYSAVE     YES - MATCH PRDS                             
         BE    M400                                                             
         DROP  R6                                                               
*                                                                               
M351     DS    0H                  END OF CLT OR PRD                            
         SPACE 1                                                                
*                                       END OF CLIENT                           
         CLI   SAVCLTPR+5,C'1'          TEST MASTER/SLAVE                       
         BNE   M352                                                             
         OC    CLTTOTS,CLTTOTS                                                  
         BZ    M353                                                             
         BAS   RE,NXTLIN                                                        
         LA    RF,M600                                                          
         GOTO1 (RF),DMCB,(C'C',CLTTOTS)                                         
*                                                                               
M352     DS    0H                                                               
         LM    R4,R6,CLTTOTS            ROLL TO AGYTOTS                         
         A     R4,AGYTOTS                                                       
         A     R5,AGYTOTS+4                                                     
         A     R6,AGYTOTS+8                                                     
         STM   R4,R6,AGYTOTS                                                    
         XC    CLTTOTS,CLTTOTS                                                  
*                                                                               
M353     DS    0H                                                               
         L     R5,CLTPNTR          GET NEXT CLIENT                              
         LA    R5,3(R5)                                                         
         ST    R5,CLTPNTR                                                       
         B     M314                                                             
*                                                                               
*                                  END OF AGY                                   
M360     DS    0H                                                               
***ADV                                                                          
         CLI   ADVSW,0             TEST IF NEW ADV SCHEME                       
         BNE   M360C               YES DO AGY TOTALS                            
         CLI   AGYAGYR,0           TEST ADVERTISER                              
         BE    M362                NO                                           
M360C    OC    AGYTOTS,AGYTOTS                                                  
         BZ    M363                                                             
         BAS   RE,NXTLIN                                                        
         LA    RF,M600                                                          
         GOTO1 (RF),DMCB,(C'A',AGYTOTS)                                         
*                                                                               
M362     DS    0H                                                               
         LM    R4,R6,AGYTOTS       ROLL TO RPT TOTS                             
         A     R4,RPTTOTS                                                       
         A     R5,RPTTOTS+4                                                     
         A     R6,RPTTOTS+8                                                     
         STM   R4,R6,RPTTOTS                                                    
         XC    AGYTOTS,AGYTOTS                                                  
*                                                                               
M363     DS    0H                                                               
         L     R5,AGYPNTR          GET NEXT AGY                                 
         LA    R5,4(R5)            WAS 3                                        
         ST    R5,AGYPNTR                                                       
         B     M304                                                             
*                                  END OF REPORT                                
M370     DS    0H                                                               
***ADV                                                                          
         CLI   ADVSW,0                                                          
         BE    M370X                                                            
*                                 MUST SWITCH BACK TO MY SYSTEM                 
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
M370X    DS    0H                                                               
         BAS   RE,NXTLIN                                                        
         LA    RF,M600                                                          
         GOTO1 (RF),DMCB,(C'R',RPTTOTS)                                         
*                                                                               
         B     M700                                                             
*                                                                               
* GET BUYREC                                                                    
M400     CLC   KEY+16(3),PCONSDT   BUY V K START DATE                           
         BL    M350                                                             
         CLC   KEY+16(3),PCONEDT                                                
         BH    M350                                                             
         LA    RE,IOAREA                                                        
         ST    RE,AREC                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         CLC   KBAACT(5),=C'DISBT'     SEE IF INCLUDING TEST BUYS               
         BE    M402                    GROSS                                    
         CLC   KBAACT(6),=C'DISBNT'    SEE IF INCLUDING TEST BUYS               
         BE    M402                    NET                                      
**TESTBUY**                                                                     
         CLI   PBDBFD,C'T'          IGNORE TEST BUYS                            
         BE    M350                 SKIP                                        
**TESTBUY**                                                                     
*                                                                               
**HELDBUY**                                                                     
*                                                                               
M402     DS    0H      **** TEST FOR INCLUDING OR EXCLUDING HELD BUYS           
         CLI   KBAACT+4,C'H'   INCLUDE HELD BUYS ? (DISBH)                      
         BE    M410            YES                                              
         CLI   KBAACT+5,C'H'   INCLUDE HELD BUYS ? (DISBNH OR DISBTH)           
         BE    M410            YES                                              
         CLI   KBAACT+6,C'H'   INCLUDE HELD BUYS ? (DISBNTH)                    
         BE    M410            YES                                              
*                      **** DO NOT INCLUDE HELD BUYS                            
         TM    PBDSTAT,X'08'       HELD BUY ?                                   
         BO    M350                YES - SKIP                                   
**HELDBUY**                                                                     
*                                                                               
M410     XC    WORK2,WORK2                                                      
* GETINS                                                                        
*****    GOTO1 =V(GETINS),DMCB,IOAREA,WORK2,KEY+13,RR=RELO30                    
         GOTO1 VGETINS,DMCB,IOAREA,WORK2,KEY+13                                 
*                                                                               
         L     R5,APBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
*                                                                               
         LA    RF,IOAREA                                                        
         ST    RF,PBYOINPT                                                      
         MVC   PBYODTCN,VDATCON                                                 
         LA    RF,WORK2                                                         
         ST    RF,PBYOVALS                                                      
*                                                                               
         MVI   PBYOCTL,X'4A'       OUTDOOR SPACE AND COMMENTS                   
*                                                                               
         GOTO1 =V(PPBYOUT),DMCB,PPBYOUTD,RR=RELO30                              
*                                                                               
         L     RF,PBYOLNS                                                       
         CLI   PBDCL,0                                                          
         BE    *+8                                                              
         LA    RF,1(RF)            PREM LINE                                    
         AR    RF,R9               ADD LINES SO FAR                             
         CH    RF,=H'15'                                                        
         BNH   *+8                                                              
         BAS   RE,CHKPAG                                                        
*                                                                               
* DISPLAY BUY     ************************************************              
*                                                                               
*            SAVE KEY OF BUY TO BE DISPLAYED                                    
*             DISPLACEMENT INTO SCREEN WILL                                     
*                   BE SET AT M700                                              
         LA    R1,SBUYTAB                                                       
         LA    RF,14              MAXIMUM DISPLAYABLE BUYS                      
M410A    DS    0H                                                               
         CLI   2(R1),C' '         BUY KEY THERE ?                               
         BNH   M410AX             NO - SAVE THIS BUY KEY                        
         LA    R1,L'SBUYTAB(R1)   BUMP TO NEXT ENTRY                            
         BCT   RF,M410A                                                         
         DC    H'0'               MORE THAN 14 BUYS                             
M410AX   DS    0H                                                               
         MVC   2(L'PBUYKEY,R1),PBUYKEY                                          
         MVC   27(2,R1),=X'FFFF'                                                
*                                                                               
         MVI   PLINEL,C' '                                                      
         MVC   PLINEL+1(74),PLINEL                                              
***ADV                                                                          
         CLI   ADVSW,0               SEE IF NEW ADV SCHEME                      
         BNE   M410C                 YES DISPLAY AGENCY                         
         CLI   AGYAGYR,0                                                        
         BE    *+10                                                             
M410C    MVC   PAGY,PBUYKAGY                                                    
         CLI   PBDJOB,C' '                                                      
         BNH   *+10                                                             
         MVC   PJOB,PBDJOB                                                      
         CLI   SAVCLTPR+5,C'1'     TEST MASTER                                  
         BNE   *+10                                                             
         MVC   PCLT,PBUYKCLT                                                    
         MVC   PPRD,PBUYKPRD       PRODUCT                                      
         MVC   DUB(2),PBUYKEST                                                  
         LH    R0,DUB                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVC   PDATE,PBYOMDY                                                    
         TM    PBDSTAT,X'08'       SEE IF HELD BUY                              
         BNO   *+8                                                              
         MVI   PDATE-1,C'H'        PRECEDE WITH 'H'                             
         CLI   PBDBFD,C'T'         SEE IF TEST BUY                              
         BNE   *+8                                                              
         MVI   PDATE-1,C'T'        PRECEDE WITH 'T' (WILL OVERWRITE             
*                                                   "H" IF HELD BUY)            
M345     DS    0H                                                               
         MVC   PSPACE,PBYOSPC1                                                  
         CLI   PBYOSPC,C' '                                                     
         BH    *+10                                                             
         MVC   PSPACE(7),PBYOUNTS                                               
         CLC   PBYOUR,SPACES                                                    
         BNH   *+10                                                             
         MVC   PSPACE+9(8),PBYOUR                                               
*                                                                               
M348     DS    0H                                                               
*                                                                               
         LA    RE,WORK2            PVALUES                                      
         USING PVALD,RE                                                         
         L     RF,GROSS                                                         
         CLI   KBAACT+4,C'N'                                                    
         BNE   *+8                                                              
         S     RF,AGYCOM                                                        
         CLI   KBAACT+5,C'L'                                                    
         BNE   *+8                                                              
         S     RF,CSHDSC                                                        
*                                                                               
         ST    RF,FULL             SAVE FOR TOTALS                              
         EDIT  (RF),(14,PGRS),2,COMMAS=YES,FLOAT=-                              
*                                                                               
M349     DS    0H                                                               
*                                                                               
         BAS   RE,NXTLIN                                                        
         LA    RF,PSPACE-2                                                      
         CLI   PBYOSPC2,C' '                                                    
         BNH   M349B                                                            
         MVC   0(20,RF),PBYOSPC2                                                
         LA    RF,20(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
M349B    DS    0H                                                               
         CLI   PBYOPRM,C' '                                                     
         BNH   *+10                                                             
         MVC   2(11,RF),PBYOPRM                                                 
*                                                                               
         CLI   PBYOMDY2,C' '                                                    
         BNH   M349C                                                            
         MVC   PDATE,PBYOMDY2                                                   
         MVI   PDATE-1,C'+'                                                     
         B     M349D                                                            
*                                                                               
M349C    DS    0H                                                               
         CLC   PBYOISNM,SPACES                                                  
         BE    M349D                                                            
         MVC   PDATE+1(11),PBYOISNM                                             
*                                                                               
M349D    DS    0H                                                               
         OC    PDATE(30),SPACES                                                 
         CLC   PDATE(30),SPACES    TEST ANYTHING TO PRINT                       
         BNH   M450                                                             
         BAS   RE,NXTLIN                                                        
*                                                                               
M450     DS    0H                                                               
         LA    R6,PBYOCOMS                                                      
M450B    DS    0H                                                               
         CLC   0(47,R6),SPACES                                                  
         BE    M450D                                                            
         MVC   PDATE(47),0(R6)                                                  
         BAS   RE,NXTLIN                                                        
         LA    R6,47(R6)                                                        
         B     M450B                                                            
*                                                                               
M450D    DS    0H                                                               
*                                                                               
         DROP  R5                                                               
* ADD BUY TO TOTALS                                                             
M475     LA    RE,WORK2            DSECT FOR PVALUES                            
         USING PVALD,RE                                                         
         LM    R4,R6,CLTTOTS                                                    
         A     R5,FULL                                                          
         CLI   PBDSPACE,C'*'                                                    
         BE    M476                                                             
         LA    R4,1(R4)                                                         
         L     R0,UNITS                                                         
         ST    RE,FULL                                                          
****                                                                            
         GOTO1 VGETEL,DMCB,(X'20',PCONREC),WORK                                 
         CLI   0(R1),X'FF'      IF NO ELEM - NOW ASSUME INCHES                  
         BE    M475A                                                            
         L     RE,WORK                                                          
         CLI   5(RE),C'L'                                                       
         BE    M475B                                                            
*                                                                               
M475A    DS    0H                                                               
         L     RE,FULL                                                          
         CLI   PBDUIND,X'89'                                                    
         BE    M475X                                                            
         CLI   PBDUIND,C'I'                                                     
         BE    RLM2B                                                            
         L     R0,UNITS                                                         
         CVD   R0,DUB                                                           
         MP    DUB,=P'1000'                                                     
         DP    DUB,=P'14'                                                       
         SRP   DUB(6),63,5                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R0,DUB                                                           
         ST    R0,UNITS                                                         
         B     M475X                                                            
RLM2B    L     R0,UNITS                                                         
         CVD   R0,DUB                                                           
         MP    DUB,=P'100'                                                      
         CVB   R0,DUB                                                           
         ST    R0,UNITS                                                         
         B     M475X                                                            
****                                                                            
M475B    L     RE,FULL                                                          
         CLI   PBDUIND,C'I'                                                     
         BE    M475C                                                            
         CLI   PBDUIND,X'89'       SEE IF LOWER CASE I                          
         BNE   M475X                                                            
M475C    MH    R0,=H'14'           14 LINES/INCH                                
         CLI   PBDUIND,X'89'       SEE IF LOWER CASE I                          
         BNE   M475X                                                            
         CVD   R0,DUB              YES - UNITS IN 2 DECIMALS                    
         AP    DUB,=P'50'                                                       
         DP    DUB,=P'100'         ROUND TO NEAREST LINE                        
         ZAP   DUB,DUB(6)                                                       
         CVB   R0,DUB                                                           
M475X    AR    R6,R0                                                            
M476     DS    0H                                                               
         STM   R4,R6,CLTTOTS                                                    
         B     M350                GET NEXT BUY                                 
         EJECT                                                                  
* DISPLAY TOTAL LINE                                                            
         SPACE 2                                                                
M600     DS    0H                                                               
         ST    RE,SAVRE                                                         
         MVC   PCLT-1(18),=C'**PRODUCT TOTALS**'                                
         CLI   SAVPRD,0            SEE IF DOING ONE PRD                         
         BNE   *+10                                                             
         MVC   PCLT-1(9),=C' **CLIENT'                                          
         CLI   0(R1),C'C'                                                       
         BE    M602                                                             
         MVC   PCLT+2(6),=C'AGENCY'                                             
         CLI   0(R1),C'A'                                                       
         BE    M602                                                             
         MVC   PCLT-2(10),=C'**CONTRACT'                                        
M602     DS    0H                                                               
         L     R5,0(R1)                                                         
*                                                                               
         EDIT  (4,4(R5)),(14,PGRS),2,COMMAS=YES,FLOAT=-                         
         MVI   PGRS+14,C'*'                                                     
         CLI   KBAMED,C'N'      SEE IF NEWSPAPERS                               
         BNE   M625                                                             
*                                                                               
         OC    8(4,R5),8(R5)     CHECK FOR ANY DATA                             
         BZ    M625                                                             
*                                                                               
         MVC   PSPACE+5(2),=C'L*'                                               
         GOTO1 VGETEL,DMCB,(X'20',PCONREC),WORK                                 
*                                                                               
         CLI   0(R1),X'FF'         NO ELEM - NOW ASSUME INCHES                  
         BE    M603                                                             
         L     RE,WORK             A(FIRST RATE ELEM)                           
         CLI   5(RE),C'L'                                                       
         BE    M605                                                             
*                                  INCHES                                       
M603     MVC   PSPACE+5(3),=C'IN*'                                              
*****                                                                           
         EDIT  (4,8(R5)),(11,PSPACE-6),2,COMMAS=YES                             
         B     M625                                                             
*****                                                                           
M605     DS    0H                                                               
         EDIT  (4,8(R5)),(8,PSPACE-3),COMMAS=YES                                
*                                                                               
M625     DS    0H                                                               
         EDIT  (4,(R5)),(4,PSPACE+8)                                            
         OI    PSPACE+8+3,C'0'                                                  
         MVC   PSPACE+13(7),=C'INSERTS'                                         
         CH    R0,=H'1'                                                         
         BNE   *+8                                                              
         MVI   PSPACE+19,C' '                                                   
*                                                                               
         BAS   RE,NXTLIN                                                        
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
* DISPLAY SCREEN -FOUT NON-ZERO LINES                                           
M700     LA    R2,MULSEL1H        POINT TO 1ST SEL FIELD                        
         LA    R3,MULSELFH        POINT TO LAST SEL FIELD                       
         SR    R4,R4                                                            
         LA    R5,SBUYTAB         POINT TO TABLE OF BUYS                        
*                                                                               
M725     DS    0H                                                               
         LR    R6,R2                                                            
         SR    R6,RA              PUT DISPLACEMENT TO SEL FIELD IN R6           
         IC    R4,0(R2)                                                         
         AR    R2,R4              NEXT FIELD (MULLIN..)                         
         CR    R2,R3              LAST LINE ?                                   
         BNL   M725M              YES                                           
         OC    8(75,R2),8(R2)     OUTPUT DATA?                                  
         BZ    M725B              NO                                            
         FOUT  (R2)                                                             
*                                                                               
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BNZ   M725B              SWAP TO BUY NOT ALLOWED                       
         CLI   PEST,C'T'          ....(T)OTALS LINE ?                           
         BE    M725B              YES                                           
         CLI   PPRD,C' '          FIRST LINE OF A BUY DISPLAY ?                 
         BH    M725D              YES - SAVE IN SBUYTAB AND UNPROTECT           
*                                                                               
M725B    AR    R6,RA              POINT R6 BACK TO MULSEL.. FIELD               
         OI    1(R6),X'20'        PROTECT AND XMIT FIELD                        
         OI    6(R6),X'80'                                                      
         B     M725H                                                            
*                                                                               
M725D    STH   R6,0(R5)       PUT DISPLACEMENT TO THIS BUY IN SBUYTAB           
         LA    R5,L'SBUYTAB(R5)   BUMP TO NEXT SBUYTAB ENTRY                    
*                                                                               
         AR    R6,RA              POINT R6 BACK TO MULSEL.. FIELD               
         NI    1(R6),X'FF'-X'20'  UNPROTECT FIELD TO ALLOW ENTRY                
         OI    6(R6),X'80'        XMIT FIELD                                    
*                                                                               
M725H    DS    0H                                                               
         IC    R4,0(R2)                                                         
         AR    R2,R4              NEXT FIELD (MULSEL..)                         
         B     M725                                                             
*                                                                               
M725M    DS    0H                                                               
         EDIT  PAGENM,(3,KBAPAG),ALIGN=LEFT   PAGE NUMBER                       
         FOUT  KBAPAGH                                                          
         MVC   KBAMSG(L'KBAMSG),SPACES                                          
         MVC   KBAMSG(14),=C'BUYS DISPLAYED'                                    
         XC    MULLINF(07),MULLINF        CLEAR PF2=BUY                         
         OC    SADVDATA(18),SADVDATA      SEE IF AOR SITUATION                  
         BNZ   M725R              SWAP TO BUY NOT ALLOWED                       
         MVC   MULLINF(07),=C'PF2=BUY'             LAST LINE                    
M725R    MVC   MULLINF+08(13),=C'PF7=UP PF8=DN'                                 
         FOUT  MULLINFH                                                         
         B     M260                EXIT                                         
         SPACE 2                                                                
NXTLIN   DS    0H                                                               
         LA    R3,MULSELFH        POINT TO LAST SEL FIELD                       
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0              POINT TO SEL.. FIELD HEADER                   
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0              POINT TO LIN.. FIELD HEADER                   
         LA    R9,1(R9)           BUMP LINE NO.                                 
*                                                                               
*NOP*    CLI   0(R2),9            DATA LINE ?                                   
*NOP*    BHR   RE                 YES                                           
*                                                                               
         CR    R2,R3              LAST LINE ?                                   
         BLR   RE                 NO                                            
*                                                                               
         SPACE 2                                                                
CHKPAG   DS    0H                                                               
         CLC   PAGENM,RPAGE                                                     
         BNL   M700                                                             
         ST    RE,LOCALRE                                                       
*                              CLEAR TABLE OF KEYS OF BUYS DISPLAYED            
         XCEFL SBUYTAB,378      AND THEIR DISPLACEMENT INTO SCREEN              
         L     RE,LOCALRE                                                       
* NEXT PAGE - CLEAR LINES                                                       
         SR    R4,R4                                                            
         LA    R2,MULSEL1H        POINT TO 1ST SEL FIELD                        
         LA    R3,MULSELFH        POINT TO LAST SEL FIELD                       
CHKPAGD  DS    0H                                                               
         CLI   8(R2),C'S'         SELECTED ?                                    
         BE    CHKPAGH            YES - DO NOT CLEAR                            
         XC    8(2,R2),8(R2)                                                    
         FOUT  (R2)                                                             
CHKPAGH  IC    R4,0(R2)                                                         
         AR    R2,R4              NEXT FIELD (MULLIN..)                         
         CR    R2,R3              LAST LINE ?                                   
         BNL   CHKPAGM                                                          
*                                                                               
         XC    8(75,R2),8(R2)                                                   
         IC    R4,0(R2)                                                         
         AR    R2,R4               NEXT LINE  (MULSEL...)                       
         CLI   0(R2),0             LAST?                                        
         BNE   CHKPAGD                                                          
*                                                                               
CHKPAGM  DS    0H                                                               
         L     R9,PAGENM           PAGE CTR                                     
         LA    R9,1(R9)                                                         
         ST    R9,PAGENM                                                        
*                                                                               
         LA    R9,1                LINE CTR                                     
         LA    R2,MULLIN1H         FIRST LINE                                   
         BR    RE                                                               
*                                                                               
ESAVKEY  DS    CL64                KEY AND KEYSAVE                              
PAGENM   DS    F                   PAGE NUMBER                                  
LOCALRE  DS    F                   ANOTHER REG E SAVE                           
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
         EJECT                                                                  
       ++INCLUDE GETADVCD                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018PPCON30   06/16/10'                                      
         END                                                                    
