*          DATA SET CTSFM22    AT LEVEL 008 AS OF 10/29/15                      
*PHASE TA0A22A                                                                  
***********************************************************************         
*                     (DFORM)                                         *         
*  TITLE: TA0A22 - DEMO FORMULA RECORDS MAINTENANCE / LIST / REPORT   *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMB5 (TA0AB5) -- MAINTENANCE                    *         
*                  CTSFM98 (TA0A98) -- LIST/REPORT                    *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - 2ND PROGRAM BASE                                      *         
*          R8 - WORK (AND SPOOLD IN LISTRECS)                         *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - 1ST PROGRAM BASE                                      *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A22 DEMO FORMULA RECORDS MAINTENANCE /LIST'                  
*****************************************************************               
*NOTE: TO ACCESS THE 4BYTE VS 3BYTE DEMOS USE FE.. AND MA..                     
*   INSTEAD OF W AND M AS THE SEX PREFIX. (SUPPORTED IN DEMOVAL)                
*   E.G. YFE6-14, YFE9-17                                                       
*                                                                               
*****************************************************************               
TA0A22   RSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,TA0A22**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       LIST DIRECT TO PRINT QUEUE                   
         BE    LR                                                               
*                                                                               
         ANSR                                                                   
         EJECT                                                                  
********************************************************************            
*                      VALIDATE KEY                                             
********************************************************************            
*                                                                               
VK       DS    0H                                                               
*                                                                               
         LA    R4,KEY              START BUILDING KEY                           
         USING CTGREC,R4                                                        
         XC    KEY,KEY                                                          
         MVI   CTGKTYP,CTGKTEQU    'G' RECORDS                                  
*                                                                               
*** VALIDATE FILE                                                               
         MVI   SVFILE,0                                                         
         LA    R2,DEMFILEH                                                      
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   WHEN,X'40'          'NOW' REPORT VIA LIST?                       
         JNE   MISSERR             NO                                           
         B     VK20                                                             
*                                                                               
         LARL  RE,FILETAB                                                       
         USING FILETABD,RE                                                      
VK10     CLI   0(RE),0                                                          
         JE    INVERR                                                           
         CLC   DEMFILE,FTKEY                                                    
         BE    *+12                                                             
         LA    RE,FILETABQ(RE)                                                  
         B     VK10                                                             
*                                                                               
         MVC   CTGKFILE,FTKEY                                                   
         MVC   SVFILE,FTKEY                                                     
         MVC   SVDBFILE,FTCODE                                                  
         DROP  RE                                                               
*                                                                               
*** VALIDATE SUB-MEDIA                                                          
VK20     DS    0H                                                               
         MVI   SVSUBMED,0                                                       
         LA    R2,DEMSUBFH                                                      
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   WHEN,X'40'          'NOW' REPORT VIA LIST?                       
         JNE   MISSERR             NO                                           
         B     VK40                                                             
*                                                                               
         LARL  RE,MEDTAB                                                        
VK30     CLI   0(RE),0                                                          
         JE    INVERR                                                           
         CLC   DEMSUBF,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,MEDTABLQ(RE)                                                  
         B     VK30                                                             
*                                                                               
         MVC   CTGKMED,DEMSUBF                                                  
         MVC   SVSUBMED,DEMSUBF                                                 
*                                                                               
*** VALIDATE SOURCE                                                             
VK40     DS    0H                                                               
         MVI   SVSOURCE,0                                                       
         LA    R2,DEMSRCH                                                       
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   WHEN,X'40'          'NOW' REPORT VIA LIST?                       
         JNE   MISSERR             NO                                           
         B     VK70                                                             
*                                                                               
         LARL  RE,SRCTAB                                                        
VK50     CLI   0(RE),0                                                          
         JE    INVERR                                                           
         CLC   DEMSRC,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,SRCTABLQ(RE)                                                  
         B     VK50                                                             
*                                                                               
         MVC   CTGKSRC,DEMSRC                                                   
         MVC   SVSOURCE,DEMSRC                                                  
*                                                                               
*** ENSURE FILE/MEDIA/SOURCE COMBINATION IS VALID                               
         CLI   WHEN,X'40'          'NOW' REPORT VIA LIST?                       
         BE    VK70                YES                                          
*                                                                               
         LA    R2,DEMFILEH         FOR CURSOR POSITIONING ON ERROR              
         LARL  RE,FMSTAB                                                        
VK60     CLI   0(RE),0                                                          
         JE    INVERR                                                           
         CLC   CTGKFMS,0(RE)       FILE/MEDIA/SOURCE                            
         BE    *+12                                                             
         LA    RE,FMSTABLQ(RE)                                                  
         B     VK60                                                             
*                                                                               
VK70     DS    0H                                                               
*                                                                               
*** VALIDATE START BOOK (MMM/YY)                                                
*                                                                               
         XC    SVSTRTBK,SVSTRTBK   ASSUME NO START BOOK LIST FILTER             
         LA    R2,DEMSTRTH                                                      
         CLI   5(R2),0                                                          
         BNE   VK80                                                             
         CLI   ACTNUM,ACTLIST      BOOK FILTER IS OPTIONAL ON LIST              
         BE    VK110                                                            
         CLI   ACTNUM,ACTDIS       DISPLAY GETS LATEST BOOK BY DEFAULT          
         BE    VK110                                                            
         J     MISSERR                                                          
*                                                                               
VK80     DS    0H                                                               
         CLI   CTGKMED,C'N'        NETWORK                                      
         BE    VK90                                                             
         CLI   CTGKMED,C'W'        WEEKLY                                       
         BE    VK90                                                             
         CLI   CTGKMED,C'I'        EIN (ESTIMATED IMPS)                         
         BE    VK90                                                             
         CLI   CTGKMED,C'V'        EVN (ESTIMATED VPH)                          
         BE    VK90                                                             
         CLI   CTGKFILE,C'C'       CABLE HISPANIC IS A WEEKLY FILE              
         BNE   *+12                                                             
         CLI   CTGKMED,C'H'                                                     
         BE    VK90                                                             
         GOTO1 DATVAL,DMCB,(2,DEMSTRT),WORK   VALIDATE MMM/YY                   
         OC    0(4,R1),0(R1)       TEST IF DATE VALID                           
         JZ    INVERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,FULL),0                                  
         MVC   CTGKSTRT,FULL       BINARY Y/M                                   
         B     VK100                                                            
*                                  VALIDATE NETWORK BOOK                        
VK90     TM    4(R2),X'08'         TEST BOOK NUMERIC                            
         JZ    INVERR                                                           
         CLI   5(R2),4             AND 4 BYTES LONG                             
         JNE   INVERR                                                           
         MVC   DUB(2),DEMSTRT      YY (YEAR)                                    
         MVC   DUB+2(4),=C'0101'   ANY MONTH/DAY WILL FOOL DATCON               
         GOTO1 DATCON,DMCB,(0,DUB),(3,FULL)                                     
         MVC   CTGKSTRT(1),FULL    SET YEAR                                     
         PACK  DUB,DEMSTRT+2(2)    GET NETWORK WEEK                             
         CVB   R0,DUB                                                           
         LTR   R0,R0               TEST 1 THRU 53                               
         JZ    INVERR                                                           
         CHI   R0,53               HIGHEST ALLOWABLE WEEK                       
         JH    INVERR                                                           
         STC   R0,CTGKSTRT+1       SET WEEK                                     
*                                                                               
VK100    DS    0H                                                               
         MVC   SVSTRTBK,CTGKSTRT   SAVE LIST FILTER                             
         XC    CTGKSTRT,=X'FFFF'   FLIP BITS IN BOOK                            
*                                                                               
VK110    DS    0H                                                               
         MVC   CTGKAGY,=X'FFFF'                                                 
         MVI   CTGKCODE,X'FF'                                                   
*                                                                               
*** VALIDATE DEMO CODE                                                          
         MVI   SVDEMMOD,0          DEFAULT TO NO MODIFIER LIST FILTER           
         XC    SVDEMO2,SVDEMO2     SAVE DEMO MODIFIER/CATEGORY                  
         LA    R2,DEMDEMOH                                                      
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK160                                                            
         J     MISSERR                                                          
*                                                                               
         CLI   DEMDEMO,C'Z'        1ST CHAR CAN'T BE > C'Z'                     
         JH    INVERR                                                           
         CLI   DEMDEMO,C'A'        IS IT AN ALPHA CHARACTER?                    
         BL    *+18                NO: COULD BE PERSONAL LANGUAGE               
         MVC   CTGKDEMO(1),DEMDEMO YES: THIS IS THE MODIFIER                    
         LA    R3,DEMDEMO          R3 = A(ALPHA MODIFIER)                       
         B     VK120                                                            
*                                                                               
         LA    R1,DEMDEMO          VALIDATE FOR PERSONAL LANGUAGE               
         BRAS  RE,PLENCODE                                                      
         JNE   INVERR                                                           
         MVC   CTGKDEMO(1),BYTE    SAVE THE ENCODED MODIFIER                    
         LA    R3,DEMDEMO+1        R3 = A(ALPHA MODIFIER)                       
*                                                                               
VK120    DS    0H                                                               
         CLI   1(R3),C' '          DOES ANOTHER CHARACTER FOLLOW?               
         BH    *+16                YES: VALIDATE REMAINDER OF FIELD             
         CLI   ACTNUM,ACTLIST      NO: IS THIS A LIST FILTER?                   
         BE    VK140               YES: IT'S A MODIFIER FILTER                  
         J     INVERR              NO: MISSING DEMO CATEGORY                    
*                                                                               
         CLI   1(R3),C'0'          NEXT CHAR. CAN BE ALPHA OR NUMERIC           
         BNL   VK130               IT'S NUMERIC                                 
*                                                                               
*                                  REMAINING CHARS MUST BE ALPHA                
         L     RE,ACOMFACS         GET A(DEMOVAL)                               
         ICM   RF,15,CDEMOVAL-COMFACSD(RE)                                      
         XC    BLOCK(256),BLOCK    FIELD 'BLOCK' USED FOR DBLOCK                
         USING DEDBLOCK,R1                                                      
         LA    R1,BLOCK                                                         
         MVC   DBFILE,SVDBFILE     FILE                                         
         MVC   DBSELMED,CTGKMED    MEDIA                                        
         MVC   DBSELSRC,CTGKSRC    SOURCE                                       
         MVI   DBDEMTYP,C'4'       SET FOR EXPANDED DEMOS                       
         ST    RE,DBCOMFCS         A(COMFACS)                                   
         DROP  R1                                                               
*                                                                               
         MVC   BYTE,0(R3)          SAVE ORIGINAL MODIFIER                       
         MVI   0(R3),C'R'          PASS MODIFIER 'R' TO DEMOVAL                 
         GOTO1 (RF),DMCB,(1,DEMDEMOH),(1,WORK),(0,BLOCK)                        
         MVC   0(1,R3),BYTE        RESTORE ORIGINAL MODIFIER                    
         CLI   4(R1),0             ERROR                                        
         JE    INVERR                                                           
*                                  CONVERT SEX 0 AND 1 TO 1-256                 
         CLI   WORK+2,1                                                         
         BNE   *+12                                                             
         OI    WORK+3,X'80'                                                     
         MVI   WORK+2,0                                                         
*                                                                               
         CLI   WORK+2,0                                                         
         BE    *+10                                                             
         MVC   CTGKCODE,WORK+2     LOOKUP CODE                                  
         MVC   CTGKDEMO+1(1),WORK+3                                             
         B     VK140                                                            
*                                                                               
VK130    DS    0H                  VALIDATE NUMERIC CATEGORY                    
         LA    RF,1(R3)            RF = A(FIRST DIGIT)                          
         LA    R1,DEMDEMO                                                       
         SR    RF,R1               RF = DISPLACEMENT TO FIRST DIGIT             
         LLC   R1,5(R2)                                                         
         SR    R1,RF               R1 = NUMBER OF DIGITS                        
         CHI   R1,3                TOO MANY DIGITS?                             
         JH    INVERR              YES                                          
*                                                                               
         BCTR  R1,0                FOR EX INSTRUCTION                           
         MVC   DUB,=8C'0'                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),1(R3)                                                     
         CLC   DUB,=8C'0'                                                       
         JNE   INVERR                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R3)                                                      
         CVB   R1,DUB              VALUE MUST BE BETWEEN 0-255                  
         ST    R1,DUB                                                           
         OC    DUB(3),DUB                                                       
         JNZ   INVERR                                                           
         MVC   CTGKDEMO+1(1),DUB+3                                              
*                                                                               
VK140    DS    0H                                                               
         MVC   SVDEMO2,CTGKDEMO    SAVE MODIFIER/CATEGORY                       
         MVC   SVDEMMOD,CTGKDEMO   EXTRACT ALPHA MODIFIER...                    
         L     RE,APLMODTB                                                      
         TR    SVDEMMOD,0(RE)      ...AND SAVE FOR LIST FILTER                  
*                                                                               
         CLI   ACTNUM,ACTLIST      DON'T REFORMAT THE DEMO ON A LIST            
         BE    VK160                                                            
*                                                                               
* THERE IS AN INTERNAL DISCREPANCY SOMEWHERE BETWEEN DEMOVAL AND                
* DEMOCON. IN THEORY, IT SHOULD ALWAYS BE SAFE TO REDISPLAY THE DEMO,           
* BUT WE'VE FOUND THAT ON OCCASION, THE REDISPLAYED DEMO DOESN'T                
* REVALIDATE CORRECTLY. UNTIL WE FIGURE OUT WHAT'S GOING ON, WE HAVE            
* HARD-CODE TO PREVENT THE REDISPLAY.                                           
*                                                                               
         CLC   =C'V2+',DEMDEMO+1   DON'T CHANGE TO "TOTAL"                      
         BE    VK150                                                            
*                                                                               
         CLI   CTGKDEMO+1,0        MACRO FORMULA?                               
         BE    VK150               YES                                          
         MVC   HALF,CTGKDEMO                                                    
         BRAS  RE,DEMODSP6         REDISPLAY THE DEMO VIA DEMOCON               
         MVC   DEMDEMO,DUB                                                      
         OI    DEMDEMOH+6,X'80'    XMIT                                         
*                                                                               
VK150    DS    0H                                                               
         OI    DEMDINTH+6,X'80'    XMIT DEMO ENTRY FIELD                        
         XC    DEMDINT,DEMDINT     ASSUME THERE IS NO PERSONAL LANGUAGE         
*                                  DISPLAY DECODED MODIFIER (AND PLD)           
         GOTO1 VDEMOCON,DMCB,(0,CTGKDEMO),('DEMOCON_15',DEMDINT),0              
         LLC   RF,DMCB             NUMBER OF RETURNED CHARACTERS                
         LA    R3,DEMDINT(RF)      A(NEXT OUTPUT POSITION)                      
         EDIT  (B1,CTGKDEMO+1),(3,(R3)),ALIGN=LEFT,ZERO=NOBLANK                 
*                                                                               
         MVI   DEMLCOD,C'('                                                     
         GOTO1 HEXOUT,DMCB,CTGKCODE,DEMLCOD+1,1,=C'TOG'                         
         CLC   =F'2',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DEMLCOD+3,C')'                                                   
         OI    DEMLCODH+6,X'80'    XMIT                                         
*                                                                               
VK160    DS    0H                                                               
         CLI   ACTNUM,ACTDIS       DISPLAY GETS LATEST BOOK BY DEFAULT          
         BNE   VKX                                                              
*                                                                               
         MVC   SAVEKEY,KEY         SAVE THE KEY WE BUILT                        
*                                                                               
         XC    CTGKDEMO,CTGKDEMO   CLEAR KEY BELOW START BOOK                   
*                                                                               
VK170    DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(CTGKSTRT-CTGKEY),SAVEKEY  KEY MATCHES PRIOR TO BOOK?         
         BNE   VK200               NO: LET GENCON READ AND GIVE ERROR           
*                                                                               
         CLC   CTGKDEMO,SVDEMO2    YES: DEMO MATCHES ALSO?                      
         BE    VK180               YES: THIS IS LATEST BOOK FOR DEMO            
         BH    *+14                                                             
         MVC   CTGKDEMO,SVDEMO2    NO, TOO LOW. RE-READ FOR THIS...             
         B     VK170               ...DEMO WITH SAME BOOK                       
         MVC   CTGKDEMO,=X'FFFF'   NO, TOO HIGH. FORCE NEXT BOOK...             
         B     VK170               ...AND START AGAIN                           
*                                                                               
VK180    DS    0H                                                               
         OI    DEMSTRTH+6,X'80'    XMIT                                         
         MVC   FULL(2),CTGKSTRT    START BOOK                                   
         XC    FULL(2),=X'FFFF'    REVERSE THE COMPLEMENT                       
         MVI   FULL+2,X'01'        ANY DAY NUMBER WILL FAKE OUT DATCON          
         CLI   CTGKMED,C'N'        NETWORK                                      
         BE    VK190                                                            
         CLI   CTGKMED,C'W'        WEEKLY                                       
         BE    VK190                                                            
         CLI   CTGKMED,C'I'        EIN (ESTIMATED IMPS)                         
         BE    VK190                                                            
         CLI   CTGKMED,C'V'        EVN (ESTIMATED VPH)                          
         BE    VK190                                                            
         CLI   CTGKFILE,C'C'       CABLE HISPANIC IS A WEEKLY FILE              
         BNE   *+12                                                             
         CLI   CTGKMED,C'H'                                                     
         BE    VK190                                                            
         GOTO1 DATCON,DMCB,(3,FULL),(6,DEMSTRT)  BINARY TO MMM/YY               
         B     VKX                                                              
*                                                                               
VK190    DS    0H                  UNPACK WEEKLY YYMM                           
         GOTO1 DATCON,DMCB,(3,FULL),(X'20',DUB)  GET 'NOT FUNNY' YEAR           
         MVC   DEMSTRT(2),DUB      YY                                           
         LLC   R0,FULL+1           BINARY WEEK NUMBER                           
         CVD   R0,DUB                                                           
         UNPK  DEMSTRT+2(2),DUB                                                 
         OI    DEMSTRT+3,X'F0'                                                  
         B     VKX                                                              
*                                                                               
VK200    DS    0H                                                               
         MVC   KEY,SAVEKEY         RESTORE KEY FOR GENCON READ                  
*                                                                               
VKX      DS    0H                                                               
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*****************************************************************               
*              VALIDATE RECORD                                                  
*****************************************************************               
VR       DS    0H                                                               
*                                                                               
         L     R6,AIO              START BUILDING RECORD                        
         USING CTGREC,R6                                                        
*                                                                               
*** OLD-STYLE ACTIVITY ELEMENT (FOR TRANSPARENCY WITH RECORDS CREATED           
*** VIA =FIL)                                                                   
         MVI   ELCODE,CTACTELQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,ELEM                                                          
         USING CTACTD,R4                                                        
         XC    ELEM,ELEM                                                        
         MVI   CTACTEL,CTACTELQ                                                 
         MVI   CTACTLEN,CTACTLNQ                                                
         GOTO1 DATCON,DMCB,(5,0),(3,CTACTDT)                                    
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
*** BUILD DESCRIPTION ELEMENT                                                   
         MVI   ELCODE,CTDSCELQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,DEMDESCH         DESCRIPTION FIELD                            
         CLI   5(R2),0                                                          
         BE    VR10                                                             
         LA    R4,ELEM                                                          
         USING CTDSCD,R4                                                        
         XC    ELEM,ELEM                                                        
         MVI   CTDSCEL,CTDSCELQ                                                 
         LLC   R1,5(R2)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTDSC(0),DEMDESC                                                 
         LA    R1,CTDSC-CTDSCD(R1)                                              
         STC   R1,CTDSCLEN                                                      
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
*** BUILD FIELD PRECISION ELEMENT                                               
VR10     DS    0H                                                               
         MVI   ELCODE,CTPRECDQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,DEMPRECH                                                      
         GOTO1 ANY                                                              
         LA    R4,ELEM                                                          
         USING CTPRECSD,R4                                                      
         XC    ELEM,ELEM                                                        
         MVI   CTPRECDE,CTPRECDQ   ELEMENT CODE (X'03)                          
         MVI   CTPRELEN,CTPRELNQ   ELEMENT LENGTH                               
*                                                                               
         CLC   =C'USE',WORK        VALIDATE FOR USE=MODIFIER                    
         BNE   VR40                                                             
         CLI   5(R2),5             USE=<MODIFIER> ?                             
         BE    VR20                YES                                          
         CLI   5(R2),6             USE=<PLD><MODIFIER> ?                        
         BNE   VR40                NO                                           
*                                                                               
         LA    R1,WORK+4           A(ALLEGED PLD CHARACTER)                     
         BRAS  RE,PLENCODE                                                      
         JNE   INVERR              INVALID MODIFIER WITH PLD                    
         MVC   WORK+4(1),BYTE      REPLACE EBCDIC WITH ENCODED MODIFIER         
         B     VR30                                                             
*                                                                               
VR20     DS    0H                                                               
         TM    4(R2),X'40'         MODIFIER MUST BE VALID ALPHA                 
         JZ    INVERR                                                           
*                                                                               
VR30     DS    0H                                                               
         MVC   CTPRECCD,WORK+4                                                  
         MVI   DIROPT,CTPRECFL_EQUATED_FORMULA                                  
         B     VR90                                                             
*                                                                               
VR40     LLC   R1,5(R2)            LOOK-UP PRECISION IN TABLE                   
         BCTR  R1,0                                                             
         LARL  RE,PRECTAB                                                       
         USING PRECTABD,RE                                                      
VR50     CLI   0(RE),0                                                          
         JE    INVERR                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PRECDESC(0),WORK                                                 
         BE    *+12                                                             
         LA    RE,PRECTABQ(RE)                                                  
         B     VR50                                                             
*                                                                               
         CLC   DEMPREC(PRECTABQ-1),PRECDESC                                     
         BE    *+14                                                             
         MVC   DEMPREC(PRECTABQ-1),PRECDESC                                     
         OI    DEMPRECH+6,X'80'                                                 
*                                                                               
         MVC   CTPRECCD,PRECCODE                                                
         MVC   PRECISN,PRECCODE                                                 
         DROP  RE                                                               
*                                  VALIDATE DIRECT XFER OPTION                  
         LA    R2,DEMDIRH                                                       
         CLI   5(R2),0             DEFAULT THE FIELD TO 'YES'                   
         BNE   *+18                                                             
         MVC   DEMDIR,=C'YES'                                                   
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         GOTO1 ANY                                                              
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVI   DIROPT,0                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=C'YES'     OPTIONS ARE YES/NO                           
         BE    VR60                                                             
         OI    DIROPT,CTPRECFL_NO_DIRECT_TRANSFER                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=C'NO '                                                  
         JNE   INVERR                                                           
         TM    PRECISN,CTPRECFL_EQUATED_FORMULA                                 
         JNZ   INVERR                                                           
*                                  VALIDATE INDEX DEMO OPTION                   
VR60     LA    R2,DEMNDXH                                                       
         CLI   5(R2),0             DEFAULT THE FIELD TO 'NO'                    
         BNE   *+18                                                             
         MVC   DEMNDX,=C'NO '                                                   
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         GOTO1 ANY                                                              
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=C'NO '     OPTIONS ARE YES/NO                           
         BE    VR70                                                             
         OI    DIROPT,CTPRECFL_INDEXED_DEMO                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=C'YES'                                                  
         JNE   INVERR                                                           
*                                  VALDATE OPTIONS                              
VR70     LA    R2,DEMOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VR90                                                             
         LARL  RE,OPTMACS                                                       
         CLI   CTGKDEMO+1,0        TEST MACRO DEMO                              
         BE    *+10                                                             
         LARL  RE,OPTDEMS                                                       
         USING OPTTABD,RE                                                       
*                                                                               
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
VR80     CLI   0(RE),X'FF'         TEST E-O-T                                   
         JE    INVERR                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DEMOPT(0),OPTDESC                                                
         BE    *+12                                                             
         LA    RE,OPTTABDQ(RE)                                                  
         B     VR80                                                             
*                                                                               
         XC    DEMOPT,DEMOPT                                                    
         MVC   DEMOPT(OPTTABDQ-1),OPTDESC                                       
         OI    DEMOPTH+6,X'80'                                                  
         OC    DIROPT,OPTFLAGS                                                  
         DROP  RE                                                               
*                                                                               
VR90     MVC   CTPRECFL,DIROPT                                                  
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
* BUILD POLISH FORMULA ELEMENT                                                  
         MVI   ELCODE,CTDPFCDQ                                                  
         GOTO1 REMELEM                                                          
         MVI   ELCODE,CTDINCDQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,DEMFORMH                                                      
         CLI   5(R2),0             FORMULA WAS INPUT?                           
         BNE   *+16                                                             
         TM    DIROPT,CTPRECFL_NO_DIRECT_TRANSFER                               
         JO    INVERR              FORMULA MUST BE INPUT                        
         B     VRX                                                              
*                                  FORMULA VALIDATION                           
         TM    DIROPT,CTPRECFL_EQUATED_FORMULA  EQUATED MODIFIER?               
         JO    INVERR              YES - FORMULA IS NOT ALLOWED                 
         GOTOR VALFORM             BUILD POLISH FORMULA ELEM. IN 'ELEM'         
         CLI   BYTE,X'FF'          VALID?                                       
         BE    VR100               YES                                          
         GOTO1 ERREX2              ERROR MESSAGE IS ALREADY SET                 
*                                                                               
VR100    DS    0H                                                               
         GOTO1 ADDELEM             ADD POLISH FORMULA ELEMENT                   
*                                                                               
* BUILD INPUT FORMULA ELEMENTS                                                  
         LA    R2,DEMFORMH         R2=A(FIRST INPUT FIELD)                      
         SR    R5,R5               R5=INPUT LINE NUMBER                         
         LA    R4,ELEM                                                          
         USING CTDINCD,R4                                                       
*                                                                               
VR110    SR    R1,R1                                                            
         ICM   R1,1,5(R2)          R1=INPUT FIELD LENGTH                        
         BZ    VR120                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTDINIF(0),8(R2)                                                 
         AHI   R5,1                                                             
         STC   R5,CTDINCS#         SET LINE NUMBER                              
         MVI   CTDINCDE,CTDINCDQ   ELEMENT CODE                                 
         LA    R1,CTDINCFQ(R1)                                                  
         STC   R1,CTDINLEN         ELEMENT LENGTH                               
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
VR120    LLC   R0,0(R2)            BUMP TO NEXT TWA FIELD                       
         AR    R2,R0                                                            
         LA    R0,DEMFRMXH                                                      
         CR    R2,R0               ANY MORE DEMO FORMULA FIELDS?                
         BNH   VR110               YES                                          
*                                                                               
VRX      DS    0H                  FALL THROUGH TO DISPREC LOGIC                
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*                    DISPLAY RECORD                                             
****************************************************************                
*                                                                               
DR       DS    0H                                                               
         GOTOR CLRSCRN                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTDSCELQ     X'02' (DESCRIPTION) ELEMENT                  
         BRAS  RE,GETEL                                                         
         BNE   DR10                                                             
*                                                                               
         USING CTDSCD,R6                                                        
         LLC   R1,CTDSCLEN                                                      
         SHI   R1,3                OVERHEAD LENGTH + 1 FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DEMDESC(0),CTDSC    DESCRIPTION                                  
         DROP  R6                                                               
*                                                                               
DR10     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTPRECDQ     X'03' (PRECISION) ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   DR20                                                             
*                                                                               
         USING CTPRECSD,R6                                                      
         LARL  R1,PRECTAB          R1=A(PRECISION TABLE)                        
         USING PRECTABD,R1                                                      
         TM    CTPRECFL,CTPRECFL_EQUATED_FORMULA                                
         BZ    DR20                                                             
*                                                                               
         MVC   DEMPREC(6),=C'USE=  ' YES - OUTPUT USE=MODIFIER                  
*                                  DISPLAY DECODED MODIFIER (AND PLD)           
         GOTO1 VDEMOCON,DMCB,(0,CTPRECCD),('DEMOCON_15',DEMPREC+4),0            
         B     DR50                                                             
*                                                                               
DR20     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PRECCODE,CTPRECCD                                                
         BE    *+12                                                             
         LA    R1,PRECTABQ(R1)                                                  
         B     DR20                                                             
*                                                                               
         MVC   DEMPREC(PRECTABQ-1),PRECDESC                                     
         DROP  R1                                                               
*                                  DISPLAY DIRECT XFER OPTION                   
         MVC   DEMDIR,=C'YES'                                                   
         TM    CTPRECFL,CTPRECFL_NO_DIRECT_TRANSFER                             
         BZ    *+10                                                             
         MVC   DEMDIR,=C'NO '                                                   
         MVC   DEMNDX,=C'NO '                                                   
         TM    CTPRECFL,CTPRECFL_INDEXED_DEMO                                   
         BZ    *+10                                                             
         MVC   DEMNDX,=C'YES'                                                   
*                                  DISPLAY OPTIONS                              
         LARL  RE,OPTMACS                                                       
         L     R1,AIO                                                           
         CLI   (CTGKDEMO+1)-CTGKEY(R1),0                                        
         BE    *+10                                                             
         LARL  RE,OPTDEMS                                                       
         USING OPTTABD,RE                                                       
DR30     CLI   0(RE),X'FF'                                                      
         BE    DR50                                                             
         CLI   OPTFLAGS,0                                                       
         BE    DR40                                                             
         LLC   R1,OPTFLAGS                                                      
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    CTPRECFL,0          *EXECUTED*                                   
         BNZ   DR40                                                             
         LA    RE,OPTTABDQ(RE)                                                  
         B     DR30                                                             
*                                                                               
DR40     MVC   DEMOPT(OPTTABDQ-1),OPTDESC                                       
         DROP  RE                                                               
         DROP  R6                                                               
*                                                                               
DR50     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTDINCDQ     X'05' (INPUT FORMULA) ELEMENT                
         BRAS  RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         USING CTDINCD,R6                                                       
         LA    R2,DEMFORMH         R1=A(FIRST TWA LINE)                         
*                                                                               
DR60     DS    0H                                                               
         LLC   RF,CTDINLEN                                                      
         SHI   RF,CTDINCFQ                                                      
         STC   RF,5(R2)            STORE LENGTH IN HEADER                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),CTDINIF                                                  
*                                                                               
         BRAS  RE,NEXTEL           ANY MORE DEMO FORMULA ELEMENTS?              
         BNE   DR70                                                             
         LLC   R0,0(R2)            YES: DISPLAY IT                              
         AR    R2,R0                                                            
         B     DR60                                                             
*                                                                               
DR70     DS    0H                                                               
         GOTOR DISFORMA            DISPLAY ALPHA FORMULA                        
         DROP  R6                                                               
*                                                                               
DRX      DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
****************************************************************                
*                    DISPLAY KEY                                                
****************************************************************                
*                                                                               
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING CTGKEY,R6                                                        
*                                                                               
         MVC   DEMFILE,CTGKFILE    FILE                                         
         OI    DEMFILEH+6,X'80'                                                 
*                                                                               
         MVC   DEMSUBF,CTGKMED     SUB-FILE                                     
         OI    DEMSUBFH+6,X'80'                                                 
*                                                                               
         MVC   DEMSRC,CTGKSRC      SOURCE                                       
         OI    DEMSRCH+6,X'80'                                                  
*                                                                               
         OI    DEMSTRTH+6,X'80'    XMIT                                         
         MVC   FULL(2),CTGKSTRT    START BOOK                                   
         XC    FULL(2),=X'FFFF'    REVERSE THE COMPLEMENT                       
         MVI   FULL+2,X'01'        ANY DAY NUMBER WILL FAKE OUT DATCON          
         CLI   CTGKMED,C'N'        NETWORK                                      
         BE    DK20                                                             
         CLI   CTGKMED,C'W'        WEEKLY                                       
         BE    DK20                                                             
         CLI   CTGKMED,C'I'        EIN (ESTIMATED IMPS)                         
         BE    DK20                                                             
         CLI   CTGKMED,C'V'        EVN (ESTIMATED VPH)                          
         BE    DK20                                                             
         CLI   CTGKFILE,C'C'       CABLE HISPANIC IS A WEEKLY FILE              
         BNE   *+12                                                             
         CLI   CTGKMED,C'H'                                                     
         BE    DK20                                                             
         GOTO1 DATCON,DMCB,(3,FULL),(6,DEMSTRT)  BINARY TO MMM/YY               
         B     DK30                                                             
*                                                                               
DK20     DS    0H                  UNPACK WEEKLY YYMM                           
         GOTO1 DATCON,DMCB,(3,FULL),(X'20',DUB)  GET 'NOT FUNNY' YEAR           
         MVC   DEMSTRT(2),DUB      YY                                           
         LLC   R0,FULL+1           BINARY WEEK NUMBER                           
         CVD   R0,DUB                                                           
         UNPK  DEMSTRT+2(2),DUB                                                 
         OI    DEMSTRT+3,X'F0'                                                  
*                                                                               
DK30     DS    0H                                                               
         XC    DEMDINT,DEMDINT     CLEAR INTERNAL-DISPLAY FIELD                 
*                                  DISPLAY DECODED MODIFIER (AND PLD)           
         GOTO1 VDEMOCON,DMCB,(0,CTGKDEMO),('DEMOCON_15',DEMDINT),0              
         LLC   RF,DMCB             NUMBER OF RETURNED CHARACTERS                
         LA    R3,DEMDINT(RF)      A(NEXT OUTPUT POSITION)                      
         EDIT  (B1,CTGKDEMO+1),(3,(R3)),ALIGN=LEFT,ZERO=NOBLANK                 
         OI    DEMDINTH+6,X'80'    XMIT                                         
*                                                                               
         OI    DEMDEMOH+6,X'80'    XMIT                                         
         XC    DEMDEMO,DEMDEMO                                                  
         CLI   CTGKDEMO+1,0        MACRO FORMULA?                               
         BNE   *+14                NO                                           
         MVC   DEMDEMO(L'DEMDINT),DEMDINT  DISPLAY INTERNAL FORMAT              
         B     DK40                                                             
*                                                                               
         LR    R4,R6               R4 = A(KEY)                                  
         MVC   HALF,CTGKDEMO                                                    
         BRAS  RE,DEMODSP6         DISPLAY THE DEMO VIA DEMOCON                 
         MVC   DEMDEMO,DUB                                                      
*                                                                               
DK40     DS    0H                                                               
         MVI   DEMLCOD,C'('                                                     
         GOTO1 HEXOUT,DMCB,CTGKCODE,DEMLCOD+1,1,=C'TOG'                         
         CLC   =F'2',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DEMLCOD+3,C')'                                                   
         OI    DEMLCODH+6,X'80'    XMIT                                         
*                                                                               
         J     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*            LIST RECORD                                                        
***********************************************************************         
*                                                                               
LR       DS    0H                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R4,KEY                                                           
         USING CTGKEY,R4                                                        
         OC    KEY,KEY             DISPLAY FROM SELECT?                         
         BNZ   LR10                READ RECORD                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVI   CTGKTYP,CTGKTEQU    'G' (DFORM RECORDS)                          
         MVC   CTGKFILE,SVFILE                                                  
         MVC   CTGKMED,SVSUBMED                                                 
         MVC   CTGKSRC,SVSOURCE                                                 
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS    0H                                                               
         CLI   CTGKTYP,CTGKTEQU    DEMO FORMULA RECORD?                         
         BNE   LRX                 NO: WE'RE DONE                               
         OC    CTGKTYP+1(14),CTGKTYP+1  NEXT 14 BYTES MUST BE NULLS             
         BNZ   LRX                 (WHAT ARE THESE RECORDS?)                    
*                                                                               
         CLI   SVFILE,0            ANY FILE FILTER?                             
         BE    *+14                NO                                           
         CLC   SVFILE,CTGKFILE     YES: MATCH ON FILE?                          
         BNE   LR130               NO: SKIP THIS RECORD                         
*                                                                               
         CLI   SVSUBMED,0          ANY SUBMEDIA FILTER?                         
         BE    *+14                NO                                           
         CLC   SVSUBMED,CTGKMED    YES: MATCH ON SUBMEDIA?                      
         BNE   LR130               NO: SKIP THIS RECORD                         
*                                                                               
         CLI   SVSOURCE,0          ANY SOURCE FILTER?                           
         BE    *+14                NO                                           
         CLC   SVSOURCE,CTGKSRC    YES: MATCH ON SOURCE?                        
         BNE   LR130               NO: SKIP THIS RECORD                         
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST DISPLAY                           
*                                                                               
         GOTO1 HEXOUT,DMCB,CTGKCODE,LSTLCODE,1,=C'TOG'                          
         CLC   =F'2',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FULL(2),CTGKSTRT    START BOOK                                   
         XC    FULL(2),=X'FFFF'    REVERSE THE COMPLEMENT                       
         OC    SVSTRTBK,SVSTRTBK   ANY START BOOK LIST FILTER?                  
         BZ    *+14                                                             
         CLC   SVSTRTBK,FULL       YES: MATCH ON FILTER?                        
         BNE   LR130               NO: SKIP THIS RECORD                         
*                                                                               
         MVI   FULL+2,X'01'        ANY DAY NUMBER WILL FAKE OUT DATCON          
         CLI   CTGKMED,C'N'        NETWORK                                      
         BE    LR30                                                             
         CLI   CTGKMED,C'W'        WEEKLY                                       
         BE    LR30                                                             
         CLI   CTGKMED,C'I'        EIN (ESTIMATED IMPS)                         
         BE    LR30                                                             
         CLI   CTGKMED,C'V'        EVN (ESTIMATED VPH)                          
         BE    LR30                                                             
         CLI   CTGKFILE,C'C'       CABLE HISPANIC IS A WEEKLY FILE              
         BNE   *+12                                                             
         CLI   CTGKMED,C'H'                                                     
         BE    LR30                                                             
         GOTO1 DATCON,DMCB,(3,FULL),(6,LSTBOOK)  BINARY TO MMM/YY               
         B     LR40                                                             
*                                                                               
LR30     DS    0H                  UNPACK WEEKLY YYMM                           
         GOTO1 DATCON,DMCB,(3,FULL),(X'20',DUB)  GET 'NOT FUNNY' YEAR           
         MVC   LSTBOOK(2),DUB      YY                                           
         LLC   R0,FULL+1           BINARY WEEK NUMBER                           
         CVD   R0,DUB                                                           
         UNPK  LSTBOOK+2(2),DUB                                                 
         OI    LSTBOOK+3,X'F0'                                                  
*                                                                               
LR40     DS    0H                                                               
         CLI   SVDEMMOD,0          ANY DEMO MODIFIER FILTER?                    
         BE    LR60                NO                                           
         LLC   RE,CTGKDEMO         YES: EXTRACT EBCDIC MODIFIER                 
         A     RE,APLMODTB                                                      
         CLC   SVDEMMOD,0(RE)      MATCH ON DEMO MODIFIER FILTER?               
         BNE   LR130               NO: SKIP THIS RECORD                         
*                                                                               
LR60     DS    0H                                                               
*                                  DISPLAY DECODED MODIFIER (AND PLD)           
         GOTO1 VDEMOCON,DMCB,(0,CTGKDEMO),('DEMOCON_15',LSTDINT),0              
         LLC   RF,DMCB             NUMBER OF RETURNED CHARACTERS                
         LA    R3,LSTDINT(RF)      A(NEXT OUTPUT POSITION)                      
         EDIT  (B1,CTGKDEMO+1),(3,(R3)),ALIGN=LEFT,ZERO=NOBLANK                 
*                                                                               
         CLI   CTGKDEMO+1,0        MACRO FORMULA?                               
         BNE   *+14                NO                                           
         MVC   LSTDEMO(L'LSTDINT),LSTDINT  DISPLAY INTERNAL FORMAT              
         B     LR65                                                             
         MVC   HALF,CTGKDEMO                                                    
         BRAS  RE,DEMODSP6         DISPLAY THE DEMO VIA DEMOCON                 
         MVC   LSTDEMO,DUB                                                      
*                                                                               
LR65     DS    0H                                                               
*                                                                               
* DISPLAY WHATEVER WILL FIT FROM 1ST LINE OF THE FORMULA (IF PRESENT)           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTDINCDQ     X'05' (INPUT FORMULA) ELEMENT                
         BRAS  RE,GETEL                                                         
         BNE   LR70                                                             
*                                                                               
         USING CTDINCD,R6                                                       
         LLC   RF,CTDINLEN                                                      
         SHI   RF,CTDINCFQ         RF = L'(FIRST FORMULA LINE)                  
         CHI   RF,L'LSTFORM1       DOES IT FIT?                                 
         BNH   *+14                YES                                          
         LHI   RF,L'LSTFORM1       USE L'SCREEN FIELD                           
         MVC   LSTFORME,=C'...'    INDICATE THAT IT DIDN'T FIT                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LSTFORM1(0),CTDINIF                                              
*                                                                               
         BRAS  RE,NEXTEL           ANY MORE DEMO FORMULA ELEMENTS?              
         BNE   LR70                NO: CONTINUE                                 
         MVC   LSTFORME,=C'...'    INDICATE THAT IT DIDN'T FIT                  
         DROP  R6                                                               
*                                                                               
LR70     DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR120                                                            
*                                                                               
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
*                                                                               
         LA    R5,P1                                                            
         USING PRTLINED,R5                                                      
*                                                                               
         MVC   PRTFILE,CTGKFILE                                                 
         MVC   PRTMEDIA,CTGKMED                                                 
         MVC   PRTSRC,CTGKSRC                                                   
         MVC   PRTLCODE,LSTLCODE                                                
         MVC   PRTBOOK,LSTBOOK                                                  
         MVC   PRTDEMO,LSTDEMO                                                  
         MVC   PRTDINT,LSTDINT                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTDSCELQ     DESCRIPTION ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   LR90                                                             
*                                                                               
         USING CTDSCD,R6                                                        
         LLC   R1,CTDSCLEN                                                      
         SHI   R1,3                OVERHEAD LENGTH + 1 FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTDESC(0),CTDSC    DESCRIPTION                                  
         DROP  R6                                                               
         LA    R5,L'P(R5)          BUMP TO NEXT PRINT LINE                      
*                                                                               
LR90     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CTDINCDQ     X'05' (INPUT FORMULA) ELEMENT                
         BRAS  RE,GETEL                                                         
         BNE   LR110                                                            
*                                                                               
LR100    DS    0H                                                               
         USING CTDINCD,R6                                                       
         LLC   RF,CTDINLEN                                                      
         SHI   RF,CTDINCFQ                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PRTFORM(0),CTDINIF                                               
*&&DO                                                                           
* IDF ASSIST. IF THIS CODE IS ENABLED, THEN IDF CAN BE USED TO DEBUG            
* THIS LOGIC. THE RELATED CODE AT VALFORM MUST ALSO BE ENABLED.                 
         XC    BLOCK(256),BLOCK                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+8(0),PRTFORM                                               
         AHI   RF,1                                                             
         STC   RF,BLOCK+5                                                       
         AHI   RF,8                                                             
         STC   RF,BLOCK                                                         
         GOTOR VALFORM             BUILD POLISH FORMULA ELEM. IN 'ELEM'         
*&&                                                                             
*                                                                               
         BRAS  RE,NEXTEL           ANY MORE DEMO FORMULA ELEMENTS?              
         BNE   LR110               NO: PRINT THIS RECORD                        
         LA    R5,L'P(R5)          BUMP TO NEXT PRINT LINE                      
         LA    RF,P4+132           A(BEYOND LAST PRINT LINE)                    
         CR    R5,RF               ANY PRINT LINES LEFT?                        
         BL    LR100               NO: CAN'T PRINT ENTIRE FORMULA               
         DROP  R6                                                               
         DROP  R5                                                               
*                                                                               
LR110    DS    0H                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR130                                                            
*                                                                               
LR120    DS    0H                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LR130    DS    0H                                                               
         GOTO1 SEQ                 NEXT DFORM RECORD                            
         B     LR20                                                             
         DROP  R4                                                               
*                                                                               
LRX      DS    0H                                                               
         J     XIT                                                              
*                                                                               
         DROP  R8                                                               
         EJECT                                                                  
SETUP    NTR1                                                                   
*                                                                               
******   OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
******   OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT2,DISTHSPG   REDISPLAY THIS LIST PAGE AFTER SEL           
*********OI    GENSTAT2,USMYERSY   USE CONTROL SYSTEM ERROR MESSAGES            
         OI    GENSTAT4,CONFDEL    CONFIRM DELETES                              
*                                                                               
         L     R3,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,R3                                                      
         CLC   CDEMOCON,=F'0'      IS A(DEMOCON) SET IN COMFACS?                
         BNE   SETUP50             YES                                          
*                                                                               
         GOTO1 CCALLOV,DMCB,0,X'D9000AE0'  NO: GET A(DEMOCON)                   
         CLI   DMCB+4,X'FF'        WAS A(DEMOCON) RETURNED FROM CALLOV?         
         BNE   *+6                                                              
         DC    H'0'                NO                                           
         MVC   CDEMOCON,DMCB       SAVE A(DEMOCON) IN COMFACS                   
*                                                                               
SETUP50  DS    0H                                                               
         MVC   VDEMOCON,CDEMOCON   SAVE A(DEMOCON) IN WORKING STORAGE           
         DROP  R3                                                               
*                                                                               
         GOTO1 VDEMOCON,DMCB,(0,0),('DEMOCON_14',APLDTABS),0                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* HEAD SPECS                                                                    
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
*                                                                               
         SSPEC H1,30,C'DEMO FORMULA REPORT'                                     
         SSPEC H2,30,C'-------------------'                                     
*                                                                               
         SSPEC H4,2,C'F'                                                        
         SSPEC H5,2,C'-'                                                        
         SSPEC H4,5,C'M'                                                        
         SSPEC H5,5,C'-'                                                        
         SSPEC H4,8,C'S'                                                        
         SSPEC H5,8,C'-'                                                        
         SSPEC H4,11,C'LC'                                                      
         SSPEC H5,11,C'--'                                                      
         SSPEC H4,15,C'BOOK'                                                    
         SSPEC H5,15,C'----'                                                    
         SSPEC H4,23,C'DEMO'                                                    
         SSPEC H5,23,C'----'                                                    
         SSPEC H4,40,C'DESCRIPTION/FORMULA'                                     
         SSPEC H5,40,C'-------------------'                                     
         DC    X'00'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DROP  RB,R7                                                            
         EJECT                                                                  
********************************************************************            
* CLRSCRN - SUBROUTINE TO CLEAR SCREEN FIELDS                                   
********************************************************************            
*                                                                               
CLRSCRN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,DEMDESCH         FIRST DATA FIELD                             
         LA    RF,DEMTABH          END OF TWA TAG FIELD                         
*                                                                               
CLRS10   DS    0H                                                               
         CR    R2,RF                                                            
         BE    CLRS30                                                           
*                                                                               
         LLC   RE,0(R2)                                                         
         TM    1(R2),X'20'         DON'T CLEAR PROTECTED FIELDS                 
         BO    CLRS20                                                           
*                                                                               
         LR    R0,RE               SAVE A(FIELD)                                
         SHI   RE,8                8 BYTE HEADER                                
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SHI   RE,8                SUBRACT 8 MORE FOR FIELD EXTENSION           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         LR    RE,R0               RESTORE A(FIELD)                             
CLRS20   AR    R2,RE                                                            
         B     CLRS10                                                           
*                                                                               
CLRS30   DS    0H                                                               
         LA    R2,DEMFRA1H         FIRST DEMO FORMULA FIELD TO CLEAR            
         LHI   R1,#FORFLDS         NUMBERO OF ALPHA FORMULA FIELDS              
*                                                                               
CLRS40   DS    0H                                                               
         LR    R0,RE               SAVE A(FIELD)                                
         SHI   RE,9                8 BYTE HEADER (PLUS ONE FOR EX)              
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         LR    RE,R0               RESTORE A(FIELD)                             
         BCT   R1,CLRS40                                                        
*                                                                               
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*        VARIOUS SUBROUTINES                                                    
***********************************************************************         
*                                                                               
* VALIDATE DEMO FORMULA AND BUILD POLISH ELEMENT IF STRING VALID.               
*                                                                               
* ON EXIT BYTE=X'FE' WITH MESSAGE SET ON ERROR.                                 
*                                                                               
VALFORM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,DEMFORMH         R1=A(FIRST TWA FORMULA LINE)                 
*&&DO                                                                           
* IDF ASSIST. IF THIS CODE IS ENABLED, THEN IDF CAN BE USED TO DEBUG            
* THIS LOGIC. THE RELATED CODE IN LISTRECS MUST ALSO BE ENABLED.                
         CLI   MODE,PRINTREP       ONLINE LIST RECORDS                          
         BNE   *+8                                                              
         LA    R1,BLOCK                                                         
*&&                                                                             
         LR    R2,R1                                                            
         LA    R5,IADR             R5=A(LIST OF FLDHDR ADDRESSES)               
         LA    R6,ISTRING          R6=A(CONVERTED INPUT STRING)                 
         SR    R3,R3               R3=L'CONVERTED INPUT STRING                  
         SR    R8,R8               R8=N'INPUT TWA LINES                         
         MVI   PREVCLAS,0          INIT                                         
         MVI   CURRCLAS,0                                                       
*                                                                               
VALF2    LR    R1,R2                                                            
         ST    R1,0(R5)            SET A(FLD HDR) IN TABLE                      
         LA    R5,4(R5)                                                         
         LA    R8,1(R8)            BUMP TWA LINE COUNT                          
         CLI   5(R2),0             ANY INPUT IN THIS FIELD?                     
         BE    VALF4               NO                                           
*                                                                               
         STC   R8,0(R6)            SET TWA LINE NUMBER IN STRING                
         LLC   R1,5(R2)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R6),8(R2)       MOVE INPUT TO CONVERTED STRING               
         LA    R6,1(R1,R6)         BUMP TO NEXT SLOT                            
         LA    R3,1(R1,R3)         BUMP CONVERTED STRING LENGTH                 
*                                                                               
VALF4    LR    R1,R2               BUMP TO NEXT TWA FIELD                       
         LLC   RE,0(R1)                                                         
         AR    R1,RE                                                            
         LR    R2,R1                                                            
         TM    1(R1),X'20'         IGNORE PROTS                                 
         BO    VALF4                                                            
         LA    R0,DEMFRMXH                                                      
         CR    R1,R0               ANY MORE DEMO FORMULA FIELDS?                
         BNH   VALF2               YES                                          
*                                                                               
         LTR   R3,R3               ANY INPUT?                                   
         BNZ   *+12                YES                                          
         L     R2,IADR                                                          
         J     INVERR              NO - ERROR                                   
*                                                                               
*                                  CONVERT INPUT STRING TO POLISH               
         MVI   0(R6),X'FF'         SET END OF CONVERTED STRING                  
         LA    R6,ISTRING          R6=A(INPUT STRING)                           
*                                                                               
         LARL  RF,TRTPLD           A(PLD TRT TABLE)                             
         LR    R4,R6               A(INPUT STRING)                              
         LR    R5,R3               L'INPUT STRING                               
VALF5    DS    0H                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R4),0(RF)       SCAN FOR PLD INDICATOR (MODIFIES R2)         
         BZ    VALF6               NO PLD INDICATOR CHARACTER FOUND             
*                                                                               
         LA    R1,1(R1)            POINT TO PERSONAL LANGUAGE INDICATOR         
         BRAS  RE,PLENCODE         ON INPUT, R1 = A(PLD CHARACTER)              
         BE    *+16                VALID PLD/MODIFIER COMBO FOUND               
         LA    R2,DEMFORMH         SET CURSOR FOR ERREX2                        
         MVI   FNDX,0              DON'T WORRY ABOUT POS'N WITHIN FIELD         
         B     VALFEE              INVALID MODIFIER WITH PLD                    
*                                                                               
         MVC   1(1,R1),BYTE        REPLACE EBCDIC MODIFIER WITH...              
*                                  ...ENCODED MODIFIER, AND THEN...             
         MVI   0(R1),PLD_CHAR      ...PRECEDE IT BY PLD INDICATOR BYTE          
*                                                                               
         LA    R4,2(R1)            BUMP PAST PLD CHAR AND MODIFIER              
         LR    R0,R4               R0 = A(REMAINING STRING)                     
         SR    R0,R6               R0 = DISP. TO CURRENT POSITION               
         LR    R5,R3               R5 = L'COMPLETE STRING                       
         SR    R5,R0               R5 = L'REMAINING STRING                      
         B     VALF5                                                            
*                                                                               
VALF6    DS    0H                                                               
         LA    R8,ELEM             R8=A(INTERMEDIATE STRING)                    
         MVI   PARENLEV,0          INITIALIZE W/S VALUES                        
         MVI   LASTON,1                                                         
         MVI   ODNUM,0                                                          
*                                                                               
VALF8    CLI   0(R6),16            TEST IF NEW TWA LINE                         
         BH    VALF10                                                           
         LLC   R1,0(R6)            YES - SET R2 FOR ERROR ROUTINES              
         SLL   R1,2                                                             
         LA    R1,IADR-4(R1)                                                    
         L     R2,0(R1)                                                         
         MVI   FNDX,1                                                           
         LA    R6,1(R6)            BUMP TO NEXT                                 
*                                                                               
VALF10   CLI   0(R6),X'FF'         TEST IF END OF INPUT STRING                  
         BE    VALF44                                                           
         MVC   PREVCLAS,CURRCLAS   FOR PSUEDO CHECKING                          
*                                                                               
         LARL  R1,ONTAB            TEST IF AN OPERATION                         
         USING ONTABD,R1                                                        
VALF12   CLI   0(R1),0                                                          
         BE    VALF18                                                           
         CLC   ONOPER,0(R6)                                                     
         BE    *+12                                                             
         LA    R1,ONTABDQ(R1)                                                   
         B     VALF12                                                           
         MVC   CURRCLAS,ONFLAGS                                                 
*                                                                               
         TM    ONFLAGS,PAREN       TEST PARENTHESES                             
         BO    VALF14                                                           
         CLI   LASTON,0            TEST IF OPERAND PROCESSED                    
         BNE   VALFE1                                                           
         TM    ONFLAGS,PSEUDO      TEST IF A PSEUDO OPERATION/AND               
         BZ    *+16                                                             
         TM    PREVCLAS,PSEUDO     CANNOT HAVE TWO IN A ROW                     
         BO    VALFED                                                           
         B     VALF17                                                           
         MVC   LASTON,0(R6)        YES - SAVE OPERATION                         
         B     VALF17                                                           
*                                  HANDLE OPEN PARENTHESIS                      
VALF14   TM    ONFLAGS,OPEN_PAREN                                               
         BZ    VALF16                                                           
         CLI   LASTON,0            TEST IF OPERATION BEFORE OPEN                
         BE    VALFE2                                                           
         MVI   LASTON,1            SET START OF STRING                          
         DROP  R1                                                               
*                                                                               
         LLC   R1,PARENLEV         BUMP PARENTHESIS LEVEL                       
         LA    R1,1(R1)                                                         
         STC   R1,PARENLEV                                                      
         B     VALF17                                                           
*                                  HANDLE CLOSE PARENTHESIS                     
VALF16   SR    R1,R1                                                            
         ICM   R1,1,PARENLEV                                                    
         BZ    VALFE3              MUST HAVE PROCESSED OPEN                     
         BCTR  R1,0                                                             
         STC   R1,PARENLEV                                                      
         MVI   LASTON,0                                                         
*                                  MOVE OPERATION TO STRING                     
VALF17   MVC   0(1,R8),0(R6)                                                    
         LA    R8,1(R8)                                                         
         LA    R6,1(R6)                                                         
         LLC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     VALF8                                                            
*                                  VALIDATE OPERAND (ANNN,P OR ANNN.P)          
VALF18   CLI   LASTON,1                                                         
         BNE   *+8                                                              
         MVI   LASTON,0            RESET START OF STRING                        
         XC    WORK,WORK                                                        
         LA    R1,WORK+1                                                        
         SR    RE,RE                                                            
*                                                                               
VALF20   CLI   0(R6),C'.'          TEST FOR PRECISION INDICATORS                
         BE    VALF24                                                           
         CLI   0(R6),C','                                                       
         BE    VALF24                                                           
         CLI   0(R6),PLD_CHAR      DOES ENCODED MODIFIER FOLLOW THIS?           
         BNE   VALF21                                                           
         CLI   1(R6),PLD_CHAR                                                   
         BNE   VALF21                                                           
         LA    R6,2(R6)            YES: BUMP PAST PLD INDICATOR...              
         B     VALF22              ...AND TREAT NEXT BYTE AS MODIFIER           
*                                                                               
VALF21   DS    0H                                                               
         CLI   0(R6),C'9'          TEST FOR OPERATIONS/SPECIALS                 
         BH    VALF30                                                           
         CLI   0(R6),C'A'                                                       
         BL    VALF30                                                           
*                                                                               
VALF22   DS    0H                                                               
*                                  VALIDATE ANNN                                
         CLI   WORK,0              TEST IF VALIDATED                            
         BNE   VALF26              YES - MUST BE PRECISION INDIC                
         LTR   RE,RE               FIRST CHARACTER CAN BE ANYTHING              
         BZ    VALF28                                                           
         CHI   RE,5                FIELD CAN'T BE LONGER THAN 5 BYTES           
         BNH   *+16                                                             
         TM    WORK+1,X'F0'                                                     
         BO    VALFEB                                                           
         B     VALFE5                                                           
         TM    0(R6),X'F0'         OTHER CHARACTERS MUST BE NUMERIC             
         BNO   VALFE5                                                           
         B     VALF28                                                           
*                                  VALIDATE PRECISION MODIFIER                  
VALF24   TM    WORK+1,X'F0'        CONSTANTS CAN'T HAVE PREC MODIFIER           
         BO    VALFEA                                                           
         CLI   WORK,0              ONLY 1 MODIFIER REQUIRED                     
         BNE   VALFE5                                                           
         CHI   RE,2                ENSURE OPERAND VALID                         
         BL    VALFE5                                                           
*                                                                               
         STC   RE,WORK             SET L'OPERAND                                
         SR    RE,RE                                                            
         LA    R1,WORK+10          MODIFIER GOES INTO WORK+10                   
         B     VALF28                                                           
*                                  VALIDATE PRECISION (0-4)                     
VALF26   CHI   RE,1                                                             
         BNE   VALFE5                                                           
         CLI   0(R6),C'0'                                                       
         BL    VALFE6                                                           
         CLI   0(R6),C'4'                                                       
         BH    VALFE6                                                           
*                                  BUMP TO NEXT STRING CHARACTER                
VALF28   MVC   0(1,R1),0(R6)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         LA    R6,1(R6)                                                         
         LLC   RF,FNDX                                                          
         LA    RF,1(RF)                                                         
         STC   RF,FNDX                                                          
         B     VALF20                                                           
*                                  END OF OPERAND LOCATED                       
VALF30   CLI   WORK,0              TEST IF PRECISION MODIFIER FOUND             
         BNE   VALF32                                                           
         TM    WORK+1,X'F0'        TEST IF A CONSTANT FIELD                     
         BNO   VALF31                                                           
         STC   RE,WORK             YES - VALIDATE NUMERIC 1 THRU 65535          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   R1,DUB                                                           
         ST    R1,DUB                                                           
         OC    DUB(4),DUB                                                       
         BZ    VALFEB                                                           
         OC    DUB(2),DUB                                                       
         BNZ   VALFEB                                                           
         MVI   DUB,X'FF'           SPECIAL MODIFIER FOR CONSTANTS               
         MVC   DUB+1(2),DUB+2                                                   
         B     VALF34                                                           
*                                                                               
VALF31   CHI   RE,2                NO - ENSURE OPERAND LENGTH VALID             
         BL    VALFE5                                                           
         STC   RE,WORK                                                          
*                                  CONVERT OPERAND INTO INTERNAL FMT            
VALF32   LLC   RE,WORK                                                          
         SHI   RE,2                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+2(0)                                                    
         CVB   R1,DUB                                                           
         ST    R1,DUB                                                           
         OC    DUB(3),DUB                                                       
         BNZ   VALFE7                                                           
         MVC   DUB(1),WORK+1                                                    
         MVC   DUB+1(1),DUB+3                                                   
         TM    PRECISN,CTPRECCD_FORMULA_PRECISION_ADJUSTMENT                    
         BZ    VALF33                                                           
*                                                                               
         L     RF,AIO                                                           
         CLC   CTGKDEMO-CTGREC(,RF),DUB                                         
         BNE   VALFEC                                                           
*                                                                               
VALF33   DS    0H                                                               
         MVI   DUB+2,X'FF'         SET NO PRECISION ADJUSTMENTS                 
         OC    WORK+10(2),WORK+10  TEST IF PRECISION INPUT                      
         BZ    VALF34                                                           
         MVC   DUB+2(1),WORK+11                                                 
         NI    DUB+2,X'0F'                                                      
         CLI   WORK+10,C'.'                                                     
         BNE   *+12                                                             
         OI    DUB+2,X'80'         X'8N' = N DECIMAL PLACES                     
         B     *+8                                                              
         OI    DUB+2,X'40'         X'4N' = N INTEGER PLACES                     
*                                  ALLOCATE NUMBER TO OPERAND                   
VALF34   LA    RE,ODSTACK          RE=A(OPERAND STACK)                          
         SR    R1,R1                                                            
         ICM   R1,1,ODNUM          R1=N'ENTRIES IN STACK                        
         BZ    VALF38                                                           
         LR    RF,R1                                                            
*                                  TEST IF ALREADY ALLOCATED A NUMBER           
VALF36   CLC   0(3,RE),DUB                                                      
         BE    VALF40                                                           
         LA    RE,4(RE)                                                         
         BCT   RF,VALF36                                                        
*                                  ASSIGN NEW NUMBER TO OPERAND                 
VALF38   LA    R1,1(R1)                                                         
         STC   R1,ODNUM                                                         
         MVC   0(3,RE),DUB                                                      
         STC   R1,3(RE)                                                         
*                                  NOW PROCESS OPERAND                          
VALF40   MVC   DUB+3(1),3(RE)                                                   
         MVC   0(1,R8),DUB+3       SET OPERAND NUMBER IN OUTPUT STRING          
         LA    R8,1(R8)                                                         
         MVI   LASTON,0                                                         
         B     VALF8                                                            
*                                  HANDLE END OF INPUT STRING                   
VALF44   CLI   LASTON,0            TEST LAST OPERAND PROCESSED                  
         BNE   VALFE8                                                           
         CLI   PARENLEV,0          TEST ALL PARENTHESES PAIRED                  
         BNE   VALFE3                                                           
         MVI   0(R8),C'='          SET END-OF-FORMULA                           
         MVI   1(R8),X'FF'                                                      
*                                  CONVERT STRING TO POLISH FORMAT              
         LA    R6,ELEM             R6=A(INTERMEDIATE STRING)                    
         LA    R8,OSTRING          R8=A(OUTPUT STRING)                          
         SR    R1,R1               R1=CURRENT PAREN LEVEL                       
         XC    ONSTACK,ONSTACK                                                  
*                                                                               
VALF45   CLI   0(R6),X'FF'         TEST END-OF-STRING                           
         BE    VALF48                                                           
         CLI   0(R6),C'='          TEST END-OF-FORMULA                          
         BE    VALF46D                                                          
*                                  DEAL WITH OPERANDS                           
         LARL  RE,ONTAB            CHECK FOR PSUEDO OPERATOR                    
         USING ONTABD,RE                                                        
VAL45A   CLI   0(RE),0                                                          
         BE    VAL45B                                                           
         CLC   0(1,R6),ONOPER      TRY TO MATCH OPERATORS                       
         BE    *+12                                                             
         LA    RE,ONTABDQ(RE)                                                   
         B     VAL45A                                                           
*                                                                               
         CLI   ONFLAGS,PSEUDO      PSUEDOS ARE LIKE OPERANDS                    
         BE    *+8                                                              
VAL45B   CLI   0(R6),X'40'         TRUE OPERATOR                                
         BH    VALF46A                                                          
*                                                                               
         LARL  RE,ONTAB            CHECK FOR PSUEDO OPERATOR                    
VAL45C   CLI   0(RE),0               IN NEXT POSTION                            
         BE    VAL45D                                                           
         CLC   1(1,R6),ONOPER      TRY TO MATCH OPERATORS                       
         BE    *+12                                                             
         LA    RE,ONTABDQ(RE)                                                   
         B     VAL45C                                                           
         CLI   ONFLAGS,PSEUDO      DELAY SAVED OPERATOR IF PSUEDO               
         BE    VALF46MV                                                         
         DROP  RE                                                               
*                                                                               
VAL45D   CLI   LASTON,0            TEST IF OPERATION SAVED                      
         BE    VALF46MV            NO - OUTPUT OPERAND                          
         MVC   0(1,R8),0(R6)       YES - OUTPUT OPERAND/OPERATION               
         LA    R8,1(R8)                                                         
         MVC   0(1,R8),LASTON                                                   
         MVI   LASTON,0                                                         
         B     VALF4686                                                         
*                                  DEAL WITH OPEN PAREN                         
VALF46A  CLI   0(R6),C'('                                                       
         BNE   VALF46B                                                          
         LA    RE,ONSTACK(R1)      RE=A(SAVE OPERATION STACK ENTRY)             
         LA    R1,1(R1)            BUMP PAREN LEVEL                             
         CLI   LASTON,0            TEST IF OPERATION SAVED                      
         BE    *+10                                                             
         MVC   0(1,RE),LASTON      YES - SAVE IN STACK                          
         MVI   LASTON,0                                                         
         B     VALF466                                                          
*                                  DEAL WITH CLOSE PAREN                        
VALF46B  CLI   0(R6),C')'                                                       
         BNE   VALF46C                                                          
         BCTR  R1,0                DECREMENT PAREN LEVEL                        
         LA    RE,ONSTACK(R1)      RE=A(SAVED OPERATION STACK ENTRY)            
         CLI   0(RE),0             TEST IF OPERATION SAVED                      
         BE    VALF466                                                          
         MVC   0(1,R8),0(RE)       YES - OUTPUT SAVED OPERATION                 
         MVI   0(RE),0                                                          
         B     VALF4686                                                         
*                                  DEAL WITH OPERATIONS                         
VALF46C  CLI   LASTON,0            TEST IF OPERATION SAVED                      
         BE    *+14                                                             
         MVC   0(1,R8),LASTON      YES - OUTPUT SAVED OPERATION                 
         LA    R8,1(R8)                                                         
         MVC   LASTON,0(R6)        SAVE OPERATION IN TEMPORARY SAVE             
         B     VALF466                                                          
*                                  DEAL WITH EQUALS SIGN                        
VALF46D  CLI   LASTON,0            TEST IF OPERATION SAVED                      
         BE    *+14                                                             
         MVC   0(1,R8),LASTON      YES - OUTPUT SAVED OPERATION                 
         LA    R8,1(R8)                                                         
*                                  MOVE TO OUTPUT STRING AND BUMP PTRS          
VALF46MV MVC   0(1,R8),0(R6)                                                    
VALF4686 LA    R8,1(R8)                                                         
VALF466  LA    R6,1(R6)                                                         
         B     VALF45                                                           
*                                  BUILD POLISH ELEMENT                         
VALF48   XC    ELEM,ELEM                                                        
E        USING CTDPFCD,ELEM                                                     
         LA    R1,E.CTDPFORM                                                    
*                                  MOVE OPERANDS TO ELEMENT                     
         LA    RE,ODSTACK                                                       
         LLC   R0,ODNUM                                                         
         SR    RF,RF                                                            
VALF50   MVC   0(3,R1),0(RE)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,VALF50                                                        
*                                  MOVE POLISH STRING TO ELEMENT                
         LA    RE,OSTRING                                                       
         SR    R8,RE                                                            
         LA    RF,CTDPFCFQ(R8,RF)                                               
         CHI   RF,255              TEST DATA WILL FIT IN ELEMENT                
         BH    VALFE9                                                           
         STC   RF,E.CTDPFLEN       ELEMENT LENGTH                               
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),OSTRING                                                  
         MVI   E.CTDPFCDE,CTDPFCDQ     ELEMENT CODE                             
         DROP  E                                                                
*                                                                               
         MVI   BYTE,X'FF'          SET VALID EXIT                               
*                                                                               
         J     XIT                                                              
*                                                                               
         EJECT                                                                  
*              HANDLE VALIDATION ERRORS                                         
*                                                                               
VALFE1   LHI   R1,1                                                             
         B     VALERR                                                           
VALFE2   LHI   R1,2                                                             
         B     VALERR                                                           
VALFE3   LHI   R1,3                                                             
         B     VALERR                                                           
VALFE4   LHI   R1,4                                                             
         B     VALERR                                                           
VALFE5   LHI   R1,5                                                             
         B     VALERR                                                           
VALFE6   LHI   R1,6                                                             
         B     VALERR                                                           
VALFE7   LHI   R1,7                                                             
         B     VALERR                                                           
VALFE8   LHI   R1,8                                                             
         B     VALERR                                                           
VALFE9   LHI   R1,9                                                             
         B     VALERR                                                           
VALFEA   LHI   R1,10                                                            
         B     VALERR                                                           
VALFEB   LHI   R1,11                                                            
         B     VALERR                                                           
VALFEC   LHI   R1,12                                                            
         B     VALERR                                                           
VALFED   LHI   R1,13                                                            
         B     VALERR                                                           
VALFEE   LHI   R1,14                                                            
         B     VALERR                                                           
*                                  FORMAT MESSAGE                               
VALERR   DS    0H                                                               
         MVI   CONHEAD,C' '        FILL MESSAGE AREA WITH SPACES                
         MVC   CONHEAD+1(L'CONHEAD-1),CONHEAD                                   
         BCTR  R1,0                                                             
         MHI   R1,L'ERRMSGS        DISPLACEMENT TO ERROR MESSAGE                
         LARL  R0,ERRMSGS          A(ERROR MESSAGE TABLE)                       
         AR    R1,R0               A(THIS ERROR MESSAGE)                        
         MVC   CONHEAD(L'ERRMSGS),0(R1)                                         
         CLI   FNDX,0                                                           
         BE    VALERRX                                                          
*                                                                               
         LA    R1,CONHEAD+L'CONHEAD-1                                           
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(8,R1),=C'NEAR COL'                                             
         LLC   RE,FNDX                                                          
         CVD   RE,DUB                                                           
         UNPK  11(2,R1),DUB                                                     
         OI    12(R1),X'F0'                                                     
*                                                                               
VALERRX  OI    CONHEADH+6,X'80'                                                 
         MVI   BYTE,X'FE'                                                       
*                                                                               
         J     XIT                                                              
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*        DISPLAY THE ALPHA FORMULA FIELD(S)                                     
*                                                                               
DISFORMA NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                  BUILD ARRAY: TWA START/END ADDRESSES         
         LA    RF,DEMFRA1H         A(TWA FIELD)                                 
         ST    RF,FORALPHA+0       SAVE A(TWA FIELD HEADER)                     
         LLC   R0,DEMFRA1H         L'FIELD                                      
         AR    RF,R0               BUMP TO NEXT FIELD                           
         BCTR  RF,0                RF = A(LAST BYTE OF FIELD)                   
         ST    RF,FORALPHA+4       SAVE A(LAST BYTE)                            
         LA    RF,DEMFRA2H                                                      
         ST    RF,FORALPHA+8                                                    
         LLC   R0,DEMFRA2H                                                      
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         ST    RF,FORALPHA+12                                                   
         LA    RF,DEMFRA3H                                                      
         ST    RF,FORALPHA+16                                                   
         LLC   R0,DEMFRA3H                                                      
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         ST    RF,FORALPHA+20                                                   
         LA    RF,DEMFRA4H                                                      
         ST    RF,FORALPHA+24                                                   
         LLC   R0,DEMFRA4H                                                      
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         ST    RF,FORALPHA+28                                                   
         LA    RF,DEMFRA5H                                                      
         ST    RF,FORALPHA+32                                                   
         LLC   R0,DEMFRA5H                                                      
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         ST    RF,FORALPHA+36                                                   
         LA    RF,DEMFRA6H                                                      
         ST    RF,FORALPHA+40                                                   
         LLC   R0,DEMFRA6H                                                      
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         ST    RF,FORALPHA+44                                                   
         XC    FORALPHA+48(4),FORALPHA+48                                       
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,DEMFORMH+5     LENGTH OF FORMULA I/P (1ST LINE)             
         BZ    DISFRMAX                                                         
*                                  SET UP DBLOCK FOR DEMOCON                    
         XC    BLOCK(256),BLOCK    FIELD 'BLOCK' USED FOR DBLOCK                
         USING DEDBLOCK,R1                                                      
         LA    R1,BLOCK                                                         
         MVC   DBFILE,SVDBFILE     FILE                                         
         L     RF,AIO                                                           
         USING CTGKEY,RF                                                        
         MVC   DBSELMED,CTGKMED    MEDIA                                        
         MVC   DBSELSRC,CTGKSRC    SOURCE                                       
         DROP  RF                                                               
         CLI   DBSELMED,C'D'                                                    
         BNE   *+8                                                              
         MVI   DBSELMED,C'T'                                                    
         L     RE,ACOMFACS                                                      
         ST    RE,DBCOMFCS         A(COMFACS)                                   
         DROP  R1                                                               
*                                  MOVE I/P LINES TO ELEM (MAX.=240)            
         LA    R6,ELEM                                                          
         LA    R1,DEMFORMH                                                      
         LHI   R5,4                                                             
         SR    RE,RE                                                            
         SR    R3,R3                                                            
DA120    ICM   RE,1,5(R1)          LENGTH OF I/P                                
         BZ    DA160                                                            
         AR    R3,RE               R3 - TOTAL LENGTH OF I/P                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),8(R1)       MOVE LINE TO ELEM                            
         AR    R6,RE                                                            
         LA    R6,1(R6)                                                         
         LLC   RF,0(R1)                                                         
         AR    R1,RF                                                            
         BCT   R5,DA120                                                         
*                                                                               
DA160    MVI   0(R6),X'FF'         SET END-OF-DATA                              
         LA    R8,FORALPHA         ADDRESSES OF ALPHA FIELDS                    
         L     RF,0(R8)                                                         
         LA    R5,8(RF)            R5 - O/P                                     
         LA    R6,ELEM             R6 - I/P                                     
*                                                                               
*                                  TRANSFER I/P TO ALPHA FORMULA                
DA300    CLI   0(R6),X'FF'                                                      
         BE    DISFRMAX                                                         
         CLI   0(R6),C'A'          LOOKING FOR ANNN                             
         BL    *+12                                                             
         CLI   0(R6),C'Z'                                                       
         BNH   DA400                                                            
*                                                                               
         MVC   0(1,R5),0(R6)       MOVE TO O/P                                  
         CLI   0(R6),C','                                                       
         BE    DA330                                                            
         CLI   0(R6),C'.'                                                       
         BE    DA330                                                            
         CLI   0(R6),C'('                                                       
         BE    DA330                                                            
         CLI   0(R6),C'A'                                                       
         BNL   DA330                                                            
         STM   R5,R6,DUB2          STORE ADDRESSES OF LAST OPERATOR             
DA330    LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
*                                  TEST FOR END OF O/P FIELD                    
         L     RE,4(R8)                                                         
         CR    RE,R5                                                            
         BNL   DA300               RETURN TO SCAN                               
*                                                                               
DA340    BCTR  R5,0                                                             
         C     R5,DUB2                                                          
         BE    *+12                                                             
         MVI   0(R5),C' '          BLANK OUT END OF LINE                        
         B     DA340                                                            
*                                                                               
         L     R6,DUB2+4           BACKUP I/P FIELD                             
         LA    R6,1(R6)                                                         
         LA    R8,8(R8)                                                         
         OC    0(4,R8),0(R8)                                                    
         BZ    DISFRMAX                                                         
*                                                                               
         L     RF,0(R8)                                                         
         LA    R5,8(RF)                                                         
         B     DA300               RETURN TO SCAN                               
*                                                                               
*                                  AN, - ANN, - ANNN,                           
DA400    DS    0H                  TEST FOR END OF O/P FIELD                    
         L     RE,4(R8)                                                         
         SHI   RE,9                                                             
         CR    RE,R5                                                            
         BNL   DA410               OK                                           
*                                                                               
         LA    R8,8(R8)                                                         
         OC    0(4,R8),0(R8)       ADDRESS OF NEXT O/P FIELD                    
         BZ    DISFRMAX                                                         
*                                                                               
         L     RF,0(R8)                                                         
         LA    R5,8(RF)                                                         
*                                                                               
DA410    LA    RE,2(R6)                                                         
         LA    RF,3                                                             
         SR    R1,R1                                                            
DA420    CLI   0(RE),C'0'          SCAN FOR ANY NON-NUMERIC                     
         BL    DA440                                                            
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   RF,DA420                                                         
         DC    H'0'                                                             
*                                  CONVERT NNN TO BINARY                        
DA440    MVC   DUB,=8C'0'                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),1(R6)                                                     
         CLC   DUB,=8C'0'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R6)                                                      
         CVB   R1,DUB                                                           
         MVI   WORK+0,X'00'        WORK(3) - I/P TO DEMOCON                     
         MVI   WORK+1,C'R'         PASS 'R' TO DEMOCON                          
         STC   R1,WORK+2                                                        
         LR    R6,RE               R6 - ADDRESS OF COMMA                        
         XC    WORK+4(16),WORK+4                                                
         GOTO1 VDEMOCON,DMCB,(1,WORK),(6,WORK+4),(0,BLOCK)                      
*                                                                               
         LA    RE,WORK+5           SUPPRESS 'R'                                 
         LA    RF,8                                                             
DA480    CLI   0(RE),X'00'                                                      
         BE    DA300               RETURN TO SCAN                               
         CLI   0(RE),C' '                                                       
         BE    DA300               RETURN TO SCAN                               
         CLI   0(RE),C'+'          CHANGE A65+ TO A65P                          
         BNE   *+8                                                              
         MVI   0(RE),C'P'                                                       
         MVC   0(1,R5),0(RE)       MOVE TO O/P                                  
         LA    R5,1(R5)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,DA480                                                         
         DC    H'0'                                                             
*                                                                               
DISFRMAX DS    0H                                                               
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
PLENCODE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* ENCODE A PERSONAL LANGUAGE DEMO MODIFIER.                                     
* INPUT:  R1 POINTS TO A 2-BYTE INPUT PLD MODIFIER.                             
* OUTPUT: FIELD 'BYTE' CONTAINS THE ENCODED MODIFIER.                           
* CC IS SET NE IF THE COMBINATION IS INVALID.                                   
*                                                                               
* SEARCH THE MODIFIER/PLD TRANSLATION TABLE                                     
*                                                                               
         L     RF,APLENCTB         MODIFIER ENCODING TABLE                      
         LH    R0,0(RF)            L'ENTRY                                      
         AHI   RF,2                START OF TABLE ENTRIES                       
         USING PLD_MOD_ENCODE_TABLED,RF                                         
*                                                                               
PLENC10  DS    0H                                                               
         OC    0(3,RF),0(RF)       EOT?                                         
         JZ    NO                  MODIFIER/PLD NOT FOUND: INVALID              
*                                                                               
         CLC   PLD_MODIFIER_CHAR,1(R1)   MATCH ON MODIFIER CHARACTER?           
         BNE   *+14                      NO: TRY NEXT TABLE ENTRY               
         CLC   PLD_ATTRIBUTE_CHAR,0(R1)  MATCH ON PLD CHARACTER?                
         BE    PLENC20                   YES: ENCODE THE MODIFIER               
*                                                                               
         AR    RF,R0               BUMP TO NEXT ENTRY                           
         B     PLENC10                                                          
*                                                                               
PLENC20  MVC   BYTE,PLD_ENCODED_MODIFIER  RETURN THE ENCODED MODIFIER           
         DROP  RF                                                               
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
DEMODSP6 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* DISPLAY DEMO CATEGORY IN DEMOCON OUTPUT TYPE 2 FORMAT (ONE-CHARACTER          
*  MODIFIER FOLLOWED BY A SIX-CHARACTER DESCRIPTION).                           
* INPUT:  FIELD 'HALF' CONTAINS THE DEMO.                                       
*         R4 = A(DEMO FORMULA RECORD KEY)                                       
* OUTPUT: FIELD 'DUB' (FIRST 7 BYTES) CONTAINS THE DISPLAYABLE DEMO.            
*                                                                               
         USING CTGKEY,R4                                                        
*                                  SET UP DBLOCK FOR DEMOCON                    
         XC    BLOCK(256),BLOCK    FIELD 'BLOCK' USED FOR DBLOCK                
         USING DEDBLOCK,R1                                                      
         LA    R1,BLOCK                                                         
         MVC   DBFILE,SVDBFILE     FILE                                         
         MVC   DBSELMED,CTGKMED    MEDIA                                        
         MVC   DBSELSRC,CTGKSRC    SOURCE                                       
         CLI   DBSELMED,C'D'                                                    
         BNE   *+8                                                              
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS   A(COMFACS)                                   
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+1(2),HALF                                                   
         LLC   RE,HALF             EXTRACT THE EBCDIC MODIFIER                  
         A     RE,APLMODTB                                                      
         MVC   FULL+1(1),0(RE)     REPLACE ENCODED MODIFIER WITH ALPHA          
*                                                                               
         CLI   CTGKCODE,X'FF'                                                   
         BE    DEMD10                                                           
         XC    FULL,FULL                                                        
         MVC   FULL+1(1),HALF                                                   
         MVC   FULL+2(1),CTGKCODE                                               
         MVC   FULL+3(1),HALF+1                                                 
         MVI   DBDEMTYP,C'4'                                                    
         DROP  R1                                                               
         DROP  R4                                                               
*                                                                               
DEMD10   DS    0H                                                               
         XC    DUB,DUB                                                          
         GOTO1 VDEMOCON,DMCB,(1,FULL),(2,DUB),(0,BLOCK)                         
*                                                                               
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*        TABLES AND CONSTANTS                                                   
***********************************************************************         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
* LTORG                                                                         
         LTORG                                                                  
         SPACE 3                                                                
         EJECT                                                                  
*                                  TABLE OF VALID FILES                         
FILETAB  DS    0H                                                               
         DC    C'T',C'TP '         TPT                                          
         DC    C'T',C'TP '         DPT                                          
         DC    C'C',C'TP '         COUNTY                                       
         DC    C'C',C'PAV'         CABLE                                        
         DC    C'P',C'PAV'         PAV                                          
         DC    C'M',C'NAD'         MOVIE                                        
         DC    C'N',C'NAD'         NAD                                          
         DC    C'E',C'EVN'         ESTIMATE                                     
         DC    C'I',C'INV'         INV                                          
         DC    C'I',C'IAG'         IAG                                          
         DC    C'O',C'OPT'         OPTIMUM                                      
         DC    C'A',C'AE '         AUD ESTIM                                    
         DC    C'R',C'RUA'         RADIO COUNTY COVERAGE                        
         DC    X'00'                                                            
*                                                                               
FILETABD DSECT                                                                  
FTKEY    DS    C                                                                
FTCODE   DS    CL3                                                              
FILETABQ EQU   *-FILETABD                                                       
*                                                                               
TA0A22   RSECT                                                                  
*                                  TABLE OF VALID SUB-FILES                     
MEDTAB   DS    0H                                                               
MEDTABLQ EQU   1                   TABLE ENTRY LENGTH                           
         DC    C'T'                USTV                                         
         DC    C'C'                CANTV                                        
         DC    C'R'                RADIO                                        
         DC    C'D'                DPT                                          
         DC    C'N'                NETWORK                                      
         DC    C'O'                OVERNITE                                     
         DC    C'H'                NHI                                          
         DC    C'P'                MPA                                          
         DC    C'V'                VPH                                          
         DC    C'I'                IMP                                          
         DC    C'U'                UPGRADE                                      
         DC    C'W'                WEEKLY                                       
         DC    C'A'                A                                            
         DC    X'00'                                                            
*                                                                               
*                                  TABLE OF VALID SOURCE CODES                  
SRCTAB   DS    0H                                                               
SRCTABLQ EQU   1                   TABLE ENTRY LENGTH                           
         DC    C'A'                ARB                                          
         DC    C'N'                NSI                                          
         DC    C'F'                FUS                                          
         DC    C'S'                SRC                                          
         DC    C'M'                BBR                                          
         DC    C'M'                MFX                                          
         DC    C'I'                IDX: OPTIMUM PROGRAM INDEX                   
         DC    C'Q'                TVQ                                          
         DC    C'R'                RAR: RADAR NET RADIO                         
         DC    C'G'                IAG FACTORS                                  
         DC    C'T'                TRITON                                       
         DC    C'V'                VIDEOLOGY                                    
         DC    X'00'                                                            
*                                                                               
*                                  VALID FILE/MEDIA/SOURCE COMBINATIONS         
FMSTAB   DS    0H                                                               
FMSTABLQ EQU   3                   TABLE ENTRY LENGTH                           
         DC    C'CNN'                                                           
         DC    C'CHN'              CABLE NHTI FILE                              
         DC    C'CUN'              COUNTY COVERAGE - NSI US                     
         DC    C'TON'              NSI OVERNITES - USTV                         
         DC    C'TTN'                                                           
         DC    C'TTF'              FUSION                                       
         DC    C'TTM'              MEDIAFAX TPT                                 
         DC    C'TTA'                                                           
         DC    C'TTS'                                                           
         DC    C'TRA'                                                           
         DC    C'TRN'                                                           
         DC    C'TRM'                                                           
         DC    C'TRR'              RADAR RADIO                                  
         DC    C'TCN'                                                           
         DC    C'TCA'                                                           
         DC    C'TWA'              BBM TV WEEKLY                                
         DC    C'TDN'                                                           
         DC    C'TDA'                                                           
         DC    C'MNN'              MOVIEGOER                                    
         DC    C'PTM'              MEDIAFAX PAV                                 
         DC    C'PTN'                                                           
         DC    C'PTA'                                                           
         DC    C'PNN'                                                           
         DC    C'TPN'                                                           
         DC    C'TPA'                                                           
         DC    C'NHN'                                                           
         DC    C'NNN'                                                           
         DC    C'EVN'                                                           
         DC    C'EIN'                                                           
         DC    C'IUA'                                                           
         DC    C'IUN'                                                           
         DC    C'TVQ'                                                           
         DC    C'TWN'                                                           
         DC    C'OPI'                                                           
         DC    C'ANN'                                                           
         DC    C'IAG'                                                           
         DC    C'RUA'                                                           
         DC    C'CNR'              RENTRAK                                      
         DC    C'TRT'              TRITON                                       
         DC    C'TTV'              VIDEOLOGY                                    
         DC    X'00'                                                            
*                                  TABLE OF VALID FIELD PRECISIONS              
PRECTAB  DS    0H                                                               
         DC    X'44',CL10'(0000)'                                               
         DC    X'43',CL10'THOUSANDS'                                            
         DC    X'42',CL10'HUNDREDS'                                             
         DC    X'41',CL10'TENS'                                                 
         DC    X'40',CL10'INTEGER'                                              
         DC    X'81',CL10'1DECIMAL'                                             
         DC    X'82',CL10'2DECIMAL'                                             
         DC    X'83',CL10'3DECIMAL'                                             
         DC    X'84',CL10'4DECIMAL'                                             
         DC    X'20',CL10'FORMULA'                                              
         DC    X'64',CL10'F(0000)'                                              
         DC    X'63',CL10'FTHOUSANDS'                                           
         DC    X'62',CL10'FHUNDREDS'                                            
         DC    X'61',CL10'FTENS'                                                
         DC    X'60',CL10'FINTEGER'                                             
         DC    X'A1',CL10'F1DECIMAL'                                            
         DC    X'A2',CL10'F2DECIMAL'                                            
         DC    X'A3',CL10'F3DECIMAL'                                            
         DC    X'A4',CL10'F4DECIMAL'                                            
         DC    X'00'                                                            
*                                                                               
PRECTABD DSECT                                                                  
PRECCODE DS    X                                                                
PRECDESC DS    CL10                                                             
PRECTABQ EQU   *-PRECTABD                                                       
*                                                                               
TA0A22   RSECT                                                                  
*                                  TABLE OF VALID OPERATIONS                    
ONTAB    DS    0H                                                               
         DC    C'+',X'00'                                                       
         DC    C'-',X'00'                                                       
         DC    C'*',X'00'                                                       
         DC    C'/',X'00'                                                       
         DC    C'(',AL1(OPEN_PAREN+PAREN)                                       
         DC    C')',AL1(PAREN)                                                  
         DC    C'''',AL1(PSEUDO)                                                
         DC    C'"',AL1(PSEUDO)                                                 
         DC    C'!',AL1(PSEUDO)                                                 
         DC    X'00'                                                            
*                                                                               
ONTABD   DSECT                                                                  
ONOPER   DS    C                                                                
ONFLAGS  DS    X                                                                
PAREN    EQU   X'80'                                                            
OPEN_PAREN EQU X'40'                                                            
PSEUDO   EQU   X'20'                                                            
ONTABDQ  EQU   *-ONTABD                                                         
*                                                                               
TA0A22   RSECT                                                                  
*                                                                               
OPTMACS  DS    0H                                                               
         DC    X'10',C'DEFAULT  '                                               
         DC    X'00',C'OVERRIDE '                                               
         DC    X'FF'                                                            
OPTDEMS  DS    0H                                                               
         DC    AL1(CTPRECFL_NO_OP_DEMO),C'NOVALUE  '    X'10'                   
         DC    X'FF'                                                            
*                                                                               
OPTTABD  DSECT                                                                  
OPTFLAGS DS    X                                                                
OPTDESC  DS    CL9                                                              
OPTTABDQ EQU   *-OPTTABD                                                        
         EJECT                                                                  
TA0A22   RSECT                                                                  
*                                                                               
* PERSONAL LANGUAGE DEMO CHARACTER TRANSLATE TABLE                              
*                                                                               
         DS    0D                                                               
TRTPLD   DC    XL256'00'                                                        
         ORG   TRTPLD+PLD_CHAR                                                  
         DC    X'FF'                                                            
         ORG                                                                    
*                                                                               
PLD_CHAR EQU   X'4F'               VERTICAL BAR "|"                             
         EJECT                                                                  
*                                  TABLE OF ERROR MESSAGES                      
         DS    0H                                                               
ERRMSGS  DS    0CL48                                                            
         DC    CL48'OPERATOR NOT PRECEED BY OPERAND'                  1         
         DC    CL48'PARENTHESIS NOT PRECEDED BY OPERATION'            2         
         DC    CL48'UNPAIRED PARENTHESIS'                             3         
         DC    CL48'PARENTHESIS NOT PRECEDED BY OPERAND'              4         
         DC    CL48'INVALID OPERAND FORMAT S/B ANNN(,N) OR ANNN(.N)'  5         
         DC    CL48'INVALID PRECISION MODIFIER S/B 0 THRU 4'          6         
         DC    CL48'INVALID DEMO NUMBER'                              7         
         DC    CL48'MISSING OPERAND'                                  8         
         DC    CL48'FORMULA TOO LONG'                                 9         
         DC    CL48'PRECISION MODIFIER NOT ALLOWED AFTER CONSTANT'   10         
         DC    CL48'INVALID CONSTANT VALUE S/B 1 THRU 65535'         11         
         DC    CL48'OUTPUT PRECISION=FORMULA INCOMPATABLE'           12         
         DC    CL48'PSUEDO OP PRECEDED BY PSUEDO OP'                 13         
         DC    CL48'A MODIFIER IS INVALID WITH PERS. LANG ATTRIBUTE' 14         
         EJECT                                                                  
* ++INCLUDE CTGENFILE                                                           
* ++INCLUDE DDCOREQUS                                                           
* ++INCLUDE DDSPLWORKD                                                          
* ++INCLUDE DDSPOOLD                                                            
* ++INCLUDE DDMONYREQU                                                          
* ++INCLUDE DEDBLOCK                                                            
* ++INCLUDE DEDEMEQUS                                                           
* ++INCLUDE DEDEMTABD                                                           
* ++INCLUDE FAFACTS                                                             
* ++INCLUDE DDCOMFACS                                                           
* ++INCLUDE DDACTIVD                                                            
* ++INCLUDE CTSFMFFD                                                            
* ++INCLUDE DDGENTWA                                                            
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDMONYREQU                                                     
         EJECT                                                                  
DEDBLOCK DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMEQUS2                                                     
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMB5D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM98D                                                       
         EJECT                                                                  
* ++INCLUDE CTSFMWORKD                                                          
         PRINT OFF                                                              
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
********************************************************************            
* TA0A22 SAVED STORAGE DSECT                                                    
********************************************************************            
*                                                                               
         ORG   SYSSPARE                                                         
DUB2     DS    D                                                                
VDEMOCON DS    V                                                                
*                                                                               
* PERSONAL LANGUAGE DEMO MODIFIER TRANSLATE TABLES                              
         DS    0A                                                               
APLDTABS DS    0XL(6*4)            6 ADDRESSES                                  
APLMODTB DS    V                   A(MODIFIER TABLE)                            
APLPLDTB DS    V                   A(PERSONAL LANG. ATTRIBUTE TABLE)            
APLENCTB DS    V                   A(MODIFIER BYTE ENCODING TABLE)              
APLPRNTB DS    V                   A(PERSONAL LANG. PRINTABLE DESCRIPS)         
         DS    V                   SPARE                                        
         DS    V                   SPARE                                        
*                                                                               
IADR     DS    6A                                                               
FORALPHA DS    (2*#FORFLDS)A,A     ADDRESSES OF 6 ALPHA FORMULA FIELDS          
#FORFLDS EQU   6                   SIX ALPHA FORMULA FIELDS                     
ODNUM    DS    X                                                                
PRECISN  DS    X                                                                
ODSTACK  DS    64CL4                                                            
ONSTACK  DS    CL32                                                             
LASTON   DS    X                                                                
PARENLEV DS    X                                                                
DIROPT   DS    X                                                                
PREVCLAS DS    X                                                                
CURRCLAS DS    X                                                                
SVFILE   DS    C                                                                
SVSUBMED DS    C                                                                
SVDBFILE DS    CL3                                                              
SVSOURCE DS    C                                                                
SVSTRTBK DS    XL2                                                              
SVDEMMOD DS    C                                                                
SVDEMO2  DS    XL2                                                              
FNDX     DS    X                                                                
SAVEKEY  DS    XL(L'KEY)           SAVED DFORMULA KEY                           
ISTRING  DS    6CL80                                                            
OSTRING  DS    CL256                                                            
         EJECT                                                                  
********************************************************************            
* LIST LINE DSECT                                                               
********************************************************************            
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTLCODE DS    CL2                 LOOKUP CODE                                  
         DS    CL2                                                              
LSTBOOK  DS    CL6                 BOOK (MONTHLY OR WEEKLY)                     
         DS    CL2                                                              
LSTDEMO  DS    CL7                 DEMO (DEMOCON OUTPUT TYPE 6)                 
         DS    CL2                                                              
LSTDINT  DS    CL5                 INTERNAL DEMO NUMBER                         
         DS    CL3                                                              
LSTFORM1 DS    CL43                FIRST FORMULA LINE (WHATEVER FITS)           
LSTFORME DS    CL3                 C'...' IF FORMULA DOESN'T FIT                
         SPACE 3                                                                
********************************************************************            
* REPORT LINE DSECT                                                             
********************************************************************            
PRTLINED DSECT                                                                  
         DS    C                                                                
PRTFILE  DS    C                   FILE                                         
         DS    CL2                                                              
PRTMEDIA DS    C                   MEDIA                                        
         DS    CL2                                                              
PRTSRC   DS    C                   SOURCE                                       
         DS    CL2                                                              
PRTLCODE DS    CL2                 LOOKUP CODE                                  
         DS    CL2                                                              
PRTBOOK  DS    CL6                 BOOK                                         
         DS    CL2                                                              
PRTDEMO  DS    CL7                 DEMO (DEMOCON OUTPUT TYPE 6)                 
         DS    CL2                                                              
PRTDINT  DS    CL5                 INTERNAL DEMO NUMBER                         
         DS    CL2                                                              
PRTDESC  DS    CL60                DESCRIPTION                                  
         ORG   PRTDESC                                                          
PRTFORM  DS    CL60                DEMO FORMULA                                 
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008CTSFM22   10/29/15'                                      
         END                                                                    
