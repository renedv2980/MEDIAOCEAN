*          DATA SET SPBUY55    AT LEVEL 029 AS OF 04/02/13                      
*PHASE T21155C                                                                  
         TITLE 'T21155 - SPOTPAK BUY PROGRAM - AFFID/INTG/FLM ETC'              
T21155   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21155                                                         
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21155+4096,R9                                                   
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
         SPACE 1                                                                
*========================================================*                      
* ON ENTRY R2 HAS A(FIRST ACTV FLDHDR)                   *                      
*========================================================*                      
         SPACE 1                                                                
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
*                                                                               
         CLC   =C'COMP',0(R4)                                                   
         BE    COMP                                                             
*                                                                               
         MVI   ERRCD,NORECALL                                                   
         OC    SVKEY+14(4),SVKEY+14                                             
         BZ    BUYERR                                                           
         MVC   KEY,SVKEY                                                        
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         MVI   UPDSW,0             RESET GETREC SWITCH                          
         MVC   BUSTAT,BDSTAT                                                    
         XC    PRDLIST,PRDLIST     CLEAR ADDED PRD LIST                         
*                                                                               
         MVI   ERRCD,TRCDERR                                                    
         CLI   FLEN+1,3                                                         
         BNE   BC2                                                              
         CLC   =C'RSV',0(R4)                                                    
         BE    B700                                                             
         B     BUYERR                                                           
BC2      DS    0H                                                               
         CLI   FLEN+1,1                                                         
         BNE   BUYERR                                                           
         CLI   0(R4),C'I'                                                       
         BE    B700                                                             
         CLI   SVPRD,X'FF'                                                      
         BNE   BUYERR                                                           
         CLI   0(R4),C'F'          FILM NUMBER ASSGN                            
         BE    B805                                                             
         DC    H'0'                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1  1                                                                
*                                                                               
BUYERR   GOTO1 ERROR                                                            
*                                                                               
BCX      DS    0H                                                               
         GOTO1 SETCHGDT                                                         
*                                                                               
         LA    RE,PRDLIST          ADDED PRD LIST                               
         CLI   0(RE),0                                                          
         BE    *+8                                                              
         ST    RE,DMCB+20          PASS ADDR IF ANY PRDS IN LIST                
         MVC   KEY,SVKEY     DISK ADDR MUST BE OK FOR DMDAPTRS                  
*                                                                               
         CLI   UPDSW,C'Y'          TEST GETREC REQUIRED                         
         BNE   BCX2                                                             
         MVC   AREC,AREC2          SET TO READ INTO DUMMY AREA                  
         GOTO1 GETREC                                                           
* NOW RESTORE ORIGINAL I/O ADDRESS                                              
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
*                                                                               
BCX2     GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 CALLDSP                                                          
*                                                                               
         C     R2,FLAST            TEST DATA IN INPUT AREA                      
         BNH   *+12                YES                                          
         LA    R2,BUYINP1H                                                      
         MVI   BUYINP2H+5,0        SUPPRESS FURTHER INPUT                       
*                                                                               
BCX4     MVC   ELEM(L'BUYINP1),8(R2)                                            
         MVI   8(R2),C'*'                                                       
         MVC   9(L'BUYINP1-1,R2),ELEM                                           
         FOUT  (R2)                                                             
*                                                                               
         CLI   BUYMSG,0                                                         
         BNE   *+10                                                             
         MVC   BUYMSG(22),=C'** ACTION COMPLETED **'                            
         B     EXIT                                                             
         EJECT                                                                  
* AFFIDS                                                                        
*                                                                               
B700     CLC   =C'I,DEL=',0(R4)                                                 
         BNE   B702                                                             
         MVI   BYTE,C'D'           FLAG THIS IS DELETE BY DATE                  
         AHI   R4,6                POINT TO ELEM DATE                           
         B     B704                                                             
*                                                                               
B702     CLC   =C'I,DEL',0(R4)                                                  
         BE    B740                                                             
*                                                                               
         MVI   ERRCD,NORCLINV                                                   
         CLI   SVRCLOPT,RCLINV                                                  
         BE    B704                                                             
         MVI   ERRCD,NORCLRSV                                                   
         CLI   SVRCLOPT,RCLRSVP                                                 
         BNE   BUYERR                                                           
*                                                                               
B704     XC    STDTP,STDTP                                                      
         MVC   ENDDTP,=X'FFFF'                                                  
*                                                                               
B705     CLI   BYTE,C'D'                                                        
         BE    B708C                                                            
         MVI   ERRCD,TRCDERR                                                    
         LA    R0,BUYINP1H         MUST BE ON INPUT LINE 1                      
         CR    R2,R0                                                            
         BNE   BUYERR                                                           
*                                                                               
B707     LA    R2,BUYOUTH                                                       
*                                                                               
* FIND AN-UNP (AFD) FIELD AND PRECEDING DATE FIELD                              
*                                                                               
B708     CLI   BYTE,C'D'           THIS CODE SKIPPED FIRST TIME FOR D           
         BNE   B708A                                                            
         CLI   FSTOP,C','                                                       
         BNE   B730                                                             
         L     R4,FADDR            R4 NOW POINTS TO NEXT FIELD                  
         AHI   R4,1                                                             
         B     B708C                                                            
*                                                                               
B708A    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    B730                                                             
         CLI   0(R2),9                                                          
         BNH   B708                                                             
         TM    1(R2),X'20'         TEST PROT                                    
         BZ    B708                NO                                           
         CLI   0(R2),25            DATE FIELD IS MAX 15 (+ FLDHDR)              
         BH    B708                SO IF MORE THIS ISNT IT                      
         ST    R2,WORK2            SAVE PROT FIELD ADDR                         
*                                                                               
         CLI   BUYKEY+3,X'FF'                                                   
         BNE   *+10                                                             
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO PRD FIELD                           
         IC    R0,0(R2)            (AND SKIP IT)                                
         AR    R2,R0               POINT TO AFFID INPUT                         
         CLI   0(R2),19            TEST FIELD LEN = 21                          
         BE    B708B                                                            
         CLI   0(R2),26            RSVP DATA LEN                                
         BE    B708B                                                            
*                                                                               
B708B    ST    R2,WORK2+4          SAVE UNP FIELD ADDR                          
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    B708                NO-IGNORE                                    
         CLI   5(R2),0             TEST NO INPUT                                
         BE    B708                YES-IGNORE                                   
* EDIT ELEMENT DATE (PROTECTED)                                                 
         L     R2,WORK2                                                         
         LA    R4,8(R2)                                                         
         CLI   0(R4),C'+'                                                       
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
*                                                                               
B708C    ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         OC    STDTP,STDTP         SAVE EARLIEST START DATE                     
         BNZ   *+10                                                             
         MVC   STDTP,BUELDT                                                     
         CLC   STDTP,BUELDT                                                     
         BNH   *+10                                                             
         MVC   STDTP,BUELDT                                                     
* SAVE EBCDIC DATE/DATE-30/DATE+60/                                             
         GOTO1 VDATCON,DMCB,(2,BUELDT),WORK+18                                  
         LA    R0,30                                                            
         CLC   =C'RSV',BUTRCODE                                                 
         BNE   *+8                                                              
         LA    R0,1                                                             
         LCR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,WORK+18,WORK+24,(R0)   GET DATE -30                  
         LA    R0,60                                                            
         CLC   =C'RSV',BUTRCODE                                                 
         BNE   *+8                                                              
         LA    R0,7                                                             
         GOTO1 (RF),(R1),,WORK+30,(R0)            AND +60                       
* EDIT AFFID DATE                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'10'                                                       
         MVI   ELEM+1,6                                                         
*                                                                               
         CLI   BYTE,C'D'                                                        
         BE    B715                                                             
         L     R2,WORK2+4          GET UNP FIELD ADDRESS                        
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         CLC   =C'DEL',0(R4)       TEST DELETE                                  
         BE    B715                                                             
         CLC   =C'**NO',0(R4)      TEST RSVP '** NO AFFID ' DISPLAY             
         BNE   B708D                                                            
         GOTO1 FLDVAL              SKIP PAST 'NO AFFID'                         
         GOTO1 FLDVAL              SKIP PAST DAYPART                            
         L     R4,FADDR            SKIP PAST DELIMITER ON BLANK FIELD           
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         B     B714A               GO EDIT RESPONSES                            
*                                                                               
B708D    OC    SVFNPROF,SVFNPROF   TEST FILM TYPE PROFILE SAVED                 
         BZ    B709C               NO                                           
         CLI   SVFNPROF,X'FF'      TEST NOT FOUND                               
         BE    B709C                                                            
         CLC   SVENDB,SVFNPROF+13  COMPARE EST END TO PROF START                
         BL    B709C                                                            
* TEST PROFILE APPLIES TO THIS BRAND                                            
         CLI   BUYKEY+3,X'FF'      TEST POL                                     
         BNE   B709C                                                            
         CLI   BUELNUM,0                                                        
         BNE   *+8                                                              
         MVI   BUELNUM,1                                                        
         BAS   RE,FNDEL                                                         
         CLI   1(R6),14             IGNORE IF UNALLOCATED                       
         BL    B709C                                                            
* FIND PRD CODE IN CLIST                                                        
         L     RE,ASVCLIST                                                      
B709PRD  CLC   3(1,RE),10(R6)                                                   
         BE    B709PRDX                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   B709PRD                                                          
         DC    H'0'                                                             
B709PRDX CLC   SVFNPROF+10(3),0(RE)                                             
         BNE   B709C                                                            
         SPACE 1                                                                
* EDIT FILM TYPE CODE *                                                         
         SPACE 1                                                                
         MVI   ERRCD,BADFLMTY                                                   
         CLI   0(R4),C'A'                                                       
         BL    BUYERR                                                           
         CLI   0(R4),C'Z'                                                       
         BH    BUYERR                                                           
         CLI   1(R4),C'/'                                                       
         BNE   BUYERR                                                           
*                                                                               
         LA    R0,10                                                            
         LA    R1,SVFNPROF                                                      
B709A    CLC   0(1,R1),0(R4)                                                    
         BE    B709B                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,B709A                                                         
         B     BUYERR                                                           
*                                                                               
B709B    LA    RE,11                                                            
         SR    RE,R0                                                            
         SLL   RE,4                                                             
         STC   RE,ELEM+4                                                        
         LA    R4,2(R4)                                                         
         ST    R4,FADDR                                                         
*                                                                               
B709C    XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C'-'                                                      
         GOTO1 FLDVAL                                                           
         TM    FVAL,X'08'          TEST NUMERIC (DAY WITHOUT MONTH)             
         BO    B710                                                             
* EDIT MMMDD                                                                    
         GOTO1 VDATVAL,DMCB,(1,(R4)),WORK                                       
         MVI   ERRCD,INVDATE                                                    
         OC    0(4,R1),0(R1)                                                    
         BZ    BUYERR                                                           
         MVC   WORK(2),WORK+24                                                  
         CLC   WORK+24(2),WORK+30  IS 90 DAY PERIOD IN ONE YEAR                 
         BE    B709X                                                            
         CLC   WORK+2(4),WORK+26   INPUT MMDD ELEM DATE -30 MMDD                
         BH    B709X               IF HI USE START YEAR                         
         MVC   WORK(2),WORK+30     ELSE SET END YEAR                            
B709X    CLC   WORK(6),WORK+24                                                  
         BL    BUYERR                                                           
         CLC   WORK(6),WORK+30                                                  
         BH    BUYERR                                                           
         B     B714                                                             
*                                                                               
* EDIT DAY ONLY - MUST BE WITHIN ELEM DT AND ELEM DT +7                         
*                                                                               
B710     MVC   WORK(6),WORK+18                                                  
         GOTO1 VADDAY,DMCB,WORK,WORK+6,6                                        
         PACK  HALF,WORK+4(2)                                                   
         PACK  HALF2,WORK+10(2)                                                 
         MVI   ERRCD,INVDATE                                                    
         CP    HALF,HALF2          TEST WEEK ENDS IN SAME MONTH                 
         BNE   B712                NO                                           
         CP    HALF,DUB            FLDVAL PUTS NUM FLD IN DUB                   
         BH    BUYERR                                                           
         CP    HALF2,DUB                                                        
         BL    BUYERR                                                           
         UNPK  WORK+4(2),DUB                                                    
         B     B714                                                             
* WEEK ENDS IN DIFFERENT MONTH - TRY 6 TIMES TO MATCH DATE                      
B712     CP    HALF,DUB                                                         
         BE    B714                                                             
         GOTO1 VADDAY,DMCB,WORK,WORK+12,1                                       
         MVC   WORK(6),WORK+12                                                  
         PACK  HALF,WORK+4(2)      PACK DAY                                     
         CLC   WORK(6),WORK+6      TEST STILL IN WEEK                           
         BNH   B712                                                             
         B     BUYERR                                                           
*                                                                               
B714     GOTO1 VDATCON,DMCB,WORK,(2,ELEM+2)                                     
*                                                                               
* EDIT TIME                                                                     
*                                                                               
         CLC   =C'RSV',BUTRCODE                                                 
         BNE   *+8                                                              
         MVI   FSTOPS,C','                                                      
         MVI   EDTVAL,TIMEDT                                                    
         GOTO1 CALLEDT                                                          
         MVI   ERRCD,TIMERR                                                     
         OC    BUTIME+2(2),BUTIME+2  SHOULD HAVE 1 TIME ONLY                    
         BNZ   BUYERR                                                           
         OC    ELEM+4(2),BUTIME    PRESERVE FILM TYPE IF PRESENT                
*                                                                               
         CLC   =C'RSV',BUTRCODE                                                 
         BNE   B715                                                             
* EDIT ACTUAL DAYPART                                                           
         MVI   EDTVAL,DPTEDT                                                    
         GOTO1 CALLEDT                                                          
* EDIT NUMBER OF RESPONSES (MAY BE FOLLOWED BY A + SIGN IF X'40'                
*      STATUS BIT IS ON WHICH MEANS DATA IS RESPONSES NOT AFFID)                
*      THIS FEATURE NOT AVAILABLE IF FILM TYPES INPUT                           
B714A    OC    SVFNPROF,SVFNPROF                                                
         BZ    *+12                                                             
         CLI   SVFNPROF,X'FF'     MAKE SURE NO FN PROFILE                       
         BNE   *+8                                                              
         MVI   FSTOPS,C'+'         STOP ON + SO IT IS IGNORED                   
         MVI   ERRCD,NPWERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         TM    FVAL,X'08'                                                       
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         STH   R0,BUDEM                                                         
         XC    ELEM+10(6),ELEM+10                                               
         MVI   ELEM+10,X'17'                                                    
         MVI   ELEM+11,6                                                        
         MVC   ELEM+12(1),BUDPT                                                 
         MVC   ELEM+14(2),BUDEM                                                 
         CLI   FSTOP,C'+'          TEST STOPPED ON + SIGN                       
         BNE   *+8                 YES                                          
         OI    ELEM+4,X'40'        SET RESPONSE DATA FLAG                       
*                                                                               
B715     CLI   BUYKEY+3,X'FF'                                                   
         BNE   B720                                                             
* POL AFFID                                                                     
         CLI   BUELNUM,0                                                        
         BNE   *+8                                                              
         MVI   BUELNUM,1                                                        
         BAS   RE,FNDEL                                                         
*                                                                               
         OC    ELEM+2(2),ELEM+2    IF NO DATE, THIS IS DELETE                   
         BNZ   B715A                                                            
         L     R4,=A(SVB0PROF-BUYSAVE)                                          
         AR    R4,RA                                                            
         CLI   4(R4),C'N'          TEST ALLOW DEL IF PAID                       
         BNE   B715A               ASSUME YES UNLESS EXPLICIT NO                
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DELPDINV)                                              
         OC    4(2,R6),4(R6)       TEST SPOT PAID                               
         BNZ   BUYERR                                                           
*                                                                               
B715A    SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         AR    R6,R0                                                            
*                                                                               
B715B    CLI   0(R6),X'10'                                                      
         BNE   *+12                                                             
         BAS   RE,DELEL                                                         
         B     B715B                                                            
*                                                                               
         CLI   0(R6),X'17'         TEST RSVP ELEM                               
         BNE   *+12                                                             
         BAS   RE,DELEL                                                         
         B     B715B                                                            
*                                                                               
         CLI   0(R6),X'10'         IF ELCODE < X'10' DONE                       
         BL    B715X                                                            
         CLI   0(R6),X'20'         IF ELCODE < X'20' CONTINUE                   
         BL    B715A                                                            
*                                                                               
B715X    OC    ELEM+2(2),ELEM+2    TEST DELETE                                  
         BNE   B716                NO                                           
         OC    ELEM+10(2),ELEM+10  TEST TO ADD COUNT ONLY                       
         BNZ   B716A                                                            
         B     B708                                                             
*                                                                               
B716     BAS   RE,FNDEL            LOCATE REGEL (AGAIN)                         
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               ADD ELEMENT AFTER IT !                       
         BAS   RE,ADDEL                                                         
*                                                                               
B716A    CLC   =C'RSV',BUTRCODE                                                 
         BNE   B708                                                             
         MVC   ELEM(6),ELEM+10                                                  
* MAKE SURE TO INSERT AFTER ANY OTHER ELEM FOR THIS SPOT                        
B716B    CLI   0(R6),X'10'                                                      
         BL    B716C                                                            
         CLI   0(R6),X'1F'                                                      
         BH    B716C                                                            
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     B716B                                                            
*                                                                               
B716C    BAS   RE,ADDEL                                                         
         B     B708                                                             
*                                                                               
* NON-POL AFFID                                                                 
*                                                                               
B720     MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
         MVI   ERRCD,BADSPOT                                                    
         SR    R8,R8                                                            
         LA    R6,BDELEM                                                        
B722     BAS   RE,NEXTEL                                                        
         BNE   BUYERR                                                           
         CLC   BUELDT,2(R6)                                                     
         BH    B722                                                             
         BL    BUYERR                                                           
B724     ZIC   R0,7(R6)                                                         
         TM    6(R6),X'80'                                                      
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AR    R8,R0                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    B725                                                             
         CLC   0(1,R6),ELCDHI                                                   
         BH    *+14                                                             
         CLC   BUELDT,2(R6)                                                     
         BE    B724                                                             
*                                                                               
B725     CLI   BUELNUM,0                                                        
         BNE   *+8                                                              
         MVI   BUELNUM,1                                                        
         IC    R0,BUELNUM                                                       
         CR    R8,R0                                                            
         BL    BUYERR                                                           
* COUNT TO THIS AFFID                                                           
         LR    R8,R0                                                            
         B     B726X                                                            
B726     IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
B726X    CLI   0(R6),X'10'                                                      
         BNE   B728                                                             
         BCT   R8,B726                                                          
*                                                                               
         BAS   RE,DELEL                                                         
         OC    ELEM+2(4),ELEM+2    TEST DELETE                                  
         BZ    B708                YES - DONE                                   
         B     B728X                                                            
*                                                                               
B728     OC    ELEM+2(4),ELEM+2    TEST DELETE                                  
         BZ    BUYERR              YES - ERROR - NO AFFID                       
B728X    BAS   RE,ADDEL                                                         
*                                                                               
         B     B708                                                             
         SPACE 2                                                                
*                                                                               
B730     MVI   BUWHY2,X'80'                                                     
         MVI   RCLOPT,RCLINV                                                    
         CLC   =C'RSV',BUTRCODE                                                 
         BNE   *+8                                                              
         MVI   RCLOPT,RCLRSVP                                                   
         B     BCX                                                              
         EJECT                                                                  
* INPUT IS 'I,DEL' - DELETE ALL AFFIDS                                          
*                                                                               
B740     L     R4,=A(SVB0PROF-BUYSAVE)                                          
         AR    R4,RA                                                            
         CLI   4(R4),C'N'          TEST ALLOW DEL IF PAID                       
         BNE   B745                ASSUME YES UNLESS EXPLICIT NO                
* SEE IF THERE ARE ANY PAID SPOTS                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
B742     BAS   RE,NEXTEL                                                        
         BNE   B745                                                             
         OC    4(2,R6),4(R6)                                                    
         BZ    B742                                                             
         OC    4(2,R6),4(R6)       TEST SPOT PAID                               
         BZ    B742                                                             
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DELPDINV)                                              
         B     BUYERR                                                           
         SPACE 1                                                                
*=============================================================                  
* NOW DELETE ALL AFFIDS                                                         
*=============================================================                  
         SPACE 1                                                                
B745     MVI   ELCDLO,X'10'                                                     
         MVI   ELCDHI,X'10'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   B730                                                             
*                                                                               
B748     BAS   RE,DELEL                                                         
         CLI   0(R6),X'17'         TEST RSVP ELEM                               
         BNE   *+8                                                              
         BAS   RE,DELEL                                                         
         BAS   RE,NEXTEL2                                                       
         BNE   B730                                                             
         B     B748                                                             
         EJECT                                                                  
* INTG/FLM/NTCD EDITS                                                           
*                                                                               
*                                                                               
B805     MVI   ERRCD,NORCLFLM      FILM NUM                                     
         CLI   SVRCLOPT,RCLFLM                                                  
         BNE   BUYERR                                                           
*                                                                               
B806     LA    R2,BUYOUTH                                                       
         XC    STDTP,STDTP                                                      
         MVC   ENDDTP,=X'FFFF'                                                  
*                                                                               
* FIND AN UNP FIELD AND PRECEDING DATE FIELD                                    
*                                                                               
B808     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    B830                                                             
         CLI   0(R2),9                                                          
         BE    B830                                                             
         TM    1(R2),X'20'         TEST PROT                                    
         BZ    B808                NO                                           
         CLI   0(R2),25            DATE IS MAX 15 (+ FLDHDR)                    
         BH    B808                SO IF HI THIS ISNT IT                        
         ST    R2,WORK2            SAVE FIELD ADDRESS                           
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               SKIP PRD FIELD                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO INTG INPUT                          
         ST    R2,WORK2+4          SAVE UNP FIELD ADDRESS                       
         TM    1(R2),X'20'         TEST PROT                                    
         BO    B808                YES-SKIP                                     
*                                                                               
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    B808                NO-IGNORE                                    
         CLI   5(R2),0             TEST NO INPUT                                
         BE    B808                YES-IGNORE                                   
* EDIT ELEMENT DATE                                                             
         L     R2,WORK2                                                         
         LA    R4,8(R2)                                                         
         CLI   0(R4),C'+'                                                       
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         OC    STDTP,STDTP         SAVE EARLIEST START DATE                     
         BNZ   *+10                                                             
         MVC   STDTP,BUELDT                                                     
         CLC   STDTP,BUELDT                                                     
         BNH   *+10                                                             
         MVC   STDTP,BUELDT                                                     
         L     R2,WORK2+4                                                       
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
*                                                                               
         CLI   SVRCLOPT,RCLFLM                                                  
         BE    B840                                                             
*                                                                               
B830     MVC   RCLOPT,SVRCLOPT                                                  
         MVI   BUWHY2,X'20'                                                     
         CLI   RCLOPT,RCLINT                                                    
         BE    B830X                                                            
         CLI   RCLOPT,RCLFLM                                                    
         BE    B830X                                                            
         MVI   BUWHY2,X'10'        PCD CHG                                      
B830X    B     BCX                                                              
         EJECT                                                                  
* EDIT FILM NUMBER(S)                                                           
*                                                                               
B840     MVI   ERRCD,INVERR                                                     
         CLC   =C'DEL',8(R2)                                                    
         BNE   B842                                                             
         BAS   RE,FNDEL                                                         
B841     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'10'                                                      
         BE    B841                                                             
         CLI   0(R6),X'11'                                                      
         BE    B841                                                             
         CLI   0(R6),X'12'                                                      
         BNE   BUYERR                                                           
         BAS   RE,DELEL                                                         
         B     B808                                                             
*                                                                               
B842     XC    ELEM,ELEM                                                        
         MVI   ELEM,X'12'                                                       
         MVI   ELEM+1,3                                                         
*                                                                               
         MVC   FSTOPS(2),=C'/-'                                                 
         SPACE 1                                                                
         EJECT                                                                  
*======================================*                                        
* EDIT FILM NUMBERS FOR TRAFFIC SYSTEM *                                        
*======================================*                                        
         SPACE 1                                                                
BNTR     DS    0H                                                               
         MVI   UPDSW,C'Y'          INDICATE GETREC REQUIRED ON EXIT             
         XC    BUPROG,BUPROG       CLEAR FILM SAVE AREA                         
*                                                                               
BNTR2    GOTO1 FLDVAL                                                           
         CLI   FLEN+1,8                                                         
         BNE   BUYERR                                                           
         MVC   BUPROG(8),0(R4)                                                  
*                                                                               
         MVI   FSTOPS+1,0          RESET P/B STOP CHAR                          
         CLI   FSTOP,C'-'                                                       
         BNE   BNTR4                                                            
*                                                                               
         GOTO1 FLDVAL                                                           
         CLI   FLEN+1,8                                                         
         BNE   BUYERR                                                           
         MVC   BUPROG+8(8),0(R4)                                                
*                                                                               
BNTR4    CLI   FSTOP,C'/'          TEST DAY PRESENT                             
         BNE   BNTR6                                                            
* VALIDATE DAY                                                                  
         MVI   FSTOPS,0                                                         
         MVI   EDTVAL,DAYEDT                                                    
         GOTO1 CALLEDT                                                          
* MAKE SURE ONLY ONE DAY ENTERED                                                
         MVI   ERRCD,DAYERR                                                     
         ZIC   RE,BUDAYS                                                        
         SR    RF,RF                                                            
         SRDL  RE,1                                                             
         LTR   RF,RF                                                            
         BZ    *-6                                                              
         LTR   RE,RE                                                            
         BNZ   BUYERR                                                           
         MVC   ELEM+2(1),BUDAYS    SET DAY IN ELEMENT                           
         IC    RE,BUDAYS           TEST DAY WITHIN BUY DESC DAYS                
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    BDDAY,0 *EXECUTED*                                               
         BZ    BUYERR                                                           
         EJECT                                                                  
* MAKE SURE SAME NUMBER OF FILMS AS ALLOCATIONS *                               
         SPACE 1                                                                
BNTR6    MVI   ERRCD,FLMPRERR                                                   
         BAS   RE,FNDEL            FIND THE ELEMENT                             
         CLI   1(R6),10            TEST UNALL                                   
         BE    BUYERR                                                           
         CLI   1(R6),14            TEST ONE PRD                                 
         BNE   BNTR10                                                           
         CLI   BUPROG+8,0          TEST TWO FILMS                               
         BNE   BUYERR              YES - ERROR                                  
         B     BNTR12                                                           
*                                                                               
BNTR10   CLI   BUPROG+8,0          TEST TWO FILMS                               
         BE    BUYERR              NO - ERROR                                   
         SPACE 1                                                                
* READ FOR FILM SEQUENCE NUMBERS *                                              
         SPACE 1                                                                
BNTR12   LA    R7,10(R6)           POINT TO PRD1                                
         LA    R1,BUPROG           POINT TO FILM                                
         BAS   RE,NTRGET           READ RECORD AND TEST VALID PRD/SLN           
         MVI   ELEM+1,5            SET ELEM LEN                                 
         MVC   ELEM+3(2),HALF      MOVE FILM SEQUENCE NUMBER                    
*                                                                               
         CLI   BUPROG+8,0          TEST PIGGYBACK                               
         BE    BNTR14              NO                                           
         LA    R7,4(R7)            POINT TO NEXT ALLOC                          
         LA    R1,BUPROG+8         POINT TO FILM                                
         BAS   RE,NTRGET                                                        
         MVI   ELEM+1,7            SET ELEM LEN                                 
         MVC   ELEM+5(2),HALF      AND FILM SEQ                                 
*                                                                               
BNTR14   DS    0H                  INSERT ELEM IN RECORD                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'10'         TEST AFFID                                   
         BE    BNTR14                                                           
         CLI   0(R6),X'12'         TEST FLM                                     
         BNE   BNTR16                                                           
BNTR15   DS    0H                  DELETE OLD FILM ELEM                         
         BAS   RE,DELEL                                                         
         CLC   =C'DEL',8(R2)                                                    
         BE    BNTR15                                                           
BNTR16   DS    0H                  ADD NEW FILM ELEM                            
         BAS   RE,ADDEL                                                         
*                                                                               
         B     B808                                                             
         EJECT                                                                  
*================================================*                              
* SUBROUTINE TO READ CMML RECORD FOR NEW TRAFFIC *                              
* TESTS THAT CMML IS VALID FOR THIS PRODUCT/SLN  *                              
*================================================*                              
         SPACE 1                                                                
NTRGET   NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'                                                        
         MVI   KEY+1,X'21'                                                      
         MVC   KEY+2(3),SVKEY      A-M/C                                        
         MVC   KEY+5(8),0(R1)      CMML                                         
*                                                                               
         OC    SVMCLCOD,SVMCLCOD   TEST SPECIAL TRF CLT                         
         BZ    *+10                                                             
         MVC   KEY+3(2),SVMCLCOD   USE SPCL CLT CODE                            
*                                                                               
         L     R8,AREC2                                                         
         CLC   KEY,0(R8)           TEST SAME FILM AS PREVIOUS                   
         BE    NTRGET10                                                         
*                                                                               
***      GOTO1 HIGH                                                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'TRFDIR',KEY,KEY                      
         MVI   ERRCD,NOFILM                                                     
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
*                                                                               
         ST    R8,AREC             SET I/O ADDRESS                              
***      GOTO1 GETREC                                                           
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'TRFFILE',KEY+14,AREC,DMWORK          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NTRGET10 DS    0H                                                               
*                                                                               
         MVI   BYTE,X'10'                                                       
         BAS   RE,NTREL                                                         
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
         CLC   CMLSLN,1(R7)        MATCH SLN                                    
         BNE   BUYERR                                                           
         MVC   HALF(2),CMLSEQ+1    SAVE LAST 2 BYTES OF SEQNUM                  
*                                                                               
         MVI   BYTE,X'20'          GET PRODUCT LIST ELEMENT                     
         BAS   RE,NTREL                                                         
*                                                                               
         USING CMLPRDEL,R6                                                      
*                                                                               
         ZIC   R0,1(R6)                                                         
         SH    R0,=H'2'                                                         
         BNP   BUYERR                                                           
         LA    R6,2(R6)                                                         
         CLI   SVMCLPRD,0          TEST SPCL PRD OVERRIDE                       
         BE    *+8                 NO                                           
         LA    R7,SVMCLPRD         POINT TO IT                                  
*                                                                               
NTRGET12 CLC   0(1,R7),0(R6)       MATCH ALLOC TO PRDLIST EL                    
         BE    NTRGETX                                                          
         LA    R6,1(R6)                                                         
         BCT   R0,NTRGET12                                                      
*                                                                               
NTRGETX  MVC   AREC,AREC1          RESTORE ORIGINAL I/O ADDRESS                 
         B     EXIT                                                             
         EJECT                                                                  
NTREL    LA    R6,24(R8)           POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
NTREL2   CLC   BYTE,0(R6)          MATCH EL CODE                                
         BER   RE                                                               
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         B     NTREL2                                                           
         EJECT                                                                  
*=============================================*                                 
* UPDATE STATUS RECORD FOR 'COMPLETE' COMMAND *                                 
*=============================================*                                 
         SPACE 1                                                                
COMP     DS    0H                                                               
         MVI   ERRCD,NORECS                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY       MOVE A-M/CLT/PRD/MKT/STA/EST                 
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   BUYERR                                                           
*                                                                               
         XC    FULL,FULL           CLEAR TEMP SAVE AREA                         
         MVC   FULL+2(2),AGYALPHA                                               
*                                                                               
         CLI   SVCXTRA+2,C'*'      TEST CCUSA INTERFACE                         
**NOP**  BNE   COMP10              THIS FLAG ISN'T ALWAYS ON                    
         CLC   =C'CC ',QCLT                                                     
         BNE   COMP10                                                           
         CLC   =C'POL',QPRD        MUST SPECIFY REAL BRAND FOR CCUSA            
         BE    COMP3                                                            
         SPACE 1                                                                
*==========================================================*                    
* READ CONTROL FILE FOR CCUSA SPOT SYSTEM ASSIGNMENT       *                    
*==========================================================*                    
         SPACE 1                                                                
         MVC   FULL+2(2),=C'CC'    FORCE AGYALPHA                               
*                                                                               
         CLC   T211FFD+10(2),=AL2(797)   TEST SIGN ON = CCUSA                   
         BNE   *+12                                                             
         MVI   ERRCD,TRCDERR       INVALID                                      
         B     BUYERR                                                           
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'                                                        
         MVC   WORK+15(10),=CL10'CCUSA     '                                    
         MVC   AREC,AREC1                                                       
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AREC                                                          
         CLC   WORK(25),0(R6)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCDLO,X'21'                                                     
         MVI   ELCDHI,X'21'                                                     
         LA    R6,28(R6)                                                        
         BAS   RE,NEXTEL2                                                       
         B     *+8                                                              
COMP2    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R6),X'02'         TEST SPOT ELEMENT                            
         BNE   COMP2                                                            
         MVC   FULL(2),3(R6)       SAVE SYSTEM AND AGENCY CODE                  
         SPACE 1                                                                
*====================================================*                          
* INITIATE FASWITCH                                                             
*====================================================*                          
         SPACE 1                                                                
         L     RF,VCOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),FULL        MOVE SYSTEM NUMBER TO PARAM LIST             
         GOTO1 (RF),DMCB                                                        
         MVI   ERRCD,NOCCUSA                                                    
         CLI   4(R1),2                                                          
         BE    BUYERR                                                           
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*=============================================================*                 
* NOW READ PRODUCT HEADER ON CCUSA TO GET BINARY PRODUCT CODE *                 
*=============================================================*                 
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY      MOVE A-M/CLT                                 
         NI    KEY+1,X'0F'         DROP AGY                                     
         OC    KEY+1(1),FULL+1     'OR' IN CCUSA AGY CODE                       
         MVC   KEY+4(3),QPRD       EBCDIC PRODUCT CODE                          
         GOTO1 HIGH                                                             
         MVI   ERRCD,PRDERR                                                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    COMP4                                                            
*                                                                               
COMP3    LA    R2,BUYPRH           MUST NOT BE A CC PRODUCT                     
         MVI   ERRCD,INVERR                                                     
         B     BUYERR                                                           
*                                                                               
COMP4    DS    0H                                                               
         GOTO1 GETREC                                                           
         SPACE 1                                                                
*===================================================*                           
* NOW COMPLETE KEY                                  *                           
*===================================================*                           
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STATD,R4                                                         
         MVC   STKTYPE,=X'0D71'                                                 
         MVC   STKAGMD(3),KEYSAVE+1   MOVE A-M/CLT FROM PRDHDR KEY              
         L     RE,AREC                                                          
         MVC   STKPRD,(PCODE+1-PRDHDRD)(RE)                                     
         MVC   STKEST,SVKEY+9                                                   
         MVC   STKMKT(5),SVKEY+4   MKT/STA                                      
         B     COMP12                                                           
         EJECT                                                                  
COMP10   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STATD,R4                                                         
         MVC   STKTYPE,=X'0D71'                                                 
         MVC   STKAGMD(4),SVKEY    A-M/CLT/PRD                                  
         MVC   STKEST,SVKEY+9      EST                                          
         MVC   STKMKT(5),SVKEY+4   MKT/STA                                      
         SPACE 1                                                                
*===========================================================*                   
* NEED TO READ ST PROFILE TO DETERMINE RECORDING LEVEL      *                   
*===========================================================*                   
         SPACE 1                                                                
COMP12   XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0ST'                                                 
         MVC   WORK+4(2),FULL+2                                                 
         MVC   WORK+6(1),BUYMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),T211FFD+1                                             
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,WORK+24,VDATAMGR                                  
         MVI   ERRCD,NOSTPROF                                                   
         CLI   WORK+24,0                                                        
         BE    BUYERR                                                           
*                                                                               
         LA    R1,WORK+24                                                       
         CLI   0(R1),C'P'          TEST STATUS BY PRODUCT                       
         BE    *+8                                                              
         MVI   STKPRD,0            SUPPRESS PRODUCT                             
*                                                                               
         CLI   2(R1),C'E'          TEST STATUS BY ESTIMATE                      
         BE    *+8                                                              
         MVI   STKEST,0            SUPPRESS ESTIMATE                            
*                                                                               
         CLI   1(R1),C'S'          TEST STATUS BY STATION                       
         BE    *+10                                                             
         XC    STKSTA,STKSTA       SUPPRESS STATION                             
*                                                                               
         GOTO1 HIGH                READ FOR STATUS RECORD                       
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    COMP20                                                           
         EJECT                                                                  
*=====================================================*                         
* BUILD NEW RECORD                                    *                         
*=====================================================*                         
         SPACE 1                                                                
         L     R4,AREC                                                          
         XC    0(256,R4),0(R4)                                                  
         MVC   0(13,R4),KEYSAVE                                                 
         LA    R0,24+BPLENQ+CSLENQ                                              
         STCM  R0,3,13(R4)         SET REC LEN                                  
         MVC   20(2,R4),FULL+2     MOVE FROM TEMP AREA                          
         MVI   BPCODE,BPCODEQ                                                   
         MVI   BPLEN,BPLENQ                                                     
         MVI   CSCODE,CSCODEQ                                                   
         MVI   CSLEN,CSLENQ                                                     
         MVC   CSAGYA,AGYALPHA                                                  
         GOTO1 VDATCON,DMCB,(5,0),(2,CSBUY)                                     
         GOTO1 ADDREC                                                           
         B     COMPX                                                            
         DROP  R4                                                               
         EJECT                                                                  
COMP20   DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AREC                                                          
         USING STATD,R6                                                         
*                                                                               
         MVI   ELCDLO,CSCODEQ                                                   
         MVI   ELCDHI,CSCODEQ                                                   
         LA    R6,BPELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    COMP22                                                           
* CREATE NEW STATUS ELEMENT                                                     
         MVI   0(R6),CSCODEQ                                                    
         MVI   1(R6),CSLENQ                                                     
         L     RE,AREC                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)         GET OLD LENGTH                               
         LA    RF,CSLENQ(RF)                                                    
         STCM  RF,3,13(RE)         SET NEW LENGTH                               
*                                                                               
COMP22   LA    R6,(CSBUY-CSELEM)(R6)                                            
         GOTO1 VDATCON,DMCB,(5,0),(2,(R6))                                      
         MVC   CSAGYA,AGYALPHA                                                  
         GOTO1 PUTREC                                                           
*                                                                               
COMPX    B     BCX4                GO SEND ACTION COMPLETED MSG                 
         EJECT                                                                  
FNDEL    LR    R8,RE               SAVE CALLING REG                             
         MVI   ERRCD,BADSPOT                                                    
         XC    ELEMDT,ELEMDT                                                    
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
         CLI   BUELNUM,0                                                        
         BNE   *+8                                                              
         MVI   BUELNUM,1                                                        
FNDEL2   BAS   RE,NEXTEL                                                        
         BNE   BUYERR                                                           
         CLC   ELEMDT,2(R6)                                                     
         BE    *+8                                                              
         MVI   ELEMNO,0                                                         
         MVC   ELEMDT,2(R6)                                                     
         IC    RE,ELEMNO                                                        
         TM    6(R6),X'80'                                                      
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,ELEMNO                                                        
         CLC   ELEMDT,BUELDT                                                    
         BNE   FNDEL2                                                           
         CLC   ELEMNO,BUELNUM                                                   
         BNE   FNDEL2                                                           
         LR    RE,R8                                                            
         BR    RE                                                               
         SPACE 2                                                                
DELEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         B     EXIT                                                             
*                                                                               
ADDEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         B     EXIT                                                             
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
* TEST SPOT AT 0(R6), BILLED OR PAID                                            
TESTBP   NTR1                                                                   
         MVI   ERRCD,BLLDPAID                                                   
*                                                                               
         OC    4(2,R6),4(R6)                                                    
         BNZ   NEQXIT                                                           
*                                                                               
         CLI   0(R6),X'0B'                                                      
         BL    TBP10                                                            
* POL TESTS                                                                     
         ZIC   R0,1(R6)                                                         
         SH    R0,=H'10'                                                        
         BZ    EQXIT                                                            
         SRL   R0,2                                                             
         LA    R7,10(R6)                                                        
TBP2     OC    2(2,R7),2(R7)                                                    
         BNZ   NEQXIT                                                           
         LA    R7,4(R7)                                                         
         BCT   R0,TBP2                                                          
         B     EQXIT                                                            
* NON-POL TESTS                                                                 
*                                                                               
TBP10    ZIC   R0,1(R6)                                                         
         SH    R0,=H'8'                                                         
         SRL   R0,1                                                             
         LA    R7,8(R6)                                                         
TBP12    OC    0(2,R7),0(R7)                                                    
         BNZ   NEQXIT                                                           
         LA    R7,2(R7)                                                         
         BCT   R0,TBP12                                                         
         B     EQXIT                                                            
         EJECT                                                                  
* TEST FOR AFFIDS FOR ELEM AT 0(R6).                                            
* ASSUME ELCDLO AND ELCDHI CONTAIN REGEL ARGUMENTS                              
*                                                                               
TESTMTCH NTR1                                                                   
         MVI   ERRCD,MATCHED                                                    
*                                                                               
         LR    R5,R6               SAVE EL ADDRESS                              
TMTCH2   BAS   RE,NEXTEL                                                        
         BNE   TMTCH4                                                           
         CLC   2(2,R5),2(R6)       TEST SAME DATE                               
         BE    TMTCH2                                                           
TMTCH4   LR    R7,R6               R7 IS END OF SRCH                            
         BCTR  R7,0                                                             
TMTCH6   CLI   0(R5),X'10'         TEST AFFID                                   
         BE    NEQXIT              EXIT WITH CC NOT EQ IF MATCHED               
         ZIC   R6,1(R5)                                                         
         BXLE  R5,R6,TMTCH6                                                     
         B     EQXIT                                                            
         SPACE 2                                                                
* MAKE SURE COMMENTS LAST ELEMS IN RECORD                                       
*                                                                               
CMTFIX   NTR1                                                                   
*                                                                               
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC2      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'FF'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                NO COMMENTS                                  
*                                                                               
CMTF2    BAS   RE,DELEL                                                         
         BAS   RE,NEXTEL2                                                       
         BE    CMTF2                                                            
* R6 POINTS TO E-O-R                                                            
         LR    R7,R6               SAVE E-O-R ADDRESS                           
         L     R6,AREC2                                                         
         LA    R6,BDELEM-BUYREC(R6) POINT TO FIRST ELEM                         
*                                                                               
CMTF4    BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         GOTO1 VRECUP,DMCB,BUYREC,(R6),(R7)                                     
         ZIC   R0,1(R7)            ADD NEXT ELEM AFTER THIS ONE                 
         AR    R7,R0                                                            
         B     CMTF4                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTAT                                                      
         PRINT OFF                                                              
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE SPBUYWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPBUY55   04/02/13'                                      
         END                                                                    
