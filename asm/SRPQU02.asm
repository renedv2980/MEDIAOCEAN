*          DATA SET SRPQU02    AT LEVEL 012 AS OF 11/11/20                      
*PHASE T13102A                                                                  
*INCLUDE XSORT                                                                  
*&&      SET   NOP=N                                                            
         TITLE '$PQ - REPORT FUNCTIONS'                                         
         PRINT NOGEN                                                            
PQRPRT   CSECT                                                                  
         NMOD1 000,**$PQ2**,R9,R8,RR=R4                                         
         LR    RC,R1                                                            
         USING PQUWKD,RC           RC=A(ROOTS WORKING STORAGE)                  
         LARL  RA,COMMON                                                        
         USING COMMON,RA                                                        
         ST    R4,RELO                                                          
         SAM24 ,                   SET IN 24-BIT MODE                           
*                                                                               
         L     R1,APARM                                                         
         L     R2,12(R1)                                                        
         USING COMFACSD,R2         R2=A(COM FAC LIST)                           
         L     R3,20(R1)                                                        
         USING SRPQUFFD,R3         R3=A(TWA)                                    
         L     R2,ASAVESTR                                                      
         USING PQSAVED,R2                                                       
         MVI   NDXRT,0             SET NO READ FOR UPDATE                       
         MVI   BYTE1,0                                                          
         MVC   HALF1,USERID        SAVE REPUSER                                 
*                                                                               
P3VAL    DS    0H                  P3=REPORT TYPE AND ATTRIBUTES                
         MVI   REPTYPE,0                                                        
         MVI   REPTYP1,0                                                        
         MVI   REPTYP2,0                                                        
         MVI   REPATTB,0                                                        
         LA    R4,SRVP3H                                                        
         USING FLDHDRD,R4                                                       
         CLI   FLDILEN,0                                                        
         BE    P3VX                                                             
         L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(4,(R6))                                      
         SR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         LTR   R0,R0               R0=NUMBER OF INPUT FIELDS                    
         BZ    ERR2                                                             
*                                                                               
P3V1     CLI   1(R6),0             EACH FLD HAS MIN OF 3 CHRS                   
         BNE   ERR2                                                             
         CLI   0(R6),3                                                          
         BL    ERR2                                                             
         CLI   0(R6),8                                                          
         BH    ERR2                                                             
         SR    R1,R1                                                            
         IC    R1,0(R6)                                                         
         BCTR  R1,0                                                             
*                                                                               
         L     RE,=A(TYPETBL)                                                   
         A     RE,RELO                                                          
P3V2     CLI   0(RE),0             SEARCH REPORT TYPE TABLE                     
         BE    P3V5                                                             
         EX    0,0(RE)             RF =A(KEYWORD)                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RF)                                                   
         BE    P3V3                                                             
         LA    RE,L'TYPETBL(RE)                                                 
         B     P3V2                                                             
*                                                                               
P3V3     IC    R1,4(RE)            TEST FOR DUPLICATE BIT VALUE                 
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    REPTYPE,0                                                        
         BNZ   ERR3                                                             
         OC    REPTYPE,4(RE)                                                    
*                                                                               
P3V4     LA    R6,32(R6)           BUMP TO NEXT FIELD                           
         BCT   R0,P3V1                                                          
         B     P3VX                                                             
*                                                                               
P3V5     L     RE,=A(ATTBTBL)      SEARCH REPORT ATTB TABLE                     
         A     RE,RELO                                                          
P3V6     CLI   0(RE),0                                                          
         BE    P3VA                                                             
         EX    0,0(RE)             RF=A(KEYWORD)                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RF)                                                   
         BE    P3V7                                                             
         LA    RE,L'ATTBTBL(RE)                                                 
         B     P3V6                                                             
*                                                                               
P3V7     ICM   R1,1,4(RE)          TEST FOR DUPLICATE BIT VALUE                 
         BZ    P3V7A                                                            
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    REPATTB,0                                                        
         BNZ   ERR3                                                             
         OC    REPATTB,4(RE)                                                    
         B     P3V8                                                             
P3V7A    OC    REPTYP2,5(RE)                                                    
*                                                                               
P3V8     LA    R6,32(R6)           BUMP TO NEXT FIELD                           
         BCT   R0,P3V1                                                          
         B     P3VX                                                             
*                                                                               
P3VA     L     RE,=A(TYP1TBL)      SEARCH REPORT TYPE#1 TABLE                   
         A     RE,RELO                                                          
P3VB     CLI   0(RE),0                                                          
         BE    ERR2                                                             
         EX    0,0(RE)             RF=A(KEYWORD)                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RF)                                                   
         BE    P3VC                                                             
         LA    RE,L'TYP1TBL(RE)                                                 
         B     P3VB                                                             
*                                                                               
P3VC     IC    R1,4(RE)            TEST FOR DUPLICATE BIT VALUE                 
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    REPTYP1,0                                                        
         BNZ   ERR3                                                             
         OC    REPTYP1,4(RE)                                                    
*                                                                               
P3VD     LA    R6,32(R6)           BUMP TO NEXT FIELD                           
         BCT   R0,P3V1                                                          
*                                                                               
P3VX     DS    0H                                                               
*                                                                               
P4VAL    DS    0H                  P4=FILTERS NAMED BY KEYWORDS                 
         LA    R4,SRVP4H                                                        
         XC    IFFILTS(IFFILTL),IFFILTS                                         
*                                                                               
         MVI   IFSORV,X'02'        SET DEFAULT SORT TO CREATE DATE              
         MVI   IFFMTV,1            SET DEFAULT FORMAT                           
*&&UK                                                                           
         CLC   USERID,=X'0E8B'     IF USER=TARGET                               
         BNE   *+8                                                              
         MVI   IFSORV,X'03'        SET DEFAULT SORT TO -CREATE                  
*                                                                               
         CLI   PQSCLASS,C'0'       IF CLASS=0 SET SORT=A                        
         BNE   *+12                                                             
         MVI   IFSORV,X'01'                                                     
         B     *+12                AND DON'T......                              
         MVI   IFCLAF,X'70'        SET DEFAULT /=0                              
         MVI   IFCLAV,C'0'                                                      
*&&                                                                             
         CLI   FLDILEN,0                                                        
         BE    P4VXX                                                            
         L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(8,(R6))                                      
         MVC   FLAG,4(R1)                                                       
         CLI   FLAG,0                                                           
         BE    ERR0                NUMBER/SYNTAX OF INPUT FIELDS                
*                                                                               
P4V0     CLI   1(R6),0             FORMAT MUST BE KEYWORD=VALUE                 
         BE    ERR0                                                             
         ZIC   R1,0(R6)                                                         
         LTR   R1,R1                                                            
         BZ    ERR0                                                             
         MVI   FLAG1,X'80'         SET DEFAULT EQ SIGN                          
         LA    RE,11(R6,R1)                                                     
         CLI   0(RE),X'4C'         CHECK AND SET LT SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'D0'                                                      
         CLI   0(RE),X'6E'         CHECK AND SET GT SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'B0'                                                      
         CLI   0(RE),X'61'         CHECK AND SET NE SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'70'                                                      
         CLI   FLAG1,X'80'         ADJUST LEN IF SIGN VALUE LAST CHR            
         BE    *+14                                                             
         MVI   0(RE),C' '                                                       
         BCTR  R1,0                                                             
         STC   R1,0(R6)                                                         
         CLI   0(R6),2             KEYWORD MUST BE 2 THRU 8 CHRS LONG           
         BL    ERRF                                                             
         CLI   0(R6),8                                                          
         BH    ERRF                                                             
         BCTR  R1,0                                                             
         LA    R7,FILTTBL                                                       
*                                                                               
P4V1     CLI   0(R7),0             SEARCH FILTER NAME TABLE                     
         BE    ERRF                                                             
         EX    0,0(R7)             RF=A(KEYWORD)                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RF)                                                   
         BE    P4V2                                                             
         LA    R7,L'FILTTBL(R7)                                                 
         B     P4V1                                                             
*                                                                               
P4V2     TM    6(R7),X'01'         TEST IF DDS ONLY KEYWORD                     
         BZ    *+12                                                             
         TM    DDS,DDSTRM                                                       
         BZ    ERRF                                                             
         CLI   FLAG1,X'80'         TEST IF SIGN VALUE ALLOWED                   
         BE    P4V3                                                             
         CLI   FLAG1,X'70'         TEST NE ALLOWED                              
         BNE   *+12                                                             
         TM    5(R7),X'80'                                                      
         BZ    ERRFA                                                            
         CLI   FLAG1,X'D0'         TEST LT ALLOWED                              
         BNE   *+12                                                             
         TM    5(R7),X'40'                                                      
         BZ    ERRFA                                                            
         CLI   FLAG1,X'B0'         TEST GT ALLOWED                              
         BNE   *+12                                                             
         TM    5(R7),X'20'                                                      
         BZ    ERRFA                                                            
*                                                                               
P4V3     CLI   4(R7),X'F5'         TEST FOR XSORT                               
         BNE   *+12                                                             
         MVI   4(R7),X'05'         SET TO SORT                                  
         MVI   BYTE1,C'X'                                                       
         ZIC   RF,4(R7)            GOTO ROUTINE FOR VALUE                       
         SLL   RF,2                                                             
         B     P4ROUTS(RF)         R6=A(SCANNER TABLE ENTRY)                    
*                                                                               
P4VX     LA    R6,32(R6)           BACK FOR NEXT KEYWORD                        
         ZIC   R1,FLAG                                                          
         SH    R1,=H'1'            DECR NUM OF KEYWORDS INPUT                   
         BZ    P4VXX                                                            
         STC   R1,FLAG                                                          
         B     P4V0                                                             
P4VXX    B     TYPVAL                                                           
*                                                                               
P4ROUTS  B     ERRF                VALIDATION ROUTINES                          
         B     CLAVALR             01                                           
         B     DATVALR             02                                           
         B     DDSVALR             03                                           
         B     SIZVALR             04                                           
         B     SORVALR             05                                           
         B     TIMVALR             06                                           
         B     FMTVALR             07                                           
         B     CDAVALR             08                                           
         B     PDAVALR             09                                           
         B     RDAVALR             10                                           
         B     PIDVALR             11                                           
         B     PINVALR             12                                           
         B     ERRF                13 N/D                                       
         B     ERRF                14 N/D                                       
         B     ERRF                15 N/D                                       
*                                                                               
CLAVALR  MVC   IFCLAF,FLAG1        SET CLASS FILTER INPUT FLAG                  
         LA    R7,IFCLAV                                                        
         CLI   1(R6),8             MAX OF 8 CLASSES                             
         BH    CLAVW                                                            
         ZIC   RF,1(R6)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     CLAVX                                                            
         MVC   0(0,R7),22(R6)      SET LIST OF CLASSES                          
CLAVW    B     ERRFB                                                            
CLAVX    B     P4VX                                                             
*                                                                               
DATVALR  MVC   IFDATF,FLAG1        SET DATE FILTER INPUT FLAG                   
         LA    R7,IFDATV                                                        
         B     DATV0                                                            
CDAVALR  MVC   IFCDAF,FLAG1        SET CREATED DATE FILTER INPUT FLAG           
         LA    R7,IFCDAV                                                        
         B     DATV0                                                            
PDAVALR  MVC   IFPDAF,FLAG1        SET PRINTED DATE FILTER INPUT FLAG           
         LA    R7,IFPDAV                                                        
         B     DATV0                                                            
RDAVALR  MVC   IFRDAF,FLAG1        SET RETAINED DATE FILTER INPUT FLAG          
         LA    R7,IFRDAV                                                        
         B     DATV0                                                            
DATV0    CLC   22(5,R6),SR@TODAY   CHECK FOR TODAYS DATE                        
         BNE   DATV1                                                            
         MVC   0(2,R7),DATEC                                                    
         MVC   2(2,R7),DATECO                                                   
         B     DATVX                                                            
DATV1    GOTO1 ADATVAL,DMCB,(0,22(R6)),DUB                                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   DATV3               VALID YYMMDD INPUT                           
DATV2    GOTO1 (RF),(R1),(1,22(R6))                                             
         OC    0(4,R1),0(R1)                                                    
         BZ    DATVW                                                            
         MVC   DUB(2),DATE         VALID MMDD INPUT                             
DATV3    GOTO1 ADATCON,DMCB,(0,DUB),(30,0(R7)) GET NEW CMPRSD DATE              
         GOTO1 (RF),(R1),,(2,2(R7))    GET OLD CMPRSD DATE                      
         B     DATVX                                                            
DATVW    B     ERRFB                                                            
DATVX    B     P4VX                                                             
*                                                                               
TIMVALR  MVC   IFTIMF,FLAG1        SET TIME FILTER INPUT FLAG                   
         LA    R7,IFTIMV                                                        
         TM    3(R6),X'80'         MUST BE HH OR HHMM FORMAT                    
         BZ    TIMVW                                                            
         CLI   1(R6),2                                                          
         BL    TIMVW                                                            
         CLC   22(2,R6),=C'00'     TEST HOURS 00-23                             
         BL    TIMVW                                                            
         CLC   22(2,R6),=C'23'                                                  
         BH    TIMVW                                                            
         PACK  DUB,22(2,R6)        CONVERT HOURS                                
         CVB   R0,DUB                                                           
         STC   R0,0(R7)                                                         
         MVI   1(R7),0                                                          
         CLI   1(R6),2                                                          
         BE    TIMVX                                                            
         CLI   1(R6),4                                                          
         BNE   TIMVW                                                            
         CLC   24(2,R6),=C'00'     TEST MINUTES 00-59                           
         BL    TIMVW                                                            
         CLC   24(2,R6),=C'59'                                                  
         BH    TIMVW                                                            
         PACK  DUB,24(2,R6)        CONVERT MINS                                 
         CVB   R0,DUB                                                           
         STC   R0,1(R7)                                                         
         B     TIMVX                                                            
TIMVW    B     ERRFB                                                            
TIMVX    B     P4VX                                                             
*                                                                               
DDSVALR  MVC   IFDDSF,FLAG1        SET SPECIAL DDS INPUT FLAG                   
         LA    R7,IFDDSV                                                        
         CLI   1(R6),1                                                          
         BNE   DDSVW                                                            
         CLI   22(R6),C'A'         ONE CHR A THRU Z                             
         BL    DDSVW                                                            
         CLI   22(R6),C'Z'                                                      
         BH    DDSVW                                                            
         MVC   0(1,R7),22(R6)                                                   
         B     DDSVX                                                            
DDSVW    B     ERRFB                                                            
DDSVX    B     P4VX                                                             
*                                                                               
FMTVALR  MVC   IFFMTF,FLAG1        SET FORMAT FILTER INPUT FLAG                 
         LA    R7,IFFMTV                                                        
         TM    3(R6),X'80'         MUST BE INTEGER 1 THRU 2                     
         BZ    FMTVW                                                            
         L     RE,8(R6)                                                         
         LTR   RE,RE                                                            
         BZ    FMTVW                                                            
         CH    RE,=H'2'                                                         
         BH    FMTVW                                                            
         STC   RE,0(R7)                                                         
         B     FMTVX                                                            
FMTVW    B     ERRFB                                                            
FMTVX    B     P4VX                                                             
*                                                                               
SIZVALR  MVC   IFSIZF,FLAG1        SET SIZE FILTER INPUT FLAG                   
         LA    R7,IFSIZV                                                        
         TM    3(R6),X'80'         MUST BE INTEGER 0 THRU 255                   
         BZ    SIZVW                                                            
         L     RE,8(R6)                                                         
         LTR   RE,RE                                                            
         BM    SIZVW                                                            
         CH    RE,=H'255'                                                       
         BH    SIZVW                                                            
         STC   RE,0(R7)                                                         
         B     SIZVX                                                            
SIZVW    B     ERRFB                                                            
SIZVX    B     P4VX                                                             
*                                                                               
SORVALR  MVC   IFSORF,FLAG1        SET SORT FILTER INPUT FLAG                   
         SR    R1,R1                                                            
         ICM   R1,1,1(R6)                                                       
         BZ    ERRFB               MUST BE AT LEAST 1 CHR                       
         BCTR  R1,0                                                             
         LA    R7,SORTTBL                                                       
         LA    RE,COMP1                                                         
         CLI   22(R6),C'A'         IS 1ST CHR SIGN                              
         BNL   SORV1                                                            
         BCTR  R1,0                                                             
         LA    RE,COMP2                                                         
SORV1    CLI   0(R7),0                                                          
         BE    ERRFB                                                            
         CLM   R1,1,6(R7)                                                       
         BNL   SORV2                                                            
         EX    0,0(R7)             RF=A(KEYWORD)                                
         EX    R1,0(RE)                                                         
         BE    SORV3                                                            
SORV2    LA    R7,L'SORTTBL(R7)                                                 
         B     SORV1                                                            
SORV3    CLI   22(R6),C'-'                                                      
         BNE   SORV4                                                            
         MVC   IFSORV(1),5(R7)     -VE SORT                                     
         B     P4VX                                                             
SORV4    MVC   IFSORV(1),4(R7)     +VE SORT                                     
         B     P4VX                                                             
*                                                                               
COMP1    CLC   22(0,R6),0(RF)      CIMPARE WITHOUT SIGN                         
COMP2    CLC   23(0,R6),0(RF)      COMPARE WITH SIGN                            
*                                                                               
PIDVALR  MVC   IFPIDF,FLAG1        SET PID FILTER INPUT FLAG                    
         LA    R7,IFPIDV                                                        
         CLI   1(R6),1                                                          
         BL    PIDVW                                                            
         BH    PIDV1                                                            
         CLI   22(R6),C'*'         PID=* MEANS MY PID                           
         BNE   PIDVW                                                            
         MVC   IFPIDV,TRMPID                                                    
         MVC   PQSPID,IFPIDV       CAN ALSO BE SET IN P2                        
         MVI   PQSPIF,X'20'                                                     
         B     PIDVX                                                            
PIDV1    CLI   1(R6),4             TEST IF DDS PASSWORD FOR PID                 
         BNE   PIDV2                                                            
         TM    DDS,DDSTRM                                                       
         BNO   PIDV2                                                            
         CLC   DDSPSWD,22(R6)      PID=DDPQ                                     
         BNE   PIDV2                                                            
         MVC   IFPIDV,FFS                                                       
         MVC   PQSPID,FFS          SET FIELD TO FF'S                            
         OI    PQSPID,X'80'        SET DDS PID/PIN INPUT                        
         B     PIDVX                                                            
PIDV2    EQU   *                   ANY OTHER VALUE IS INVALID                   
PIDVW    B     ERRFB                                                            
PIDVX    B     P4VX                                                             
*                                                                               
PINVALR  MVC   IFPINF,FLAG1        SET PIN FILTER INPUT FLAG                    
         LA    R7,IFPINV                                                        
         CLI   1(R6),4                                                          
         BH    PINVW                                                            
         MVC   IFPINV,22(R6)                                                    
         MVC   PQSPIN,IFPINV       CAN ALSO BE SET IN P2                        
         MVI   PQSPIF,X'10'                                                     
PINV1    TM    DDS,DDSTRM          TEST IF DDS PASSWORD FOR PIN                 
         BNO   PINVX                                                            
         CLC   DDSPSWD,22(R6)      PIN=DDPQ                                     
         BNE   PINVX                                                            
         MVC   IFPINV,FFS                                                       
         MVC   PQSPIN,FFS          SET FIELD TO FF'S                            
         OI    PQSPIN,X'80'        SET DDS PID/PIN INPUT                        
         B     PINVX                                                            
PINVW    B     ERRFB                                                            
PINVX    B     P4VX                                                             
*                                                                               
TYPVAL   DS    0H                  CHECK FIELD COMPATIBILITY                    
         OC    PQSSEQ,PQSSEQ       INDIVIDUAL REPORT SPECIFIED                  
         BZ    TYPV1               NO                                           
         MVI   REPTYPE,X'FF'       IGNORE TYPE                                  
         B     TYPV6                                                            
*                                                                               
TYPV1    LA    R4,SRVP3H           GROUP OF REPORTS SPECIFIED                   
         L     R1,=A(COMPTBL)                                                   
         A     R1,RELO                                                          
TYPV2    CLI   0(R1),X'FF'         FIND FIRST ENTRY FOR ACTION                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),ACTN                                                     
         BE    TYPV3                                                            
         LA    R1,L'COMPTBL(R1)                                                 
         B     TYPV2                                                            
*                                                                               
TYPV3    CLI   1(R1),0             TEST IF OPTIONAL REPORT TYPE                 
         BNE   TYPV4                                                            
         CLI   REPTYPE,0                                                        
         BNE   TYPV4                                                            
         LA    R1,L'COMPTBL(R1)    EXTRACT DEFAULT                              
         MVC   REPTYPE,1(R1)                                                    
         B     TYPV6                                                            
*                                                                               
TYPV4    CLI   REPTYPE,0           REQUIRED REPORT TYPE                         
         BE    ERR1                                                             
TYPV5    LA    R1,L'COMPTBL(R1)                                                 
         CLC   ACTN,0(R1)                                                       
         BH    ERR2                NOT IN TABLE                                 
         CLC   REPTYPE,1(R1)                                                    
         BNE   TYPV5                                                            
*                                                                               
TYPV6    CLI   ACTN,X'21'          TEST DISPLAY ACTION                          
         BE    TYPV7                                                            
         CLI   ACTN,X'28'          TEST SIZE ACTION                             
         BE    TYPV7                                                            
*                                                                               
         MVI   NDXRT,X'80'         SET INDEX READ TYPE                          
*                                                                               
TYPV7    MVI   SRVEX1,C' '         **NOP** WAS C'.'                             
         CLI   ACTN,X'27'          TEST CLEAR SPECIAL ACTION                    
         BNE   TYPV8                                                            
*NOP     BRAS  RE,CLR                                                           
         B     EXIT                                                             
*                                                                               
TYPV8    TM    INTFLAG,INTRUN      TEST INTERNAL ACTION                         
         BNO   LOAD                                                             
*                                                                               
         SAM31                                                                  
         MVC   FIWRES,SPACES                                                    
         MVC   FIWRES(L'PRTQID),PRTQID                                          
         BRAS  RE,FIRSET                                                        
*                                                                               
         MVC   FIWCIA,CIADDR                                                    
         BRAS  RE,FIRCN                                                         
         L     R1,FIWNDA                                                        
         MVC   FIWNDX,SI1NDX-SI1PAR(R1)                                         
         SAM24                                                                  
*                                                                               
         LA    R5,FIWNDX                                                        
         USING PQRECD,R5                                                        
         MVC   CISTAT,PQSTAT       SET STAT FROM INDEX                          
         MVC   CITYP1,PQTYP1       SET TYP1 FROM INDEX                          
         B     INTACT                                                           
*                                                                               
LOAD     TM    INTFLAG,INTLAST     DONT INIT IF INTLAST                         
         BO    *+14                                                             
         OC    PQSCHANG,PQSCHANG   TEST MATCH ON FIELDS                         
         BNE   INIT                                                             
         SR    R1,R1                                                            
         IC    R1,PQSPAGE                                                       
         TM    INTFLAG,INTLAST     IGNORE PFKEYS IF INTLAST                     
         BO    LOAD2                                                            
         CLI   PFKEY,4             OR IF DQU RETURN                             
         BE    LOAD2                                                            
         CLI   PFKEY,9             OR IF JOB RETURN                             
         BE    LOAD2                                                            
LOAD1    CLI   PFKEY,PFUP5                                                      
         BE    *+12                                                             
         CLI   PFKEY,PFUP7                                                      
         BNE   LOAD1A                                                           
         BCTR  R1,0                                                             
         B     LOAD2                                                            
LOAD1A   LA    R1,1(R1)                                                         
*                                                                               
LOAD2    LTR   R1,R1               TEST FOR NEGATIVE PAGE                       
         BNM   *+6                                                              
         SR    R1,R1                                                            
         STC   R1,PQSPAGE                                                       
         MH    R1,=H'16'                                                        
         CH    R1,PQSTOTL                                                       
         BL    *+8                                                              
         MVI   PQSPAGE,0                                                        
         TM    DDS,DDSNEW                                                       
         BO    INIT                READ QUEUE ON DISPLAY ACTION                 
         CLI   ACTN,X'28'                                                       
         BE    SRCHPQ              READ QUEUE ON SIZE ACTION                    
*                                                                               
         LA    RF,PQSCIADS         RF = A(SAVE AREA)                            
         USING CISAVED,RF                                                       
         L     RE,AREPTAB          RE = A(REPTAB POSITION)                      
         LA    R1,600                                                           
LOAD3    MVC   RTEUSER-RTED(2,RE),=X'0000'                                      
         MVI   RTESTAT-RTED(RE),0                                               
         MVI   RTETYP1-RTED(RE),0                                               
         TM    CISACTN,X'80'                                                    
         BNO   *+8                                                              
         OI    RTESTAT-RTED(RE),PQSTPG                                          
         MVC   RTECIADR-RTED(2,RE),CISADR                                       
         MVC   RTEREPNO-RTED(2,RE),CISSEQ                                       
         TM    DDS1,DDSTOT                                                      
         BNO   *+14                                                             
         MVC   RTESORT-RTED(2,RE),CISSEQ                                        
         B     *+10                                                             
         MVC   RTESORT-RTED(2,RE),CISACTN                                       
         LA    RE,L'RTEDATA(RE)                                                 
         LA    RF,L'CISAVE(RF)                                                  
         CLC   0(6,RF),FFS                                                      
         BE    *+8                                                              
         BCT   R1,LOAD3                                                         
         MVC   0(L'RTEDATA,RE),FFS     END MARK                                 
         B     SAVE1                                                            
         DROP  RF                                                               
*                                                                               
INIT     MVI   PQSPAGE,0                                                        
*                                                                               
SRCHPQ   EQU   *                   SEARCH INDEX FOR MATCHING ENTRIES            
         MVI   SRVEX1,C' '         *NOP* WAS C'*'                               
         MVI   PQSBLK,X'FF'        ENSURE NO MATCH AT SAVE1                     
         NI    DDS,255-DDSNEW      CLEAR RELOAD FLAG                            
         ZAP   QR,=P'0'                                                         
         ZAP   QA,=P'0'                                                         
         ZAP   QH,=P'0'                                                         
         ZAP   QG,=P'0'                                                         
         ZAP   QD,=P'0'                                                         
         ZAP   QE,=P'0'                                                         
         ZAP   QK,=P'0'                                                         
         ZAP   QT,=P'0'                                                         
         ZAP   QJ,=P'0'                                                         
         ZAP   QI,=P'0'                                                         
         ZAP   QX,=P'0'                                                         
         ZAP   QY,=P'0'                                                         
         ZAP   QPID,=P'0'                                                       
         MVI   QIND,1              SET SEARCHING PART1 INDEX                    
         XC    CITOTS,CITOTS                                                    
*                                                                               
         MVC   FULN(2),DATEC       SET TODAYS DATE OR DATE= DATE                
         MVC   FULO(2),DATECO                                                   
         CLI   IFDATF,0                                                         
         BE    *+16                                                             
         MVC   FULN(2),IFDATV                                                   
         MVC   FULO(2),IFDATVO                                                  
         BRAS  RE,GETTIME          SET TIME NOW OR TIME= TIME                   
         CLI   IFTIMF,0                                                         
         BE    *+10                                                             
         MVC   DUB(2),IFTIMV                                                    
         SR    R0,R0               CONVERT HHMM TO 10 MIN INCREMENT             
         SR    R1,R1                                                            
         IC    R1,DUB                                                           
         MH    R1,=H'60'                                                        
         IC    R0,DUB+1                                                         
         AR    R1,R0               R1=TIME IN MINUTES                           
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,FULN+2           FULL+2(1)=TIME INCREMENT VALUE               
         STC   R1,FULO+2                                                        
*                                                                               
SRCH0    L     R6,AREPTAB          R6=A(NEXT ENTRY IN SAVE TABLE)               
         MVC   0(L'RTEDATA,R6),FFS                                              
*                                                                               
         MVI   QIND,1                                                           
         LA    R5,FIWNDX                                                        
         USING PQRECD,R5           R5=A(PRTQUE INDEX ENTRY)                     
*                                                                               
         SAM31                                                                  
         MVC   FIWRES,PRTQID                                                    
         BRAS  RE,FIRSET           SET SHARED MEMORY VALUES FOR PQ              
         JNE   *+2                                                              
         MVC   FIWNDA,FIWP1A       FIRST PART1 INDEX                            
         BRAS  RE,FIRNC                                                         
         B     SRCH1A                                                           
         SAM24                                                                  
*                                                                               
SRCH1    SAM31                                                                  
         CLI   QIND,2                                                           
         BE    SRCH1B                                                           
         BRAS  RE,FIRNSN                                                        
         BNE   SRCHXX                                                           
SRCH1A   L     R1,FIWNDA                                                        
         MVC   FIWNDX(L'PQINDEX),SI1NDX-SI1PARD(R1)                             
         B     SRCH1D                                                           
SRCH1B   BRAS  RE,FIRNSN2                                                       
         BNE   SRCHXX                                                           
SRCH1C   L     R1,FIWNDA                                                        
         MVC   FIWNDX(L'PQINDEX),SI2NDX-SI2PARD(R1)                             
SRCH1D   BRAS  RE,FIRNC            A(NODE) TO A(CI)                             
         SAM24                                                                  
*                                                                               
         OC    PQKEY,PQKEY         IGNORE CLEARED REPORTS                       
         BZ    SRCHX                                                            
SRCH2A   CLI   PQAGERT,X'FF'       TEST IF TEMPORARY                            
         BNE   SRCH2C                                                           
         CLI   QIND,1                                                           
         BNE   SRCH2B                                                           
         LH    R1,CIX1             BUMP PART1 NOTAVAIL                          
         LA    R1,1(R1)                                                         
         STH   R1,CIX1                                                          
         B     SRCHX                                                            
SRCH2B   LH    R1,CIX2             BUMP PART2 NOTAVAIL                          
         LA    R1,1(R1)                                                         
         STH   R1,CIX2                                                          
         B     SRCHX                                                            
SRCH2C   LA    R1,FULN                                                          
         TM    PQTYP1,PQTYNCD      SET R1 TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    R1,FULO                                                          
         CLC   PQAGERD,0(R1)       TEST RETAIN DATE                             
         BL    SRCH2G                                                           
         BH    SRCH2D                                                           
         CLI   PQAGERT,X'FE'       TEST RETAIN TIME VALUE                       
         BE    SRCH2D                                                           
         CLC   PQAGERT,2(R1)                                                    
         BL    SRCH2G                                                           
SRCH2D   CLI   QIND,1                                                           
         BNE   SRCH2E                                                           
         LH    R1,CIX1             BUMP PART1 NOTAVAIL                          
         LA    R1,1(R1)                                                         
         STH   R1,CIX1                                                          
         B     SRCH2G                                                           
SRCH2E   LH    R1,CIX2             BUMP PART2 NOTAVAIL                          
         LA    R1,1(R1)                                                         
         STH   R1,CIX2                                                          
         B     SRCHX                                                            
SRCH2G   EQU   *                                                                
*                                                                               
SRCH3    OC    USERID,USERID       TEST USER ID DEFINED                         
         BZ    SRCH3G                                                           
         TM    PQSUSER,X'80'       TEST GENERIC USER ID                         
         BZ    SRCH3B                                                           
         CLI   AGENIDS,X'FF'       TEST VGENIDS PRESENT                         
         BE    SRCHX                                                            
         GOTO1 AGENIDS,DMCB,PQSUSER,ADATAMGR                                    
         BNE   SRCHX                                                            
         LM    RE,RF,0(R1)         RE=N'ENTRIES,RF=A(ENTRIES)                   
         CLC   PQSRCID,0(RF)                                                    
         BE    SRCH3A                                                           
         LA    RF,2(RF)                                                         
         BCT   RE,*-14                                                          
         B     SRCHX                                                            
SRCH3A   B     SRCH3G                                                           
SRCH3B   CLC   PQSRCID,USERID      ID WAS SPECIFIED                             
         BNE   SRCHX                                                            
         CLI   PQSTAT,PQSTPU       IGNORE PURGED                                
         BE    SRCHX                                                            
SRCH3G   EQU   *                                                                
*&&US                                                                           
         CLI   IFDDSV,C'E'         UNLESS DDS=E                                 
         BE    SRCH3G1                                                          
         TM    SYSFLG,X'80'        OR TEST SYSTEM                               
         BO    SRCH3G1                                                          
         CLI   PQCLASS,C'N'        IGNORE CLASS N                               
         BE    SRCHX                                                            
         CLI   PQCLASS,C'G'        IGNORE CLASS G                               
         BE    SRCHX                                                            
*&&                                                                             
SRCH3G1  CLI   PQSCLASS,0          FILTER ON CLASS (POSITIVE/NEGATIVE)          
         BE    SRCH3G2                                                          
         TM    PQSCLASS,X'40'                                                   
         BZ    *+18                                                             
         CLC   PQCLASS,PQSCLASS                                                 
         BNE   SRCHX                                                            
         B     SRCH3H                                                           
         MVC   WORK(1),PQSCLASS                                                 
         OI    WORK,X'40'                                                       
         CLC   PQCLASS,WORK                                                     
         BE    SRCHX                                                            
*                                                                               
SRCH3G2  CLI   PQSRTYP,0           FILTER ON REPORT TYPE                        
         BE    SRCH3H                                                           
         CLC   PQREPTY,PQSRTYP                                                  
         BNE   SRCHX                                                            
*                                                                               
SRCH3H   CLC   PQSSUBID,SR@ALL     TEST SUB-ID                                  
         BE    SRCH3I                                                           
         CLC   PQSSUBID,=X'FFFFFF' SPECIAL FLAG MEANING JOBS                    
         BE    SRCH3HA                                                          
         CLC   PQSSUBID(1),PQSUBID                                              
         BNE   SRCHX                                                            
         CLI   PQSSUBID+1,C'*'                                                  
         BE    SRCH3I                                                           
         CLC   PQSSUBID+1(1),PQSUBID+1                                          
         BNE   SRCHX                                                            
         CLI   PQSSUBID+2,C'*'                                                  
         BE    SRCH3I                                                           
         CLC   PQSSUBID+2(1),PQSUBID+2                                          
         BE    SRCH3I                                                           
         B     SRCHX                                                            
*                                                                               
SRCH3HA  L     R4,ATIA             LOOK IN TWA11 JOB QUEUE                      
         USING SRSD,R4                                                          
         SR    R0,R0                                                            
         ICM   R0,1,SRJOBINQ                                                    
         BZ    SRCHX               NO JOBS                                      
*                                                                               
SRCH3HB  CLC   SRJOBSUB(5),PQSUBID                                              
         BE    SRCH3I                                                           
         LA    R4,SRJOBQLN(R4)     NEXT                                         
         BCT   R0,SRCH3HB                                                       
         B     SRCHX                                                            
*                                                                               
SRCH3I   AP    QR,=P'1'            BUMP TOTAL REPORTS                           
         TM    PQATTB,PQATJOBI                                                  
         BZ    *+14                                                             
         AP    QJ,=P'1'            BUMP SOON                                    
         B     SRCH3J              DON'T COUNT SOON AS ANYTHING ELSE            
         TM    PQSTAT,PQSTAC                                                    
         BZ    *+10                                                             
         AP    QA,=P'1'            BUMP ACTIVE                                  
         TM    PQSTAT,PQSTHO                                                    
         BZ    *+10                                                             
         AP    QH,=P'1'            BUMP HOLD                                    
         CLI   PQAGERT,X'FE'                                                    
         BNE   *+10                                                             
         AP    QG,=P'1'            BUMP PRINTING                                
         TM    PQSTAT,PQSTPR                                                    
         BZ    *+10                                                             
         AP    QD,=P'1'            BUMP PRINTED                                 
         TM    PQSTAT,PQSTSE                                                    
         BZ    *+10                                                             
         AP    QE,=P'1'            BUMP SENT                                    
         TM    PQSTAT,PQSTKE                                                    
         BZ    *+10                                                             
         AP    QK,=P'1'            BUMP KEEP                                    
         TM    PQSTAT,PQSTIN                                                    
         BZ    *+10                                                             
         AP    QI,=P'1'            BUMP INVISIBLE                               
*                                                                               
SRCH3J   TM    DDS,DDSTRM          ONLY DDS CAN SEE FUTURE DATED                
         BO    SRCH3J1                                                          
         LA    RF,DATEC                                                         
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,DATECO                                                        
         CLC   PQAGELD,0(RF)       FILTER ON DATE/TIME                          
         BH    SRCHX                                                            
SRCH3J1  OC    PQSTIMES,PQSTIMES   TEST NO TIMES INPUT                          
         BZ    SRCH3M                                                           
         ZIC   R1,IFCDAF           TEST CREATED DATE FILTER                     
         LTR   R1,R1                                                            
         BZ    SRCH3K                                                           
         LA    RF,IFCDAV           SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,2(RF)                                                         
         CLC   PQAGELD,0(RF)                                                    
         EX    R1,*+8                                                           
         B     SRCHX                                                            
         BC    0,SRCH3K                                                         
*                                                                               
SRCH3K   MVC   HALF2,DATEC         SET HALF2 TO TODAY                           
         MVC   HALF3,DATECO                                                     
         CLI   IFCDAF,X'80'        TEST CDATE= FILTER                           
         BNE   *+16                                                             
         MVC   HALF2,IFCDAV        SET HALF TO CDATE=FILTER                     
         MVC   HALF3,IFCDAV+2                                                   
         LA    RF,HALF2            SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,2(RF)                                                         
         TM    DDS1,DDSTIM         TEST SINGLE TIME INPUT                       
         BNO   SRCH3L                                                           
         CLC   PQAGELD,0(RF)       INCLUDE ANYTHING CREATED BEFORE DATE         
         BL    SRCH4                                                            
         B     *+14                                                             
SRCH3L   CLC   PQAGELD,0(RF)       EXCLUDE ANYTHING CREATED BEFORE DATE         
         BL    SRCHX                                                            
         CLC   PQAGELT,PQSTIMES                                                 
         BL    SRCHX                                                            
         CLC   PQAGELT,PQSTIMES+2                                               
         BH    SRCHX                                                            
SRCH3M   CLC   PQSSEQ,PQREPNO    * NOT NEEDED FOR OPTIMISED QUEUE               
         BE    SRCH4             *                                              
         OC    PQSSEQ,PQSSEQ     *                                              
         BNZ   SRCHX             *                                              
*                                                                               
SRCH4    CLI   REPTYPE,X'FF'       FILTER ON REPORT STATUS TYPE                 
         BE    SRCH4X                                                           
SRCH4A   MVC   WORK(1),REPTYPE                                                  
         TM    WORK,PQSTKE         TEST KEEP STATUS FIRST                       
         BZ    SRCH4B                                                           
         TM    PQSTAT,PQSTKE                                                    
         BZ    SRCHX               BYPASS IF FAILS KEEP TEST                    
         NI    WORK,255-PQSTKE                                                  
         BZ    SRCH4X                                                           
SRCH4B   TM    WORK,PQSTIN         TEST INVISIBLE STATUS                        
         BZ    SRCH4C                                                           
         TM    PQSTAT,PQSTIN                                                    
         BZ    SRCHX               BYPASS IF FAILS INVISABLE TEST               
         NI    WORK,255-PQSTIN                                                  
         BZ    SRCH4X                                                           
SRCH4C   IC    R1,WORK             TEST OTHER STATUS BITS                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    PQSTAT,0                                                         
         BZ    SRCHX                                                            
SRCH4X   EQU   *                                                                
*                                                                               
SRCH5    CLI   IFCLAF,0            FILTER ON CLASS                              
         BE    SRCH5X                                                           
         CLC   IFCLAV(3),SR@ALL    ALLOW CLASS=ALL                              
         BE    SRCH5X                                                           
         LA    RE,IFCLAV                                                        
SRCH5A   CLI   0(RE),0             TEST FOR END OF INPUT CLASS LIST             
         BE    SRCH5C                                                           
         CLI   0(RE),C'*'                                                       
         BNE   *+16                                                             
         CLI   PQCLASS,0                                                        
         BE    SRCH5B                                                           
         B     *+14                                                             
         CLC   0(1,RE),PQCLASS                                                  
         BE    SRCH5B                                                           
         LA    RE,1(RE)                                                         
         B     SRCH5A                                                           
SRCH5B   CLI   IFCLAF,X'80'        CLASS VALUE IN LIST                          
         BE    SRCH5X                                                           
         B     SRCHX                                                            
SRCH5C   CLI   IFCLAF,X'70'        CLASS VALUE NOT IN LIST                      
         BE    SRCH5X                                                           
         B     SRCHX                                                            
SRCH5X   EQU   *                                                                
*                                                                               
SRCH6    TM    DDS,DDSTRM          ONLY DDS CAN SEE FUTURE DATED                
         BO    SRCH6A                                                           
         LA    RF,DATEC                                                         
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,DATECO                                                        
         CLC   PQAGELD,0(RF)       TEST CREATED DATE WITH TODAYS DATE           
         BH    SRCHX                                                            
SRCH6A   ZIC   R1,IFDATF           FILTER ON DATE (DEFAULT IS CREATED)          
         LTR   R1,R1                                                            
         BZ    SRCH6X                                                           
         LA    RF,IFDATV                                                        
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,IFDATVO                                                       
         CLC   PQAGELD,0(RF)                                                    
         EX    R1,*+8                                                           
         B     SRCHX                                                            
         BC    0,SRCH6X                                                         
SRCH6X   EQU   *                                                                
*                                                                               
SRCH7    ZIC   R1,IFCDAF           FILTER ON DATE (DEFAULT IS CREATED)          
         LTR   R1,R1                                                            
         BZ    SRCH7X                                                           
         LA    RF,IFCDAV           SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,2(RF)                                                         
         CLC   PQAGELD,0(RF)                                                    
         EX    R1,*+8                                                           
         B     SRCHX                                                            
         BC    0,SRCH7X                                                         
SRCH7X   EQU   *                                                                
*                                                                               
SRCH8    ZIC   R1,IFPDAF           FILTER ON PRINTED DATE                       
         LTR   R1,R1                                                            
         BZ    SRCH8X                                                           
         LA    RF,IFPDAV           SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,2(RF)                                                         
         OC    PQAGEDD,PQAGEDD     IF NEVER PRINTED IGNORE                      
         BZ    SRCHX                                                            
         CLI   PQAGERT,X'FE'       IF PRINTING NO DATE AVAIL                    
         BE    SRCHX                                                            
         CLC   PQAGEDD,0(RF)                                                    
         EX    R1,*+8                                                           
         B     SRCHX                                                            
         BC    0,SRCH8X                                                         
SRCH8X   EQU   *                                                                
*                                                                               
SRCH9    ZIC   R1,IFRDAF           FILTER ON RETAINED DATE                      
         LTR   R1,R1                                                            
         BZ    SRCH9X                                                           
         LA    RF,IFRDAV           SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,2(RF)                                                         
         CLC   PQAGERD,0(RF)                                                    
         EX    R1,*+8                                                           
         B     SRCHX                                                            
         BC    0,SRCH9X                                                         
SRCH9X   EQU   *                                                                
*                                                                               
SRCHA    ZIC   R1,IFSIZF           FILTER ON REPORT SIZE FACTOR                 
         LTR   R1,R1                                                            
         BZ    SRCHAX                                                           
         CLC   PQAGES,IFSIZV                                                    
         EX    R1,*+8                                                           
         B     SRCHX                                                            
         BC    0,SRCHAX                                                         
SRCHAX   EQU   *                                                                
*                                                                               
SRCHB    CLI   IFDDSV,C'N'         DDS=N IS THE CODE TO SEE ALL                 
         BE    SRCHBX                                                           
         TM    PQATTB,PQATNP       TEST REPORT NON PRINTABLE                    
         BO    SRCHX                                                            
         TM    PQATTB,PQATJOBI     TEST REPORT CONTAINS JCL                     
         BZ    SRCHB1                                                           
         TM    REPATTB,PQATJOBI    WAS JOBI INPUT                               
         BO    SRCHB1                                                           
         CLI   IFDDSV,C'J'         DDS=J IS THE SECRET CODE TO SEE              
         BNE   SRCHX                                                            
         B     SRCHBX                                                           
SRCHB1   TM    PQSTAT,PQSTIN       TEST REPORT INVISIBLE                        
         BZ    SRCHBX                                                           
         CLI   IFDDSV,C'I'         DDS=I IS THE SECRET CODE TO SEE              
         BNE   SRCHX                                                            
SRCHBX   EQU   *                                                                
*                                                                               
SRCHC    CLI   REPATTB,0           TEST IF REPORT ATTRIBUTES INPUT              
         BNE   SRCHC1                                                           
         CLI   REPTYP1,0           OR REPORT TYPES                              
         BNE   SRCHC1                                                           
         CLI   REPTYP2,0                                                        
         BE    SRCHD                                                            
SRCHC1   TM    REPATTB,PQATPW      WAS PASSWORD INPUT                           
         BZ    *+12                                                             
         TM    PQATTB,PQATPW       YES TEST IF PASSWORD REPORT                  
         BZ    SRCHX                                                            
         TM    REPATTB,PQATWIDE    WAS WIDE INPUT                               
         BZ    *+12                                                             
         TM    PQATTB,PQATWIDE     YES TEST IF WIDE REPORT                      
         BZ    SRCHX                                                            
         TM    REPATTB,PQATERR     WAS ERROR INPUT                              
         BZ    *+12                                                             
         TM    PQATTB,PQATERR      YES TEST IF ERROR REPORT                     
         BZ    SRCHX                                                            
         TM    REPATTB,PQATJOBI    YES TEST IF JOBI INPUT                       
         BZ    *+12                                                             
         TM    PQATTB,PQATJOBI     YES TEST IF JCL REPORT                       
         BZ    SRCHX                                                            
         TM    REPATTB,PQATJOBO    WAS JOBO INPUT                               
         BZ    *+12                                                             
         TM    PQATTB,PQATJOBO     YES TEST IF JOB OUT REPORT                   
         BZ    SRCHX                                                            
         TM    REPATTB,PQATSHIT    WAS SHIT INPUT                               
         BNO   *+12                                                             
         TM    PQATTB,PQATSHIT     YES TEST IF SHIT REPORT                      
         BNO   SRCHX                                                            
*                                                                               
         TM    REPTYP2,PQTYUPDT    WAS UPDATE INPUT                             
         BZ    *+12                                                             
         TM    PQTYPE,PQTYUPDT     YES TEST UPDATIVE TYPE                       
         BZ    SRCHX                                                            
         TM    REPTYP2,PQTYONL     WAS NOW INPUT                                
         BZ    *+20                                                             
         TM    PQTYPE,PQTYONL      YES TEST OF ONLINE TYPE                      
         BZ    SRCHX                                                            
         TM    PQATTB,PQATJOBO     AND NOT SOON                                 
         BO    SRCHX                                                            
         TM    REPTYP2,PQTYXTN     WAS EXT INPUT                                
         BZ    *+12                                                             
         TM    PQTYPE,PQTYXTN      REJECT IF NOT EXTENDED                       
         BZ    SRCHX                                                            
         TM    REPTYP2,PQTYDL      WAS DOWNL INPUT                              
         BZ    *+12                                                             
         TM    PQTYPE,PQTYDL       YES TEST DOWNL                               
         BZ    SRCHX                                                            
         TM    REPTYP2,PQTYSQL     WAS SQL INPUT                                
         BZ    *+12                                                             
         TM    PQTYPE,PQTYSQL      YES TEST SQL                                 
         BZ    SRCHX                                                            
         TM    REPTYP2,X'04'       WAS OVERNIGHT INPUT                          
         BZ    *+12                                                             
         TM    PQTYPE,PQTYONL      IGNORE ONLINE REPORTS                        
         BO    SRCHX                                                            
*                                                                               
         TM    REPTYP1,PQTYAE+PQTYAR+PQTYAD                                     
         BNO   *+16                                                             
         TM    PQTYP1,PQTYAE+PQTYAR+PQTYAD                                      
         BZ    SRCHX                                                            
         B     SRCHD                                                            
         TM    REPTYP1,PQTYAE      WAS ARCHIVE ELEGIBLE INPUT                   
         BZ    *+12                                                             
         TM    PQTYP1,PQTYAE       YES TEST AE                                  
         BZ    SRCHX                                                            
         TM    REPTYP1,PQTYAR      WAS ARCHIVABLE INPUT                         
         BZ    *+12                                                             
         TM    PQTYP1,PQTYAR       YES TEST AR                                  
         BZ    SRCHX                                                            
         TM    REPTYP1,PQTYAD      WAS ARCHIVED INPUT                           
         BZ    *+12                                                             
         TM    PQTYP1,PQTYAD       YES TEST AD                                  
         BZ    SRCHX                                                            
         TM    REPTYP1,PQTYBKU     WAS BACKUP INPUT                             
         BZ    *+12                                                             
         TM    PQTYP1,PQTYBKU      YES TEST BACKED UP                           
         BZ    SRCHX                                                            
         TM    REPTYP1,PQTYREP     WAS REP INPUT                                
         BZ    *+12                                                             
         TM    PQTYP1,PQTYREP      YES TEST CREATED BY REP SYSTEM               
         BZ    SRCHX                                                            
         TM    REPTYP1,PQTYNCD     WAS NCD INPUT                                
         BZ    *+12                                                             
         TM    PQTYP1,PQTYNCD      YES TEST NEW COMP DATES                      
*NOP     BZ    SRCHX                                                            
         BNZ   SRCHX               Swap this to turn NCD to OCD                 
SRCHD    EQU   *                                                                
*                                                                               
         XC    CIADDR,CIADDR       FILTERING ON DATA IN REPORT ITSELF           
*                                                                               
         OC    PQSRPID,PQSRPID     TEST IF PID=* TO FILTER ON MY PID            
         BZ    SRCHL                                                            
         CLC   PQSRPID,FFS         ALL VALID IF DDS PASSWORD INPUT              
         BE    SRCHL                                                            
         CLI   QIND,1              ONLY IF READING PART#1                       
         BNE   SRCHL                                                            
         CLI   NDXRT,0                                                          
         BNE   SRCHL                                                            
         OC    USERID,USERID       ONLY IF SPECIFIC USERID DEFINED              
         BZ    SRCHL                                                            
*                                                                               
         SAM31                                                                  
         BRAS  RE,FIRNC                                                         
         MVC   CIADDR,FIWCIA                                                    
         SAM24                                                                  
*                                                                               
         L     RF,ACIREC1                                                       
         GOTO1 ADATAMGR,DMCB,(X'00',DMREAD),PRTQID,CIADDR,(RF)                  
         CLI   8(R1),0                                                          
         BNE   SRCHX                                                            
         L     RF,ACIREC1                                                       
         CLC   PQSRPID,PQPIDNUM-PQRECD(RF)                                      
         BNE   SRCHX                                                            
         AP    QPID,=P'1'          BUMP REPORTS THAT MATCH ON PID               
*                                                                               
SRCHL    OC    PQSRREF,PQSRREF     TEST IF REF=ABC IN REPORT ID                 
         BZ    SRCHM                                                            
         CLI   QIND,1              ONLY IF READING PART#1                       
         BNE   SRCHM                                                            
         CLI   NDXRT,0                                                          
         BNE   SRCHM                                                            
         OC    USERID,USERID       ONLY IF SPECIFIC USERID DEFINED              
         BZ    SRCHM                                                            
         MVC   FULL,CIADDR                                                      
*                                                                               
         SAM31                                                                  
         BRAS  RE,FIRNC                                                         
         MVC   CIADDR,FIWCIA                                                    
         SAM24                                                                  
*                                                                               
         CLC   FULL,CIADDR                                                      
         BE    SRCHL1                                                           
         L     RF,ACIREC1                                                       
         GOTO1 ADATAMGR,DMCB,(X'00',DMREAD),PRTQID,CIADDR,(RF)                  
         CLI   8(R1),0                                                          
         BNE   SRCHX                                                            
SRCHL1   L     RF,ACIREC1                                                       
         CLI   PQSRREF+1,C'*'      TEST MATCH ON FIRST CHR ONLY                 
         BE    SRCHL2                                                           
         CLI   PQSRREF+1,C' '                                                   
         BNE   SRCHL3                                                           
SRCHL2   CLC   PQSRREF(1),PQMAKER-PQRECD(RF)                                    
         BNE   SRCHX                                                            
         B     SRCHM                                                            
SRCHL3   CLC   PQSRREF(3),PQMAKER-PQRECD(RF)                                    
         BNE   SRCHX                                                            
*                                                                               
SRCHM    TM    PQATTB,PQATPW       TEST IF SECURE REPORT                        
         BZ    SRCHN                                                            
         CLC   PQSRPID,FFS         ALL VALID IF DDS PASSWORD INPUT              
         BE    SRCHN                                                            
         CLI   QIND,1              ONLY IF READING PART#1                       
         BNE   SRCHN                                                            
         CLI   NDXRT,0                                                          
         BNE   SRCHN                                                            
         OC    USERID,USERID       ONLY IF SPECIFIC USERID DEFINED              
         BZ    SRCHN                                                            
         MVC   FULL,CIADDR                                                      
*                                                                               
         SAM31                                                                  
         BRAS  RE,FIRNC                                                         
         MVC   CIADDR,FIWCIA                                                    
         SAM24                                                                  
*                                                                               
         CLC   FULL,CIADDR                                                      
         BE    SRCHM1                                                           
         L     RF,ACIREC1                                                       
         GOTO1 ADATAMGR,DMCB,(X'00',DMREAD),PRTQID,CIADDR,(RF)                  
         CLI   8(R1),0                                                          
         BNE   SRCHX                                                            
SRCHM1   L     RF,ACIREC1                                                       
         TM    PQSECF1-PQRECD(RF),PQSINONO TEST IF SEC FLAGS VALID              
         BO    SRCHN                                                            
         TM    PQSECF1-PQRECD(RF),PQSIPID  TEST IF PID PROTECTED                
         BZ    SRCHN                                                            
         CLC   TRMPID,PQPSWD-PQRECD(RF)                                         
         BNE   SRCHX               DONT SHOW OTHER PERSONS PID PROTS            
*                                                                               
SRCHN    EQU   *                                                                
*                                                                               
SRCHT    CP    QT,QTMAX            ROOM IN TABLE                                
         BL    SRCHU               YES                                          
         CP    QX,=P'0'            NO SAVE CI ADDR AND BUMP NO ROOM             
         BNE   SRCHT1                                                           
*                                                                               
         SAM31                                                                  
         BRAS  RE,FIRNC                                                         
         MVC   CIADDR,FIWCIA                                                    
         SAM24                                                                  
*                                                                               
         LH    R0,CIADDR                                                        
         CVD   R0,DUB                                                           
         ZAP   QY,DUB              QY=TTTT OF FIRST NO ROOM CI                  
SRCHT1   AP    QX,=P'1'            QX=COUNT OF NO ROOM ENTRYS                   
         B     SRCHX                                                            
*                                                                               
SRCHU    TM    DDS1,DDSTOT         BUILD NORMAL SAVE TABLE ENTRY                
         BO    SRCHV                                                            
*                                                                               
         SAM31                                                                  
         BRAS  RE,FIRNC                                                         
         MVC   CIADDR,FIWCIA                                                    
         SAM24                                                                  
*                                                                               
         XC    0(L'RTEDATA,R6),0(R6)                                            
         MVC   RTEUSER-RTED(2,R6),PQSRCID                                       
SRCHU1   CLI   IFSORV,X'02'        CREATED ASCENDING                            
         BNE   SRCHU2                                                           
         MVC   RTESORT+2-RTED(2,R6),PQAGELT                                     
         MVC   RTESORT-RTED(2,R6),PQAGELD                                       
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    SRCHU9                                                           
*TEMP                                                                           
         TM    PQAGELD,X'80'                                                    
         JZ    SRCHU9                                                           
*TEMP                                                                           
         GOTO1 ADATCON,DMCB,(2,RTESORT-RTED(R6)),(30,RTESORT-RTED(R6))          
         B     SRCHU9                                                           
*                                                                               
SRCHU2   CLI   IFSORV,X'01'        ALPHA ON REPORT ID                           
         BNE   SRCHU3                                                           
         MVC   RTESORT-RTED(3,R6),PQSUBID                                       
         B     SRCHU9                                                           
SRCHU3   CLI   IFSORV,X'03'        CREATED DESCENDING                           
         BNE   SRCHU4                                                           
         ICM   R0,15,FFS                                                        
         ICM   R1,12,PQAGELD                                                    
         ICM   R1,3,PQAGELT                                                     
         SR    R0,R1                                                            
         STCM  R0,15,RTESORT-RTED(R6)                                           
         B     SRCHU9                                                           
SRCHU4   CLI   IFSORV,X'04'        PRINTED ASCENDING                            
         BNE   SRCHU5                                                           
         MVC   RTESORT-RTED(2,R6),FFS                                           
         OC    PQAGEDD,PQAGEDD     TEST IF LAST PRTD DATE DEFINED               
         BZ    SRCHU9                                                           
         CLI   PQAGERT,X'FE'       TEST IF PRINTING NOW                         
         BE    SRCHU9                                                           
         MVC   RTESORT-RTED(2,R6),PQAGEDD                                       
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    SRCHU9                                                           
         GOTO1 ADATCON,DMCB,(2,RTESORT-RTED(R6)),(30,RTESORT-RTED(R6))          
         B     SRCHU9                                                           
*                                                                               
SRCHU5   CLI   IFSORV,X'05'        PRINTED DESCENDING                           
         BNE   SRCHU6                                                           
         MVC   RTESORT-RTED(2,R6),FFS                                           
         OC    PQAGEDD,PQAGEDD     TEST IF LAST PRTD DATE DEFINED               
         BZ    SRCHU9                                                           
         CLI   PQAGERT,X'FE'       TEST IF PRINTING NOW                         
         BE    SRCHU9                                                           
         SR    R0,R0                                                            
         ICM   R0,3,FFS            GET INVERT VALUE                             
         MVC   HALF3,PQAGEDD                                                    
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    SRCHU5A                                                          
         GOTO1 ADATCON,DMCB,(2,HALF3),(30,HALF3)                                
SRCHU5A  SR    R1,R1                                                            
         ICM   R1,3,HALF3                                                       
         SR    R0,R1                                                            
         STCM  R0,3,RTESORT-RTED(R6)                                            
         B     SRCHU9                                                           
SRCHU6   CLI   IFSORV,X'06'        RETAINED ASCENDING                           
         BNE   SRCHU6A                                                          
         MVC   RTESORT-RTED(3,R6),PQAGERD                                       
         B     SRCHU9                                                           
SRCHU6A  CLI   IFSORV,X'07'        RETAINED DESCENDING                          
         BNE   SRCHU7                                                           
         SR    R0,R0                                                            
         ICM   R0,7,FFS            GET INVERT VALUE                             
         SR    R1,R1                                                            
         ICM   R1,7,PQAGERD                                                     
         SR    R0,R1                                                            
         STCM  R0,7,RTESORT-RTED(R6)                                            
         B     SRCHU9                                                           
SRCHU7   CLI   IFSORV,X'08'        SIZE ASCENDING                               
         BNE   SRCHU7A                                                          
         MVC   RTESORT-RTED(1,R6),PQAGES                                        
         B     SRCHU9                                                           
SRCHU7A  CLI   IFSORV,X'09'        SIZE DESCENDING                              
         BNE   SRCHU8                                                           
         LA    R1,255                                                           
         SR    R0,R0                                                            
         IC    R0,PQAGES                                                        
         SR    R1,R0                                                            
         STC   R1,RTESORT-RTED(R6)                                              
         B     SRCHU9                                                           
SRCHU8   DC    H'0'                                                             
SRCHU9   MVC   RTEREPNO-RTED(2,R6),PQREPNO                                      
         MVC   RTESTAT-RTED(1,R6),PQSTAT                                        
         MVC   RTETYP1-RTED(1,R6),PQTYP1                                        
         MVC   RTECIADR-RTED(2,R6),CIADDR                                       
         LA    R6,L'RTEDATA(R6)                                                 
         AP    QT,=P'1'                                                         
         B     SRCHX                                                            
*                                                                               
SRCHV    L     R6,AREPTAB          BUILD COUNT SAVE TABLE ENTRY                 
         CLC   0(L'RTEDATA,R6),FFS                                              
         BE    SRCHV4                                                           
         CLC   PQSRCID,RTEUSER-RTED(R6)                                         
         BE    *+12                                                             
         LA    R6,L'RTEDATA(R6)                                                 
         B     SRCHV+4                                                          
SRCHV2   LH    RE,RTESORT-RTED(R6) UPDATE COUNT IN EXISTING ENTRY               
         LA    RE,1(RE)                                                         
         STH   RE,RTESORT-RTED(R6)                                              
         CLC   PQREPNO,RTEREPNO-RTED(R6)                                        
         BL    SRCHX                                                            
         B     SRCHV6              SAVE HIGHEST SEQ NUM                         
SRCHV4   AP    QT,=P'1'            ADD NEW TABLE ENTRY FOR USERID               
         MVC   L'RTEDATA(L'RTEDATA,R6),FFS                                      
         MVC   RTEUSER-RTED(2,R6),PQSRCID                                       
         MVC   RTESORT-RTED(3,R6),=X'000100'                                    
SRCHV6   MVC   RTEREPNO-RTED(2,R6),PQREPNO                                      
                                                                                
         SAM31                                                                  
         XC    FIWREF,FIWREF                                                    
         MVC   FIWREF+2(2),PQREPNO                                              
         BRAS  RE,FIRRN                                                         
         BRAS  RE,FIRRC                                                         
         MVC   CIADDR,FIWCIA                                                    
         MVC   RTECIADR-RTED(2,R6),CIADDR                                       
*                                                                               
SRCHX    B     SRCH1               BUMP TO NEXT INDEX ENTRY                     
*                                                                               
SRCHXX   SAM24                                                                  
         CLI   QIND,2                                                           
         BE    SRCHY                                                            
         CLI   ACTN,X'28'                                                       
         BNE   SRCHY                                                            
         MVI   QIND,2              SET TO SERCH PART2 FOR SIZE ACTION           
         MVC   FIWNDA,FIWP2A       POINT TO START OF PART2 INDEX                
         SAM31                                                                  
         B     SRCH1                                                            
*                                                                               
SRCHY    TM    DDS1,DDSTOT                                                      
         BO    SORT+6                                                           
         B     SORT                                                             
*                                                                               
SORT     MVC   0(L'RTEDATA,R6),FFS SET END OF TABLE AND SORT ON KEY             
         XC    FULL,FULL                                                        
         ZAP   DUB,QT                                                           
         CVB   R6,DUB                                                           
         STH   R6,PQSTOTL                                                       
         LA    R6,1(R6)                                                         
         CH    R6,=H'2'                                                         
         BNH   SORTX                                                            
         L     R0,AREPTAB                                                       
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+15,7           LENGTH                                       
         MVI   DMCB+19,0           DISPLACEMENT                                 
         CLI   BYTE1,C'X'                                                       
         BNE   SORT1               EXCLUDE USERID                               
         MVI   DMCB+15,5           ON XSORT                                     
         MVI   DMCB+19,2                                                        
         TM    DDS1,DDSTOT         TEST FOR TOTALS                              
         BNO   SORT1                                                            
         CLI   IFSORV,X'03'        TEST FOR XSORT=-C                            
         BNE   SORT1                                                            
         ICM   R0,8,FFS            REVERSE SORT                                 
         LR    R1,R6                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'12'           L'RTEDATA                                    
         AR    R1,R0                                                            
         XC    0(L'RTEDATA,R1),0(R1)  XC END OF TABLE                           
         ST    R1,FULL                                                          
SORT1    GOTO1 =V(XSORT),DMCB,(R0),(R6),12,,,RR=RELO                            
         ICM   R1,15,FULL                                                       
         BZ    SORTX                                                            
         MVC   0(L'RTEDATA,R1),FFS RESET END OF TABLE                           
*                                                                               
SORTX    CLI   ACTN,X'28'          SIZE ACTION                                  
         BNE   SAVE1                                                            
*                                                                               
SIZE010  XC    SRVP2,SRVP2         CLEAR P2                                     
         MVI   BYTE,0                                                           
         CLI   ACTN1,4             SIZE,SCAN                                    
         JNE   SIZE015                                                          
         MVI   BYTE,3              BYTE=3 IF SIZE,SCAN                          
         MVI   ACTN1,0                                                          
*                                                                               
         MVC   SRVP1,SPACES        CLEAR P1                                     
         MVC   SRVP1(4),=C'SIZE'   REPLACE WITH SIZE                            
         CLI   PQSFLAG,C'%'                                                     
         JNE   SIZE015                                                          
         MVC   SRVP1+4(2),=C',%'   OR SIZE,%                                    
         MVI   ACTN1,1             RESTORE SIZE,%                               
*                                                                               
SIZE015  L     RF,=A(NSIZE)                                                     
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
         SAM31                                                                  
         L     R7,FIWSHA           SHARED MEMORY AREA                           
         USING SIHDRD,R7                                                        
         L     R0,SIHNOFR          NUMBER OF RESOURCES                          
         LA    R7,L'SIHDR(,R7)                                                  
         USING SITABD,R7                                                        
         LA    R4,SRVSA1H          POINT TO 1ST SEL FIELD                       
         USING SIZED,R4                                                         
*                                                                               
SIZE020  CLI   BYTE,3              SIZE,SCAN                                    
         JE    SIZE025             SET 1 FLAG                                   
         CLI   SZSHDR+5,1                                                       
         JNE   SIZE030                                                          
         CLI   SZACT,C'S'          S OR 1                                       
         JE    *+12                                                             
         CLI   SZACT,C'1'                                                       
         JNE   *+8                                                              
SIZE025  OI    SITRIND,SITRP1S     SET P1 REQUIRED                              
         CLI   SZACT,C'2'          2                                            
         JNE   *+8                                                              
         OI    SITRIND,SITRP2S     SET P2 REQUIRED                              
         OI    BYTE,1                                                           
*                                                                               
SIZE030  XC    SZACT,SZACT                                                      
         LA    R7,L'SITAB(,R7)                                                  
         LA    R4,93(R4)                                                        
         JCT   R0,SIZE020          DO FOR EACH RESOURCE                         
         CLI   BYTE,0                                                           
         JE    SIZEX                                                            
         DROP  R7                                                               
*                                                                               
         L     R7,FIWSHA                                                        
         USING SIHDRD,R7                                                        
         XR    R3,R3                                                            
         ICM   R3,3,SIHMASID                                                    
         LOCASCB ASID=(R3)         LOCATE ASCB FROM ASID                        
         LTR   RF,RF                                                            
         JNZ   SIZEX                                                            
*                                                                               
         LR    R4,R1                                                            
         USING ASCB,R4                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         JNE   SIZEX                                                            
*                                                                               
         L     R3,SIHMECB          POST ALERT TO PQMON                          
         POST  (R3),99,ASCB=(R4),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTPA)          
         LTR   RF,RF                                                            
*                                                                               
SIZEX    SAM24                                                                  
         B     EXIT                                                             
*                                                                               
POSTPA   POST  ECBKEY=YES,MF=L                                                  
*                                                                               
         EJECT                                                                  
                                                                                
SAVE1    TM    INTFLAG,INTLAST     TEST LAST SUB ACTION                         
         BO    INDIV                                                            
         TM    INTFLAG,INTRUN      TEST INTERNAL ACTION                         
         BO    INDIV                                                            
         MVI   FLAG,0              00=FIRST 01=NEXT FF=LAST                     
         LA    RF,PQSCIADS         RF = A(SAVE AREA)                            
         L     RE,AREPTAB          RE = A(REPTAB POSITION)                      
         LA    R1,600                                                           
SAVE1A   CLC   0(L'RTEDATA,RE),FFS                                              
         BE    SAVE1B                                                           
         TM    DDS1,DDSTOT                                                      
         BO    SAVE1A1                                                          
         MVC   RTESORT-RTED(4,RE),=X'00000000'                                  
         TM    RTESTAT-RTED(RE),PQSTPG                                          
         BNO   *+8                                                              
         OI    RTESORT-RTED(RE),X'80'                                           
         MVC   2(2,RF),RTESORT-RTED(RE)                                         
SAVE1A1  MVC   4(2,RF),RTECIADR-RTED(RE)                                        
         MVC   0(2,RF),RTEREPNO-RTED(RE)                                        
         TM    DDS1,DDSTOT                                                      
         BNO   *+10                    SAVE COUNTS FOR TOTALS                   
         MVC   0(2,RF),RTESORT-RTED(RE)                                         
         LA    RE,L'RTEDATA(RE)                                                 
         LA    RF,6(RF)                                                         
         BCT   R1,SAVE1A                                                        
SAVE1B   LA    RE,600                                                           
         SR    RE,R1               RE=NUM REPORTS SAVED                         
         STCM  RE,3,PQSBLKSI                                                    
*                                                                               
SAVE2    OC    PQINDX,PQINDX                                                    
         BZ    *+8                                                              
         MVI   FLAG,X'01'                                                       
         MVC   0(6,RF),FFS                                                      
*                                                                               
SAVEX    EQU   *                                                                
*                                                                               
*----------------------------------                                             
* INDIVIDUAL REPORT FUNCTIONS                                                   
*----------------------------------                                             
INDIV    EQU   *                                                                
         OC    PQSSEQ,PQSSEQ                                                    
         BZ    GROUP                                                            
         CP    QT,=P'1'            INDIVIDUAL REPORT FUNCTIONS                  
         BH    GROUP                                                            
INDIV0   MVC   STAACT,SPACES                                                    
         XC    NDX,NDX                                                          
         MVI   QIND,0              SET NO STATUS CHANGE                         
         MVI   BYTE,0              CLEAR ERROR FLAG                             
         LA    R4,SRVP2H                                                        
         L     R1,AREPTAB                                                       
         CLC   PQSSEQ,RTEREPNO-RTED(R1) MUST BE FIRST IN TABLE                  
         BNE   ERR6                                                             
         MVC   CISTAT,RTESTAT-RTED(R1)                                          
         MVC   CITYP1,RTETYP1-RTED(R1)                                          
         MVC   CIADDR(2),RTECIADR-RTED(R1)                                      
         MVI   CIADDR+2,1                                                       
         MVI   CIADDR+3,0                                                       
*                                                                               
INTACT   LA    R5,CXREC            READ RECORD INTO INDEX AREA                  
         USING PQRECD,R5                                                        
         XC    SAVE(12),SAVE                                                    
         MVC   SAVE+4(4),=C'LINE'                                               
         GOTO1 ADATAMGR,DMCB,(X'00',BUFFER),PRTQID,NDX,SAVE,(R5)                
         CLI   8(R1),0                                                          
         BNE   INDERR                                                           
         L     RE,8(R5)            SET DISK ADDR IN SAVE AREA                   
         AR    RE,R5                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKINTNO-SKBUFFD(1,RE),CFPQINUM                                   
         MVC   SKEXTNO-SKBUFFD(1,RE),BUPQFILE                                   
         GOTO1 ADATAMGR,DMCB,(X'00',RANDOM)                                     
         CLI   8(R1),0                                                          
         BNE   INDERR                                                           
         MVC   SVPQREC,0(R5)       SAVE REPORT DATA BEFORE ACTION               
         B     INDH                                                             
*                                                                               
INDH     CLI   ACTN,X'22'          HOLD                                         
         BNE   INDHX                                                            
         MVC   STAACT(3),=C'HOL'                                                
         TM    CISTAT,PQSTPG                                                    
         BO    ERR14                                                            
         TM    CISTAT,PQSTAC                                                    
         BZ    ERR7                MUST BE ACTIVE                               
         NI    CISTAT,255-PQSTAC                                                
         OI    CISTAT,PQSTHO                                                    
         SP    QA,=P'1'                                                         
         AP    QH,=P'1'                                                         
         B     INDUPD                                                           
INDHX    DS    0H                                                               
*                                                                               
INDPR    CLI   ACTN,X'2A'          PRINTED                                      
         BNE   INDPRX                                                           
         MVC   STAACT(6),=C'PRIRET'                                             
         TM    CISTAT,PQSTPG                                                    
         BO    ERR14                                                            
         TM    CISTAT,PQSTAC                                                    
         BZ    ERR7                MUST BE ACTIVE                               
         NI    CISTAT,255-PQSTAC                                                
         OI    CISTAT,PQSTPR                                                    
         SP    QA,=P'1'                                                         
         AP    QD,=P'1'                                                         
         BRAS  RE,PRINTED                                                       
         B     INDUPD                                                           
INDPRX   DS    0H                                                               
*                                                                               
INDP     CLI   ACTN,X'23'          PURGE                                        
         BNE   INDPX                                                            
         MVC   STAACT(3),=C'PUR'                                                
         TM    CISTAT,PQSTPG                                                    
         BO    ERR14                                                            
*&&US                                                                           
TRAPUS1  CLC   PQSRCID,=AL2(9158)  TEST IF GMMRE OVERNIGHT                      
         BNE   TRAPUS1X                                                         
         TM    PQTYPE,PQTYONL                                                   
         B     *+6                 *NOP* BO                                     
         DC    H'0'                                                             
TRAPUS1X EQU   *                                                                
*                                                                               
         CLI   IFDDSV,C'E'         ONLY PURGE CLASS N IF DDS=E                  
         BE    INDPA                                                            
         CLI   PQCLASS,C'N'                                                     
         BE    ERR15                                                            
         CLI   PQCLASS,C'G'                                                     
         BE    ERR15                                                            
INDPA    DS    0H                                                               
*&&                                                                             
         TM    CISTAT,PQSTAC                                                    
         BZ    *+10                                                             
         SP    QA,=P'1'                                                         
         TM    CISTAT,PQSTHO                                                    
         BZ    *+10                                                             
         SP    QH,=P'1'                                                         
         TM    CISTAT,PQSTPR                                                    
         BZ    *+10                                                             
         SP    QD,=P'1'                                                         
         TM    CISTAT,PQSTSE                                                    
         BZ    *+10                                                             
         SP    QE,=P'1'                                                         
         TM    CISTAT,PQSTKE                                                    
         BZ    *+10                                                             
         SP    QK,=P'1'                                                         
         TM    CISTAT,PQSTIN                                                    
         BZ    *+10                                                             
         SP    QI,=P'1'                                                         
         MVI   CISTAT,PQSTPU                                                    
         SP    QR,=P'1'                                                         
         B     INDUPD                                                           
INDPX    DS    0H                                                               
*                                                                               
INDA     CLI   ACTN,X'24'          ACTIVATE                                     
         BNE   INDAX                                                            
         MVC   STAACT(3),=C'ACT'                                                
         CLI   ACTN1,1             TEST FOR ACTIVATE,RETAIN                     
         BNE   *+10                                                             
         MVC   STAACT+3(3),=C'RET'                                              
         TM    CISTAT,PQSTPG                                                    
         BZ    INDA1                                                            
         CLC   SRVP3(8),SR@PNTG                                                 
         BNE   ERR14                                                            
         NI    CISTAT,255-PQSTPG   CAN BE PNTG (IF SPELLED OUT)                 
         SP    QG,=P'1'                                                         
         B     INDUPD                                                           
INDA1    TM    CISTAT,PQSTHO                                                    
         BZ    INDA2                                                            
         NI    CISTAT,255-PQSTHO   CAN BE HOLD                                  
         OI    CISTAT,PQSTAC                                                    
         SP    QH,=P'1'                                                         
         AP    QA,=P'1'                                                         
         B     INDUPD                                                           
INDA2    TM    CISTAT,PQSTPR                                                    
         BZ    INDA3                                                            
*&&US                                                                           
         CLI   IFDDSV,C'E'         ONLY ACTV CLASS N IF DDS=E                   
         BE    *+12                                                             
         CLI   PQCLASS,C'N'                                                     
         BE    ERR15                                                            
*&&                                                                             
         NI    CISTAT,255-PQSTPR   CAN BE PRINTED                               
         OI    CISTAT,PQSTAC                                                    
         SP    QD,=P'1'                                                         
         AP    QA,=P'1'                                                         
         B     INDUPD                                                           
INDA3    TM    CISTAT,PQSTSE                                                    
         BZ    ERR8                                                             
*&&US                                                                           
         CLI   IFDDSV,C'E'         ONLY ACTV CLASS N IF DDS=E                   
         BE    *+12                                                             
         CLI   PQCLASS,C'N'                                                     
         BE    ERR15                                                            
*&&                                                                             
         NI    CISTAT,255-PQSTSE   CAN BE SENT                                  
         OI    CISTAT,PQSTAC                                                    
         SP    QE,=P'1'                                                         
         AP    QA,=P'1'                                                         
         B     INDUPD                                                           
INDAX    DS    0H                                                               
*                                                                               
INDK     CLI   ACTN,X'25'          KEEP                                         
         BNE   INDKX                                                            
         MVC   STAACT(3),=C'KEE'                                                
         TM    CISTAT,PQSTKE       CANT BE KEEP                                 
         BO    ERR9                                                             
         OI    CISTAT,PQSTKE                                                    
         AP    QK,=P'1'                                                         
         B     INDUPD                                                           
INDKX    DS    0H                                                               
*                                                                               
INDUK    CLI   ACTN,X'26'          UNKEEP                                       
         BNE   INDUKX                                                           
         MVC   STAACT(3),=C'UNK'                                                
         TM    CISTAT,PQSTKE       MUST BE KEEP                                 
         BZ    ERR10                                                            
         NI    CISTAT,255-PQSTKE                                                
         SP    QK,=P'1'                                                         
         B     INDUPD                                                           
INDUKX   DS    0H                                                               
*                                                                               
INDV     CLI   ACTN,X'2C'          VISIBLE                                      
         BNE   INDVX                                                            
         MVC   STAACT(3),=C'VIS'                                                
         TM    CISTAT,PQSTIN       MUST BE INVISIBLE                            
         BZ    ERR16                                                            
         NI    CISTAT,255-PQSTIN                                                
         SP    QI,=P'1'                                                         
         B     INDUPD                                                           
INDVX    DS    0H                                                               
*                                                                               
INDUV    CLI   ACTN,X'2D'          INVISIBLE                                    
         BNE   INDUVX                                                           
         MVC   STAACT(3),=C'INV'                                                
         TM    CISTAT,PQSTIN       MUST BE VISIBLE                              
         BO    ERR17                                                            
         OI    CISTAT,PQSTIN                                                    
         AP    QI,=P'1'                                                         
         B     INDUPD                                                           
INDUVX   DS    0H                                                               
*                                                                               
ARCA     CLI   ACTN,X'41'          ARCHIVABLE                                   
         BNE   ARCAX                                                            
         MVC   STAACT(3),=C'ARS'                                                
         TM    DDS,DDSTRM                                                       
         BO    ARCA1                                                            
         TM    CITYP1,PQTYAE       MUST BE ELIGIBLE FOR ARCHIVE                 
         BZ    ERR18                                                            
ARCA1    OI    CITYP1,PQTYAR                                                    
         NI    CITYP1,255-PQTYAE-PQTYAD                                         
         B     INDUPD                                                           
ARCAX    DS    0H                                                               
*                                                                               
UNARCA   CLI   ACTN,X'42'          UNARCHIVABL                                  
         BNE   UNARCAX                                                          
         MVC   STAACT(3),=C'ARU'                                                
         TM    CITYP1,PQTYAR       MUST BE ARCHIVABLE                           
         BZ    ERR19                                                            
         NI    CITYP1,255-PQTYAR                                                
         B     INDUPD                                                           
UNARCAX  DS    0H                                                               
*                                                                               
ARCD     CLI   ACTN,X'43'          ARCHIVED                                     
         BNE   ARCDX                                                            
         MVC   STAACT(3),=C'ADS'                                                
         OI    CITYP1,PQTYAD                                                    
         NI    CITYP1,255-PQTYAE-PQTYAR                                         
         B     INDUPD                                                           
ARCDX    DS    0H                                                               
*                                                                               
UNARCD   CLI   ACTN,X'44'          UNARCHIVED                                   
         BNE   UNARCDX                                                          
         MVC   STAACT(3),=C'ADU'                                                
         TM    CITYP1,PQTYAD       MUST BE ARCHIVED                             
         BZ    ERR19                                                            
         NI    CITYP1,255-PQTYAD                                                
         B     INDUPD                                                           
UNARCDX  DS    0H                                                               
*                                                                               
ARCE     CLI   ACTN,X'45'          ELIGIBLE FOR ARCHIVE                         
         BNE   ARCEX                                                            
         MVC   STAACT(3),=C'AES'                                                
         OI    CITYP1,PQTYAE                                                    
         NI    CITYP1,255-PQTYAR-PQTYAD                                         
         B     INDUPD                                                           
ARCEX    DS    0H                                                               
*                                                                               
UNARCE   CLI   ACTN,X'46'          UNELIBLE FOR ARCHIVE                         
         BNE   UNARCEX                                                          
         MVC   STAACT(3),=C'AEU'                                                
         TM    CITYP1,PQTYAE       MUST BE ELIGIBLE FOR ARCHIVE                 
         BZ    ERR19                                                            
         NI    CITYP1,255-PQTYAE                                                
         B     INDUPD                                                           
UNARCEX  DS    0H                                                               
*                                                                               
BKUP     CLI   ACTN,X'47'          BACKUP TO TAPE                               
         BNE   BKUPX                                                            
         MVC   STAACT(3),=C'BKS'                                                
         OI    CITYP1,PQTYBKU                                                   
         B     INDUPD                                                           
BKUPX    DS    0H                                                               
*                                                                               
UNBKUP   CLI   ACTN,X'48'          NOT BACKED UP TO TAPE                        
         BNE   UNBKUPX                                                          
         MVC   STAACT(3),=C'BKU'                                                
         TM    CITYP1,PQTYBKU      MUST BE BACKED UP ALREADY                    
         BZ    ERR20                                                            
         NI    CITYP1,255-PQTYBKU                                               
         B     INDUPD                                                           
UNBKUPX  DS    0H                                                               
*                                                                               
SETNCD   CLI   ACTN,X'49'          SET NEW COMP DATE                            
         BNE   SETNCDX                                                          
         MVC   STAACT(3),=C'NCD'                                                
         OI    CITYP1,PQTYNCD                                                   
         B     INDUPD                                                           
SETNCDX  DS    0H                                                               
*                                                                               
SETOCD   CLI   ACTN,X'4A'          UNSET NEW COMP DATE                          
         BNE   SETOCDX                                                          
         MVC   STAACT(3),=C'OCD'                                                
         NI    CITYP1,255-PQTYNCD                                               
         B     INDUPD                                                           
SETOCDX  DS    0H                                                               
*                                                                               
INDEK    CLI   ACTN,X'2B'          UNERROR                                      
         BNE   INDEKX                                                           
         MVC   STAACT(3),=C'UNE'                                                
         B     INDUPD                                                           
INDEKX   DS    0H                                                               
*                                                                               
INDEL    CLI   ACTN,X'2E'          UNSECURE                                     
         BNE   INDELX                                                           
         MVC   STAACT(3),=C'UNS'                                                
         B     INDUPD                                                           
INDELX   DS    0H                                                               
*                                                                               
INDR     CLI   ACTN,X'29'          RETAIN                                       
         BNE   INDRX                                                            
         MVC   STAACT(3),=C'RET'                                                
         CLI   ACTN1,X'FF'         TEST FOR RETAIN HOURS INPUT                  
         BE    INDR1                                                            
         MVI   ACTN2,24            DEFAULT TO ONE DAY                           
INDR1    CLI   ACTN2,168           RETAIN HOURS MUST BE LT ONE WEEK             
         BNH   INDR2                                                            
         TM    DDS,DDSTRM                                                       
         BO    *+12                                                             
         MVI   ACTN2,168           USERS HAVE MAX OF 168 HOURS                  
         B     INDR2                                                            
         CLI   ACTN2,255           DDS MAX HOURS MEANS PERMANENT                
         BNE   INDR2                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FFS            HOURS=X'FFFF' MEANS PERMANENT                
         B     INDR3                                                            
INDR2    SR    R1,R1                                                            
         IC    R1,ACTN2                                                         
INDR3    STCM  R1,3,NXINFO         SET RETAIN HOURS IN INDEX                    
         OI    NXFLAG,X'10'        SET FLAG TO SHOW PASSING HOURS               
         STCM  R1,3,NDX+20         **TEMP** OLD STYLE CALL **REMOVE**           
         MVI   NDX+22,X'FF'        **TEMP** OLD STYLE CALL **REMOVE**           
         B     INDUPD                                                           
*                                                                               
INDRX    DS    0H                                                               
*                                                                               
INDANY   B     INDDSP              DEFAULT ACTION IS DISPLAY                    
*                                                                               
INDUPD   BRAS  RE,PQUPDATE         UPDATE STATUS                                
*                                                                               
         MVI   QIND,1                                                           
         TM    INTFLAG,INTRUN+INTCONT  INTERNAL "DON'T" DISPLAY                 
         BO    INDDSPX2                                                         
         XC    SAVE(12),SAVE       READ NEW RECORD INTO BUFFER                  
         MVC   SAVE+4(4),=C'LINE'                                               
         GOTO1 ADATAMGR,DMCB,(X'00',RANDOM),PRTQID,NDX,SAVE,(R5)                
         CLI   8(R1),0                                                          
         BNE   INDERR                                                           
*                                                                               
INDDSP   BRAS  RE,INDDSPR                                                       
*                                                                               
INDDSPX2 TM    DDS,DDSTRM          TRUNCATE SCREEN FOR NON DDS TERM             
         BO    EXIT                                                             
         TM    INTFLAG,INTCONT     CONTINUE. SCREEN NOT LOADED                  
         BO    EXIT                                                             
         MVC   SRVGRP4H(8),SRVUPROH UNPROT FIELD NEEDED FOR PC VERSION          
         LA    RE,SRVGRP4H                                                      
         MVI   8(RE),C' '                                                       
         XR    RF,RF                                                            
         IC    RF,0(,RE)                                                        
         AR    RE,RF                                                            
         MVC   0(3,RE),=X'000100'                                               
         B     EXIT                                                             
                                                                                
*----------------------------------------------------------------------         
* GROUP REPORT FUNCTIONS                                                        
*----------------------------------------------------------------------         
GROUP    DS    0H                                                               
*                                                                               
         LA    R4,SRVP1H                                                        
         OC    QT,QT                                                            
         BZ    ERR11                                                            
         CP    QT,=P'0'                                                         
         BE    ERR11               NO REPORTS FOUND                             
         ZAP   QO,=P'0'            QO=NUMBER OF REPORTS DISPLAYED               
         ZAP   QS,=P'0'            QS=NUMBER OF STATUS CHANGES                  
         ZAP   QN,=P'0'            QN=NUMBER DISPLAYED SO FAR                   
         XC    FULL,FULL           FULL=LAST USER ID NUM                        
         MVC   SVREPID(2),USERID                                                
         L     R6,AREPTAB          R6=A(NEXT REPTAB ENTRY)                      
         LA    R1,L'RTEDATA                                                     
         MH    R1,PQINDX                                                        
         AR    R6,R1               INDEX INTO TABLE                             
*                                                                               
         TM    DDS1,DDSTOT         TEST FOR V=... FORMAT                        
         BO    GRP1                                                             
         CP    QT,=P'1'            CONVERT TO INDIV IF ONE REPORT ONLY          
         BNE   GRP2                                                             
         B     GRP2                NOP BRANCH TO INDIV0                         
*                                                                               
GRP1     MVCDD SRVHD1,SR#PQUH6     SET TOTAL FORMAT HEADLINES                   
GRP2     MVI   QIND,0              SET NO STATUS CHANGE                         
         MVC   STAACT,SPACES                                                    
         XC    NDX,NDX                                                          
         MVC   CISTAT,RTESTAT-RTED(R6)   SET STATUS FROM INDEX ENTRY            
         MVC   CITYP1,RTETYP1-RTED(R6)                                          
*                                                                               
GRPH     CLI   ACTN,X'22'          HOLD ALL ACTIVES                             
         BNE   GRPHX                                                            
         MVC   STAACT(3),=C'HOL'                                                
         TM    CISTAT,PQSTPG                                                    
         BO    GRPX                                                             
         TM    CISTAT,PQSTAC                                                    
         BZ    GRPX                                                             
         NI    CISTAT,255-PQSTAC                                                
         OI    CISTAT,PQSTHO                                                    
         SP    QA,=P'1'                                                         
         AP    QH,=P'1'                                                         
         B     GRPUPD                                                           
GRPHX    DS    0H                                                               
*                                                                               
GRPPR    CLI   ACTN,X'2A'          PRINT ALL ACTIVES                            
         BNE   GRPPRX                                                           
         MVC   STAACT(6),=C'PRIRET'                                             
         TM    CISTAT,PQSTPG                                                    
         BO    GRPX                                                             
         TM    CISTAT,PQSTAC                                                    
         BZ    GRPX                                                             
         NI    CISTAT,255-PQSTAC                                                
         OI    CISTAT,PQSTPR                                                    
         SP    QA,=P'1'                                                         
         AP    QD,=P'1'                                                         
         BRAS  RE,PRINTED                                                       
         B     GRPUPD                                                           
GRPPRX   DS    0H                                                               
*                                                                               
GRPP     CLI   ACTN,X'23'          PURGE                                        
         BNE   GRPPX                                                            
         MVC   STAACT(3),=C'PUR'                                                
         TM    CISTAT,PQSTPG                                                    
         BO    GRPX                                                             
*                                                                               
TRAP2    CLC   PQSRCID,=AL2(9158)                                               
         BNE   TRAP2X                                                           
         TM    PQTYPE,PQTYONL                                                   
         BO    *+6                                                              
         DC    H'0'                DIE IF PURGE OF GMMRE OVERNIGHT              
TRAP2X   EQU   *                                                                
*                                                                               
*&&US                                                                           
         CLI   IFDDSV,C'E'         ONLY PURGE CLASS N IF DDS=F                  
         BE    *+12                                                             
         CLI   PQCLASS,C'N'                                                     
         BE    GRPX                                                             
*&&                                                                             
         TM    CISTAT,PQSTAC                                                    
         BZ    *+10                                                             
         SP    QA,=P'1'                                                         
         TM    CISTAT,PQSTHO                                                    
         BZ    *+10                                                             
         SP    QH,=P'1'                                                         
         TM    CISTAT,PQSTPR                                                    
         BZ    *+10                                                             
         SP    QD,=P'1'                                                         
         TM    CISTAT,PQSTSE                                                    
         BZ    *+10                                                             
         SP    QE,=P'1'                                                         
         TM    CISTAT,PQSTKE                                                    
         BZ    *+10                                                             
         SP    QK,=P'1'                                                         
         MVI   CISTAT,PQSTPU                                                    
         SP    QR,=P'1'                                                         
         B     GRPUPD                                                           
GRPPX    DS    0H                                                               
*                                                                               
GRPA     CLI   ACTN,X'24'          ACTIVATE ALL HOLDS AND/OR DEADS              
         BNE   GRPAX                                                            
*&&US                                                                           
         CLI   IFDDSV,C'E'         ONLY ACTV CLASS N IF DDS=F                   
         BE    *+12                                                             
         CLI   PQCLASS,C'N'                                                     
         BE    GRPX                                                             
*&&                                                                             
         MVC   STAACT(3),=C'ACT'                                                
         CLI   ACTN1,1             TEST FOR ACTIVATE,RETAIN                     
         BNE   *+10                                                             
         MVC   STAACT+3(3),=C'RET'                                              
         TM    CISTAT,PQSTPG                                                    
         BO    GRPX                                                             
         TM    CISTAT,PQSTHO                                                    
         BZ    GRPA1                                                            
         NI    CISTAT,255-PQSTHO                                                
         OI    CISTAT,PQSTAC                                                    
         SP    QH,=P'1'                                                         
         AP    QA,=P'1'                                                         
         B     GRPUPD                                                           
GRPA1    TM    CISTAT,PQSTPR                                                    
         BZ    GRPA2                                                            
         NI    CISTAT,255-PQSTPR                                                
         OI    CISTAT,PQSTAC                                                    
         SP    QD,=P'1'                                                         
         AP    QA,=P'1'                                                         
         B     GRPUPD                                                           
GRPA2    TM    CISTAT,PQSTSE                                                    
         BZ    GRPX                                                             
         NI    CISTAT,255-PQSTSE                                                
         OI    CISTAT,PQSTAC                                                    
         SP    QE,=P'1'                                                         
         AP    QA,=P'1'                                                         
         B     GRPUPD                                                           
GRPAX    DS    0H                                                               
*                                                                               
GRPK     CLI   ACTN,X'25'          KEEP ALL UNKEEPS                             
         BNE   GRPKX                                                            
         MVC   STAACT(3),=C'KEE'                                                
         OI    CISTAT,PQSTKE                                                    
         AP    QK,=P'1'                                                         
         B     GRPUPD                                                           
GRPKX    DS    0H                                                               
*                                                                               
GRPUK    CLI   ACTN,X'26'          UNKEEP ALL KEEPS                             
         BNE   GRPUKX                                                           
         MVC   STAACT(3),=C'UNK'                                                
         TM    CISTAT,PQSTKE                                                    
         BZ    GRPX                                                             
         NI    CISTAT,255-PQSTKE                                                
         SP    QK,=P'1'                                                         
         B     GRPUPD                                                           
GRPUKX   DS    0H                                                               
*                                                                               
GRPV     CLI   ACTN,X'2C'          VISIBLE ALL INVISIBLES                       
         BNE   GRPVX                                                            
         MVC   STAACT(3),=C'VIS'                                                
         TM    CISTAT,PQSTIN                                                    
         BZ    GRPX                                                             
         NI    CISTAT,255-PQSTIN                                                
         SP    QI,=P'1'                                                         
         B     GRPUPD                                                           
GRPVX    DS    0H                                                               
*                                                                               
GRPUV    CLI   ACTN,X'2D'          INVISIBLE ALL VISIBLES                       
         BNE   GRPUVX                                                           
         MVC   STAACT(3),=C'INV'                                                
         TM    CISTAT,PQSTIN                                                    
         BO    GRPX                                                             
         OI    CISTAT,PQSTIN                                                    
         AP    QI,=P'1'                                                         
         B     GRPUPD                                                           
GRPUVX   DS    0H                                                               
*                                                                               
GRPARS   CLI   ACTN,X'41'          ARCHIVABL                                    
         BNE   GRPARSX                                                          
         MVC   STAACT(3),=C'ARS'                                                
         B     GRPUPD                                                           
GRPARSX  DS    0H                                                               
*                                                                               
GRPARU   CLI   ACTN,X'42'          UNARCHIVABL                                  
         BNE   GRPARUX                                                          
         MVC   STAACT(3),=C'ARU'                                                
         B     GRPUPD                                                           
GRPARUX  DS    0H                                                               
*                                                                               
GRPADS   CLI   ACTN,X'43'          ARCHIVED                                     
         BNE   GRPADSX                                                          
         MVC   STAACT(3),=C'ADS'                                                
         B     GRPUPD                                                           
GRPADSX  DS    0H                                                               
*                                                                               
GRPADU   CLI   ACTN,X'44'          UNARCHIVED                                   
         BNE   GRPADUX                                                          
         MVC   STAACT(3),=C'ADU'                                                
         B     GRPUPD                                                           
GRPADUX  DS    0H                                                               
*                                                                               
GRPAES   CLI   ACTN,X'45'          ARCHIVE ELIGIBLE                             
         BNE   GRPAESX                                                          
         MVC   STAACT(3),=C'AES'                                                
         B     GRPUPD                                                           
GRPAESX  DS    0H                                                               
*                                                                               
GRPAEU   CLI   ACTN,X'46'          UNELIGIBLE                                   
         BNE   GRPAEUX                                                          
         MVC   STAACT(3),=C'AEU'                                                
         B     GRPUPD                                                           
GRPAEUX  DS    0H                                                               
*                                                                               
GRPBKU   CLI   ACTN,X'47'          BACKED UP                                    
         BNE   GRPBKUX                                                          
         MVC   STAACT(3),=C'BKS'                                                
         B     GRPUPD                                                           
GRPBKUX  DS    0H                                                               
*                                                                               
GRPUBKU  CLI   ACTN,X'48'          NOT BACKED UP                                
         BNE   GRPUBKUX                                                         
         MVC   STAACT(3),=C'BKU'                                                
         B     GRPUPD                                                           
GRPUBKUX DS    0H                                                               
*                                                                               
GRPNCD   CLI   ACTN,X'49'          SET NCD                                      
         BNE   GRPNCDX                                                          
         MVC   STAACT(3),=C'NCD'                                                
         B     GRPUPD                                                           
GRPNCDX  DS    0H                                                               
*                                                                               
GRPOCDU  CLI   ACTN,X'4A'          SET OCD                                      
         BNE   GRPOCDX                                                          
         MVC   STAACT(3),=C'OCD'                                                
         B     GRPUPD                                                           
GRPOCDX  DS    0H                                                               
*                                                                               
GRPEK    CLI   ACTN,X'2B'          UNERROR ALL                                  
         BNE   GRPEKX                                                           
         MVC   STAACT(3),=C'UNE'                                                
         B     GRPUPD                                                           
GRPEKX   DS    0H                                                               
*                                                                               
GRPEL    CLI   ACTN,X'2E'          UNSECURE ALL                                 
         BNE   GRPELX                                                           
         MVC   STAACT(3),=C'UNS'                                                
         B     GRPUPD                                                           
GRPELX   DS    0H                                                               
*                                                                               
GRPR     CLI   ACTN,X'29'          RETAIN                                       
         BNE   GRPRX                                                            
         MVC   STAACT(3),=C'RET'                                                
         CLI   ACTN1,X'FF'         TEST FOR RETAIN HOURS INPUT                  
         BE    GRPR1                                                            
         MVI   ACTN2,24            DEFAULT TO ONE DAY                           
GRPR1    CLI   ACTN2,168           RETAIN HOURS MUST BE LT ONE WEEK             
         BNH   GRPR2                                                            
         TM    DDS,DDSTRM                                                       
         BO    *+12                                                             
         MVI   ACTN2,168           USERS HAVE MAX OF 168 HOURS                  
         B     GRPR2                                                            
         CLI   ACTN2,255           DDS MAX HOURS MEANS PERMANENT                
         BNE   GRPR2                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FFS            HOURS=X'FFFF' MEANS PERMANENT                
         B     GRPR3                                                            
GRPR2    SR    R1,R1                                                            
         IC    R1,ACTN2                                                         
GRPR3    STCM  R1,3,NXINFO         SET RETAIN HOURS IN INDEX                    
         OI    NXFLAG,X'10'        SET FLAG TO SHOW PASSING HOURS               
         STCM  R1,3,NDX+20         **TEMP** OLD STYLE CALL **REMOVE**           
         MVI   NDX+22,X'FF'        **TEMP** OLD STYLE CALL **REMOVE**           
         B     GRPUPD                                                           
GRPRX    DS    0H                                                               
*                                                                               
GRPANY   B     GRPX                DEFAULT TO DISPLAY                           
*                                                                               
GRPUPD   LA    R5,CXREC            INITIALISE CXREC FOR STATUS CHANGE           
         USING PQRECD,R5                                                        
         MVC   CIADDR(2),RTECIADR-RTED(R6)                                      
         MVI   CIADDR+2,1                                                       
         MVI   CIADDR+3,0                                                       
         GOTO1 ADATAMGR,DMCB,(X'00',BUFFER),PRTQID,NDX,SAVE,(R5)                
         CLI   8(R1),0                                                          
         BNE   GRPERR                                                           
         L     RE,8(R5)            SET DISK ADDR IN SAVE AREA                   
         AR    RE,R5                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKINTNO-SKBUFFD(1,RE),CFPQINUM                                   
         MVC   SKEXTNO-SKBUFFD(1,RE),BUPQFILE                                   
         XC    SAVE(12),SAVE                                                    
         MVC   SAVE+4(4),=C'LINE'                                               
         GOTO1 ADATAMGR,DMCB,(X'00',RANDOM)                                     
         CLI   8(R1),0                                                          
         BNE   INDERR                                                           
         MVC   SVPQREC,0(R5)       SAVE REPORT DATA BEFORE ACTION               
*&&US                                                                           
         CLI   IFDDSV,C'E'         ONLY PURGE CLASS N IF DDS=F                  
         BE    *+12                                                             
         CLI   PQCLASS,C'N'                                                     
         BE    GRPX                                                             
*&&                                                                             
         CLI   SVPQREC+PQSTAT-PQRECD,PQSTPU IS THIS ALREADY PURGED?             
         BE    *+8                          CAN'T PURGE AGAIN                   
*                                                                               
         BRAS  RE,PQUPDATE         UPDATE STATUS                                
         MVI   QIND,1              SET STATUS CHANGE IN CISTAT                  
         AP    QS,=P'1'                                                         
*                                                                               
GRPX     AP    QN,=P'1'            BUMP NUMBER OF REPORTS                       
         SR    R1,R1                                                            
         IC    R1,PQSPAGE          FIRST CHECK PAGE                             
         MH    R1,=H'16'           16 REPORTS PER PAGE                          
         CVD   R1,DUB                                                           
         CP    QN,DUB                                                           
         BNH   GRPDSP4                                                          
         CP    QO,DSPMAX           ROOM TO DISPLAY                              
         BNL   GRPDSP4             NO                                           
         CLI   QIND,0              WAS THERE AN UPDATE                          
         BNE   GRPDSP1             YES                                          
*                                                                               
GRPDSP   LA    R5,CXREC            INITIALISE CXREC FOR DISPLAY                 
         USING PQRECD,R5                                                        
         MVC   CIADDR(2),RTECIADR-RTED(R6)                                      
         MVI   CIADDR+2,1                                                       
         MVI   CIADDR+3,0                                                       
         GOTO1 ADATAMGR,DMCB,(X'00',BUFFER),PRTQID,NDX,SAVE,(R5)                
         CLI   8(R1),0                                                          
         BNE   GRPERR                                                           
         MVC   CIDATA,12(R5)       **NEW CODE**APR 94                           
         MVC   BUFFDATA,0(R5)                                                   
         L     RE,8(R5)            SET DISK ADDR IN SAVE AREA                   
         AR    RE,R5                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKINTNO-SKBUFFD(1,RE),CFPQINUM                                   
         MVC   SKEXTNO-SKBUFFD(1,RE),BUPQFILE                                   
*                                                                               
         SAM31                                                                  
         MVC   FIWRES,PRTQID                                                    
         BRAS  RE,FIRSET                                                        
         JNE   *+2                                                              
         MVC   FIWCIA,CIADDR                                                    
         BRAS  RE,FIRCN                                                         
         SAM24                                                                  
*                                                                               
GRPDSP1  XC    SAVE(12),SAVE       READ FIRST CI REC INTO CXREC                 
         MVC   SAVE+4(4),=C'LINE'                                               
         GOTO1 ADATAMGR,DMCB,(X'00',RANDOM),PRTQID,NDX,SAVE,(R5)                
         CLI   8(R1),0                                                          
         BNE   GRPERR                                                           
         CLI   QIND,0              TEST IF THERE WAS AN UPDATE                  
         BNE   *+10                YES                                          
         MVC   SVPQREC,0(R5)       SAVE REPORT DATAA                            
*                                                                               
GRPDSP2  AP    QO,=P'1'            BUMP DISPLAY COUNT                           
         ZAP   DUB,QO              CALC SCREEN LOCATION                         
         CVB   R4,DUB                                                           
         BCTR  R4,0                                                             
         LA    R0,93               GET ENTRY WIDTH                              
         STH   R0,DUB                                                           
         MH    R4,DUB                                                           
         LA    R4,SRVSA1H(R4)      R4=A(SCREEN DISPLAY LINE)                    
         LA    R1,SRVPFKH                                                       
         CR    R4,R1                                                            
         BNL   GRPDSP4                                                          
         USING PQNLD,R4                                                         
         TM    DDS1,DDSTOT                                                      
         BO    GRPTEST                                                          
*                                                                               
         OI    6(R4),X'80'         XMIT                                         
         NI    6(R4),(X'FF'-X'08') SET NORMAL                                   
         XC    8(4,R4),8(R4)                                                    
         TM    RTESORT-RTED(R6),X'7F'   TEST FOR ANYTHING                       
         BZ    GRPTEST                                                          
         TM    RTESORT-RTED(R6),X'20'   TEST EXECUTED                           
         BO    ACT000                                                           
         TM    RTESORT-RTED(R6),X'40'                                           
         BNO   ACT000                                                           
         OI    6(R4),X'08'         SET HI                                       
         MVI   8(R4),C'*'                                                       
ACT000   MVC   BYTE,RTESORT-RTED(R6)                                            
         NI    BYTE,X'0F'           GET ACTION CODE                             
         L     R1,ASELTABL                                                      
ACT001   CLC   BYTE,6(R1)                                                       
         BE    ACT002                                                           
         LA    R1,8(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   ACT001                                                           
         B     ACT004              MUST BE INVALID                              
ACT002   EX    0,0(R1)             RF=A(ACTION)                                 
         MVC   DUB1,SPACES                                                      
         MVC   DUB1(1),0(RF)                                                    
         MVC   BYTE1,4(R1)         SAVE ACTION VALUE                            
         TM    RTESORT-RTED(R6),X'20'  TEST ERROR ON VALID ACTION               
         BNO   *+8                                                              
         ST    R4,CURSOR               SET CURSOR                               
         MVC   BYTE,RTESORT+1-RTED(R6)                                          
         TM    5(R1),X'40'             TEST FOR NUMERIC ALLOWED                 
         BNO   ACT00X                                                           
         TM    5(R1),X'20'             TEST FOR NEGATIVE ALLOWED                
         BNO   ACT003                                                           
         CLI   BYTE,X'80'              TEST FOR LIST LAST                       
         BNE   *+14                                                             
         MVC   DUB1+1(1),SR@LAST                                                
         B     ACT00X                                                           
         TM    BYTE,X'80'              TEST FOR NEGATIVE USED                   
         BNO   ACT003                                                           
         NI    BYTE,(X'FF'-X'80')                                               
         MVI   DUB1+1,C'-'                                                      
         EDIT  (B1,BYTE),(2,DUB1+2),ALIGN=LEFT                                  
         B     ACT00X                                                           
ACT003   EDIT  (B1,BYTE),(3,DUB1+1),ALIGN=LEFT                                  
         B     ACT00X                                                           
ACT004   MVC   DUB1(3),SR@ERROR    MARK INVALID INPUT                           
*                                                                               
ACT00X   CLI   8(R4),C'*'                                                       
         BE    ACT00XB                                                          
         TM    PNMUOPTS,CTIDOFPN   TEST USING PRINTER NAMES                     
         BZ    ACT00XA                                                          
         CLI   BYTE1,X'11'         TEST START                                   
         BE    *+12                                                             
         CLI   BYTE1,X'14'         TEST SCHED                                   
         BNE   ACT00XA                                                          
         OC    PRNNAME,PRNNAME                                                  
         BZ    GRPTEST                                                          
         MVC   8(4,R4),PRNNAME                                                  
         B     GRPTEST                                                          
ACT00XA  MVC   8(4,R4),DUB1                                                     
         B     GRPTEST                                                          
ACT00XB  TM    PNMUOPTS,CTIDOFPN   TEST USING PRINTER NAMES                     
         BO    ACT00XC                                                          
         CLI   BYTE1,X'11'         TEST START                                   
         BE    *+8                                                              
         CLI   BYTE1,X'14'         TEST SCHED                                   
         BE    *+14                                                             
ACT00XC  MVC   9(3,R4),DUB1                                                     
         B     GRPTEST                                                          
         MVC   9(3,R4),DUB1+1      *NNN IF SCHED OR START                       
*                                                                               
GRPTEST  MVCDD PQNLID(L'PQNLDATA),SR#REXPD                                      
         CLI   PQSDDSFN,0          TEST FOR SPECIAL DDS                         
         BNE   GRPTESTX                                                         
         MVC   USERID,HALF1                                                     
         OC    USERID,USERID       TEST USER ID DEFINED                         
         BZ    GRPTESTD                                                         
         TM    PQSUSER,X'80'       TEST GENERIC USER ID                         
         BZ    GRPTESTB                                                         
         CLI   AGENIDS,X'FF'       TEST VGENIDS PRESENT                         
         BE    GRPTRAP                                                          
         GOTO1 AGENIDS,DMCB,PQSUSER,ADATAMGR                                    
         BNE   GRPTRAP                                                          
         LM    RE,RF,0(R1)         RE=N'ENTRIES,RF=A(ENTRIES)                   
         CLC   PQSRCID,0(RF)                                                    
         BE    GRPTESTA                                                         
         LA    RF,2(RF)                                                         
         BCT   RE,*-14                                                          
         B     GRPTRAP                                                          
*                                                                               
GRPTESTA B     GRPTESTD                                                         
GRPTESTB CLC   PQSRCID,USERID      ID WAS SPECIFIED                             
         BNE   GRPTRAP                                                          
GRPTESTD CLC   PQREPNO,RTEREPNO-RTED(R6)    MATCH ON SEQ NO                     
         BNE   GRPTRAP                                                          
GRPTESTX B     GRPDSP19                                                         
*                                                                               
GRPTRAP  B     GRPDSP4                                                          
*                                                                               
*GRPTRAP  CLC   USERID,=AL2(2303)  TEST FOR GFMO                                
*         BE    DUMPTRAP                                                        
*         CLC   USERID,=AL2(2757)  OR DEGFFR                                    
*         BE    DUMPTRAP                                                        
*         B     GRPDSP4                                                         
*                                                                               
*DUMPTRAP DC    H'0'                                                            
*                                                                               
GRPDSP19 MVC   CISTAT,PQSTAT                                                    
         TM    PQSTAT,PQSTAC                                                    
         BNO   GRPDSP20                                                         
         TM    RTESORT-RTED(R6),X'80'                                           
         BNO   GRPDSP20                                                         
         OI    CISTAT,PQSTPG                                                    
GRPDSP20 EQU   *                                                                
         MVC   PQNLID(3),PQSUBID                                                
         MVI   PQNLID+3,C','                                                    
         LA    RF,PQNLID+4                                                      
         SR    R0,R0                                                            
         ICM   R0,3,PQREPNO                                                     
         EDIT  (R0),(5,0(RF)),ALIGN=LEFT                                        
*                                                                               
         MVC   HALF3,PQDATEL                                                    
         GOTO1 ADATCON,DMCB,(X'4E',HALF3),(7,PQNLCD) LOWER CASE MONTH           
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    *+8                                                              
         MVI   DMCB,X'42'          OLD DATE                                     
         BASR  RE,RF                                                            
         MVC   DUB(2),PQTIMEL                                                   
         BRAS  RE,TIMEOUT                                                       
         MVC   PQNLCD+6(5),DUB+2                                                
*&&US                                                                           
         TM    DDS,DDSTRM          TEST IF DDS TERMINAL                         
         BZ    *+18                                                             
         TM    PQTYP1,PQTYREP      TEST IF CREATED BY REP SYSTEM                
         BZ    *+10                                                             
         OC    PQNLCD(3),SPACES    UPPER CASE MONTH IF REP REPORT               
*&&                                                                             
         MVC   PQNLCLAS,PQCLASS                                                 
*                                                                               
         TM    PQATTB,PQATERR      ERROR REPORTS                                
         BZ    GRPDSP22                                                         
         MVC   PQNLTYPE,SR@ERROR   DISPLAY ERROR                                
         MVC   PQNLPTY,PQERRRC     DISPLAY ERROR REASON CODE                    
         OI    PQNLPTY,C' '                                                     
         B     GRPDSP23                                                         
GRPDSP22 MVC   PQNLTYPE,SR@OVNIT   DISPLAY TYPE                                 
         TM    PQTYPE,PQTYUPDT                                                  
         BNO   *+14                                                             
         MVC   PQNLTYPE,SR@UPD                                                  
         B     GRPDSP23                                                         
         TM    PQATTB,PQATJOBO                                                  
         BNO   *+14                                                             
         MVC   PQNLTYPE,SR@SOON                                                 
         B     GRPDSP23                                                         
         TM    PQTYPE,PQTYONL                                                   
         BNO   *+10                                                             
         MVC   PQNLTYPE,SR@NOW                                                  
*                                                                               
GRPDSP23 TM    PQTYPE,PQTYDL       TEST FOR DOWNLOAD                            
         BO    *+8                                                              
         NI    PQNLTYPE,255-X'40'  IF NOT SET TO LC                             
*                                                                               
GRPDSP24 MVC   PQNLNAME,PQDESC                                                  
*                                                                               
         MVC   SLINE(2),PQPAGES                                                 
         TM    DDS1,DDSTOT                                                      
         BZ    *+16                                                             
         MVC   CISTAT,PQSTAT       DISPLAY COUNT FOR TOTAL OPTION               
         MVC   SLINE(2),RTESORT-RTED(R6)                                        
         SR    R0,R0                                                            
         ICM   R0,3,SLINE                                                       
         EDIT  (R0),(5,PQNLPAGE)                                                
*                                                                               
         BRAS  RE,STATOUT                                                       
         MVC   PQNLSTAT,STAT       SHORT STATUS                                 
*                                                                               
         LA    RF,MAXDATE                                                       
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,MAXDATEO                                                      
         CLC   PQAGERD,0(RF)       PERMANENT IF RETAINED TO MAXDATE             
         BNE   GRPDSP2C                                                         
         MVC   PQNLRET,SR@PERM                                                  
         LA    RF,255              SET LOTS OF HOURS FOR COLOUR                 
         B     GRPDSP2G                                                         
*                                                                               
GRPDSP2C MVC   DUB(2),PQAGERD      GET RETAIN DATE                              
         LA    RF,DATEC                                                         
         LA    R0,14                                                            
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    *+12                                                             
         LA    R0,2                                                             
         LA    RF,DATECO                                                        
         CLC   DUB(2),0(RF)        CHECK WITH TODAY                             
         BL    GRPDSP2D                                                         
         GOTO1 ADATCON,DMCB,((R0),DUB),(0,DUB1)                                 
         GOTO1 APERVERT,DMCB,DATE,DUB1                                          
         LH    R1,DMCB+8           NUMBER OF DAYS                               
         BCTR  R1,0                                                             
         MH    R1,=H'1440'         24 HRS = 1440 MINS                           
         STH   R1,HALF                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,PQAGERT          GET RETAIN TIME                              
         MH    R1,=H'10'                                                        
         ST    R1,DUB1                                                          
         BRAS  RE,GETTIME          GET TIME NOW                                 
*                                                                               
         L     RF,DUB1             RETAIN TIME                                  
         L     RE,FULL1            TIME NOW                                     
         SR    RF,RE                                                            
         AH    RF,HALF             ADD 24 HRS PER DAY                           
         BL    GRPDSP2D                                                         
         SR    RE,RE                                                            
         D     RE,=F'60'                                                        
         LTR   RF,RF                                                            
         BZ    GRPDSP2E                                                         
         CH    RF,=H'99'                                                        
         BH    GRPDSP2F                                                         
         EDIT  (RF),(2,PQNLRET)                                                 
         B     GRPDSP2G                                                         
GRPDSP2D MVI   PQNLRET+1,C'0'      SHOW EXPIRED                                 
         SR    RF,RF               SET TO ZERO HRS FOR COLOUR                   
         B     GRPDSP2G                                                         
GRPDSP2E MVC   PQNLRET,=C'<1'      SOON TO EXPIRE                               
         B     GRPDSP2G                                                         
GRPDSP2F MVC   PQNLRET,=C'>>'      GREATER THAN 99 HRS                          
*                                                                               
GRPDSP2G CH    RF,=H'2'            TEST HRS TO GO                               
         BNL   *+12                                                             
         MVI   PQNLHDR+5,2         HRS <2 RED                                   
         B     *+8                                                              
         MVI   PQNLHDR+5,6         HRS=>2 YELLOW                                
         TM    CISTAT,PQSTDEAD                                                  
         BZ    *+8                                                              
         MVI   PQNLHDR+5,4         PRTD/SENT GREEN                              
         TM    CISTAT,PQSTKE+PQSTHO                                             
         BZ    *+8                                                              
         MVI   PQNLHDR+5,5         HOLD/KEEP TURQUOISE                          
         TM    CISTAT,PQSTPG                                                    
         BZ    *+8                                                              
         MVI   PQNLHDR+5,X'44'     FLASH GREEN PRINTING                         
*                                                                               
GRPDSP3  MVI   SWORK,2             GET EXTRA INFO FOR REPORT                    
         BRAS  RE,XTRA                                                          
         MVC   PQNLFMS,SWORK                                                    
*&&UK                                                                           
GRPDSP3A CLC   PQNLNAME(3),=C'A55' MAKE ROOM FOR FORM DISPLAY                   
         BNE   GRPDSP3B                                                         
         CLC   PQNLNAME+4(6),=C'LABELS'                                         
         BNE   GRPDSP3B                                                         
         MVC   PQNLNAME+4(6),=C'LAB   '                                         
         B     GRPDSP3D                                                         
GRPDSP3B EQU   *                                                                
*&&                                                                             
GRPDSP3D CLC   SWORK+12(4),SPACES  SET FORM IN LAST 4 CHRS OF DESC              
         BE    *+10                                                             
         MVC   PQNLNAME+7(4),SWORK+12                                           
         CLI   SWORK+20,C' '       TEST IF ANY SECURITY INFO                    
         BNH   *+16                                                             
         OC    PQNLFMS,SPACES                                                   
         MVC   PQNLFMS+L'PQNLFMS-1(1),SWORK+20                                  
*                                                                               
         MVC   PQNLPRTD,=C'..... .....'                                         
         OC    PQDATED,PQDATED                                                  
         BZ    GRPDSP3E                                                         
         MVC   HALF3,PQDATED                                                    
         GOTO1 ADATCON,DMCB,(X'4E',HALF3),(7,PQNLPRTD) LOWER CASE MONTH         
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    *+8                                                              
         MVI   DMCB,X'42'          OLD DATE                                     
         BASR  RE,RF                                                            
         MVC   DUB(2),PQTIMED                                                   
         BRAS  RE,TIMEOUT                                                       
         MVC   PQNLPRTD+6(5),DUB+2                                              
*&&US                                                                           
         TM    DDS,DDSTRM          TEST IF DDS TERMINAL                         
         BZ    *+18                                                             
         TM    PQTYP1,PQTYREP      TEST IF CREATED BY REP SYSTEM                
         BZ    *+10                                                             
         OC    PQNLPRTD(3),SPACES  UPPER CASE MONTH IF REP REPORT               
*&&                                                                             
GRPDSP3E OC    SVREPID(2),SVREPID  TEST TO DISPLAY USERID NAME                  
         BZ    GRPDSP3F                                                         
         TM    DDS1,DDSTOT+DDSGEN  V=..../U=GENERIC                             
         BZ    GRPDSP4                                                          
GRPDSP3F TM    DDS1,DDSTOT                                                      
         BO    GRPDSP3G                                                         
         CLC   PQSRCID,FULL        TEST FOR USERID CHANGE FOR U=ALL             
         BE    GRPDSP4                                                          
         MVC   FULL(2),PQSRCID                                                  
GRPDSP3G MVC   GIUSER,PQSRCID                                                   
         GOTO1 AGETUSER                                                         
         MVC   PQNLNAME,GIUSERID                                                
         TM    DDS1,DDSTOT                                                      
         BO    *+8                                                              
         MVI   PQNLNAME-1,C'>'     SHOW USERID BREAK FOR U=ALL                  
         MVI   PQNLHDR+5,X'07'     SHOW BREAK IN WHITE                          
*                                                                               
GRPDSP4  LA    R6,L'RTEDATA(R6)                                                 
         CLC   0(2,R6),FFS         TEST END OF REPTAB                           
         BNE   GRP2                                                             
*                                                                               
GRPDSP6  BRAS  RE,CNTDSP           DISPLAY COUNTERS                             
*                                                                               
         MVI   MSG,C'.'            AREA FOR GETTXT INCLUDES                     
         MVC   MSG+1(L'MSG-1),MSG                                               
         LA    RE,MSG                                                           
*                                                                               
         EDIT  (P3,QT),(4,1(RE)),ALIGN=LEFT                                     
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         ST    RE,FULL                                                          
         CP    QX,=P'0'            INFORM OF TABLE OVERFLOW                     
         BE    GRPDSP8                                                          
         LA    RE,0(R1,RE)                                                      
         MVC   0(2,RE),=C' +'                                                   
         EDIT  (P3,QX),(4,2(RE)),ALIGN=LEFT                                     
         AR    R1,R0                                                            
         LA    R1,2(R1)                                                         
         L     RE,FULL                                                          
GRPDSP8  STC   R1,0(RE)                                                         
         LA    RE,0(R1,RE)                                                      
*                                                                               
         MVCDD 1(7,RE),SR#RPTS                                                  
         TM    DDS1,DDSTOT                                                      
         BZ    *+10                                                             
         MVCDD 1(7,RE),SR#USRIS                                                 
         MVI   0(RE),X'08'                                                      
         LA    RE,8(RE)                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PQSPAGE                                                       
         LA    R1,1(R1)                                                         
         EDIT  (R1),(2,1(RE)),ALIGN=LEFT                                        
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,0(RE)                                                         
         LA    RE,0(R1,RE)                                                      
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,PQSTOTL                                                     
         D     R0,=F'16'                                                        
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         EDIT  (R1),(2,1(RE)),ALIGN=LEFT                                        
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,0(RE)                                                         
         LA    RE,0(R1,RE)                                                      
*                                                                               
         MVC   0(2,RE),=X'0100'                                                 
         CLI   ACTN,X'21'                                                       
         BE    GRPDSPX                                                          
         CP    QS,=P'1'                                                         
         BL    GRPDSP9                                                          
         EDIT  (P3,QS),(4,1(RE)),ALIGN=LEFT                                     
         LR    R1,R0                                                            
         B     GRPDSPA                                                          
GRPDSP9  MVI   MSG+41,C'0'                                                      
         LA    R1,1                                                             
GRPDSPA  ST    RE,FULL                                                          
         AR    RE,R1                                                            
         MVCDD 2(14,RE),SR#STCHS  STATUS CHANGES                                
         LA    R1,15(R1)                                                        
         L     RE,FULL                                                          
         STC   R1,0(RE)                                                         
*                                                                               
GRPDSPX  TM    INTFLAG,INTERR      TEST FOR INTERNAL ERROR                      
         BNO   GRPDSPX2                                                         
         L     RE,CURSOR                                                        
         OI    6(RE),X'40'                                                      
         NI    INTFLAG,255-INTERR  CLEAR ERROR AND EXIT                         
         B     EXIT                                                             
GRPDSPX2 OC    PQSCURS,PQSCURS     TEST FOR CURSOR SAVED                        
         BNZ   *+12                                                             
         OI    SRVSA1H+6,X'40'                                                  
         B     GRPDSPX3                                                         
         SR    RE,RE                                                            
         ICM   RE,3,PQSCURS        RESET SAVED CURSOR                           
         AR    RE,R3                                                            
         OI    6(RE),X'40'                                                      
         XC    PQSCURS,PQSCURS                                                  
GRPDSPX3 LA    RE,168              PAGE N OF M DISPLAYED                        
         GOTO1 AGETTXT,DMCB,(RE),0,(C'I',0),0,MSG,X'010000'                     
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVEX1H+6,X'80'                                                  
         B     EXIT                                                             
*                                                                               
GRPERR   DC    H'0'                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* PQ UPDATE                                                                     
***********************************************************************         
PQUPDATE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 ADATAMGR,DMCB,(0,STAACT),PRTQID,NDX,SAVE,(R5)                    
         CLI   8(R1),0                                                          
         BE    PQUX                                                             
         CLI   ACTN,X'23'          IF ACTION PURGE DON'T BOTHER                 
         BE    PQUX                                                             
         OI    DDS,DDSERR          SET ERROR OCCURED ON UPDATE                  
         B     PQUX                                                             
*                                                                               
PQUX     J     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* INDEX DISPLAY ROUTINE                                                         
***********************************************************************         
INDDSPR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,CNTDSP           DISPLAY COUNTERS                             
         TM    DDS,DDSTRM          DISPLAY DISK ADDR FOR DDS TRM                
         BZ    INDDSP1                                                          
*                                                                               
         LA    R4,SRVINF+L'SRVINF-1                                             
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,1(R4)                                                         
         LA    RE,SRVINF+L'SRVINF-15                                            
         CR    R4,RE                                                            
         BH    INDDSP1             IGNORE IF NO ROOM                            
         MVC   0(6,R4),=C',ADDR='                                               
         GOTO1 AHEXOUT,DMCB,CIADDR,6(R4),4,=C'MIX'                              
*                                                                               
         OC    PQPIDNUM,PQPIDNUM                                                
         BZ    INDDSP1                                                          
*                                                                               
         LA    R4,14(R4)           ADVANCE PAST DISK ADDRESS                    
         LA    RE,SRVINF+L'SRVINF-16  ROOM FOR ',PID=XY/ABCDDDNY'?              
         CR    R4,RE                                                            
         BH    INDDSP1                                                          
*                                                                               
         MVC   0(5,R4),=C',PID='                                                
         LA    R4,5(R4)                                                         
         MVC   0(2,R4),PQPIDNUM                                                 
         MVI   2(R4),C'/'                                                       
         LA    R4,3(R4)                                                         
*                                                                               
         L     RF,ACTREC                                                        
         USING SA0REC,RF                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,PQPIDNUM                                                 
         MVC   SA0KNUM,PQPIDNUM+2                                               
         GOTO1 ADATAMGR,DMCB,(X'00',=C'DMRDHI'),=C'CTFILE',0(RF),     ,X        
               L'SA0KEY(RF)                                                     
         L     RF,ACTREC                                                        
         LA    RE,L'SA0KEY(RF)                                                  
         CLC   0(L'SA0KEY,RE),0(RF)                                             
         BNE   INDDSP1                                                          
*                                                                               
         LA    RE,SA0DATA-SA0REC(RE)                                            
INDDSP0  CLI   0(RE),0                                                          
         BE    INDDSP1                                                          
         CLI   0(RE),SAPALELQ                                                   
         BE    *+16                                                             
         LLC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     INDDSP0                                                          
*                                                                               
         MVC   0(L'SAPALPID,R4),SAPALPID-SAPALD(RE)                             
*                                                                               
INDDSP1  MVC   SRVIDEN(3),PQSUBID                                               
         MVI   SRVIDEN+3,C','                                                   
         LA    RF,SRVIDEN+4                                                     
         SR    R0,R0                                                            
         ICM   R0,3,PQREPNO                                                     
         EDIT  (R0),(5,0(RF)),ALIGN=LEFT                                        
*                                                                               
INDDSP2  MVC   SRVCLAS(1),PQCLASS  DISPLAY REPORT CLASS                         
         CLI   PQREPTY,C' '                                                     
         BNH   INDDSP2A                                                         
         MVI   SRVCLAS+1,C','                                                   
         MVC   SRVCLAS+2(1),PQREPTY DISPLAY REPORT TYPE                         
         CLI   SRVCLAS,C' '                                                     
         BH    *+8                                                              
         MVI   SRVCLAS,C'?'        SET CLASS UNDEFINED                          
*                                                                               
INDDSP2A MVC   SRVNAME(L'PQDESC),PQDESC DISPLAY REPORT NAME                     
*                                                                               
INDDSP3  MVI   SWORK,1             GET EXTRA INFO FOR REPORT                    
         BRAS  RE,XTRA                                                          
         MVC   SRVFORM,SWORK                                                    
         CLI   SWORK+20,C' '       TEST IF ANY SECURITY INFO                    
         BNH   INDDSP3F                                                         
         OC    SRVFORM,SPACES                                                   
         MVC   SRVFORM+L'SRVFORM-1(1),SWORK+20                                  
         B     INDDSP3F                                                         
INDDSP3F EQU   *                                                                
*                                                                               
         BRAS  RE,STATOUT          DISPLAY REPORT STATUS                        
         MVC   SRVSTAT,STATA                                                    
*                                                                               
INDDSP3G OC    PQPSWD,PQPSWD       DISPLAY PASSWORD PROTECTED                   
         BZ    INDDSP3L                                                         
         CLC   PQPSWD,SPACES                                                    
         BE    INDDSP3L                                                         
         TM    PQSECF1,PQSINONO    TEST IF SECURITY FLAGS ARE VALID             
         BO    INDDSP3H                                                         
         TM    PQSECF1,PQSIPID     TEST IF PID PROTECTED                        
         BZ    INDDSP3H                                                         
*                                                                               
         MVC   SRVPSWD(3),DC@PID   PID PROTECTED                                
         CLC   PQSPID,FFS                                                       
         BNE   INDDSP3L                                                         
         MVI   SRVPSWD+3,C'='                                                   
         MVC   SRVPSWD+4(2),PQPSWD                                              
         GOTO1 AHEXOUT,DMCB,PQPSWD+2,SRVPSWD+6,2,=C'MIX'                        
         B     INDDSP3L                                                         
*                                                                               
INDDSP3H MVC   SRVPSWD(3),DC@PIN   PIN PROTECTED                                
         CLC   PQSPIN,FFS                                                       
         BNE   INDDSP3L                                                         
         MVI   SRVPSWD+3,C'='                                                   
         MVC   SRVPSWD+4(4),PQPSWD                                              
*                                                                               
INDDSP3L TM    PQSECF1,PQSINONO    TEST IF SECURITY FLAGS ARE VALID             
         BO    INDDSP3P                                                         
         LA    R4,SRVSEC                                                        
         TM    PQSECF1,PQSIPID     TEST IF PID PROTECTED                        
         BZ    INDDSP3N                                                         
         MVC   0(3,R4),DC@PID      PID PROTECTED                                
         LA    R4,3(R4)                                                         
         CLC   PQPSWD,TRMPID       TEST IF MY PID                               
         BNE   INDDSP3M                                                         
         MVC   0(3,R4),=C'=* '                                                  
         LA    R4,3(R4)                                                         
         B     INDDSP3O                                                         
INDDSP3M MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
*                                                                               
INDDSP3N TM    PQSECF1,PQSIPIN     TEST IF PIN PROTECTED                        
         BZ    INDDSP3O                                                         
         MVC   0(3,R4),DC@PIN                                                   
         MVI   3(R4),C' '                                                       
         LA    R4,4(R4)                                                         
*                                                                               
INDDSP3O TM    PQSECF1,PQSIBNK     B=BANK                                       
         BZ    *+12                                                             
         MVI   0(R4),C'B'                                                       
         LA    R4,1(R4)                                                         
         TM    PQSECF1,PQSISEC     C=CONTROL                                    
         BZ    *+12                                                             
         MVI   0(R4),C'C'                                                       
         LA    R4,1(R4)                                                         
         TM    PQSECF1,PQSIPAY     P=PAYROLL                                    
         BZ    *+12                                                             
         MVI   0(R4),C'P'                                                       
         LA    R4,1(R4)                                                         
         TM    PQSECF1,PQSISSN     S=SOCIAL SECURITY NUMBER                     
         BZ    *+12                                                             
         MVI   0(R4),C'S'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
INDDSP3P TM    PQTYP1,PQTYBKU      TEST IF BACKED UP AND NOT ARCHIVED           
         BZ    INDDSP3Q                                                         
         TM    PQTYP1,PQTYAD                                                    
         BO    INDDSP3Q                                                         
         TM    DDS,DDSTRM          SHOW BACKUP DATE/TIME IF DDS TERM            
         BZ    INDDSP3Q                                                         
         OC    PQDATEA,PQDATEA                                                  
         BZ    INDDSP3Q                                                         
         MVI   SRVARC+0,C'>'       DISPLAY BACKED UP CHARACTER                  
         B     INDDSP3S                                                         
*                                                                               
INDDSP3Q TM    PQTYP1,PQTYAE+PQTYAR+PQTYAD                                      
         BZ    INDDSP3V                                                         
         TM    PQTYP1,PQTYAD       TEST IF ARCHIVED                             
         BZ    INDDSP3T                                                         
         MVC   SRVARC+0(1),PQARC   DISPLAY ARCHIVE CLASS FOR DDS TERM           
         CLI   SRVARC+0,C'A'                                                    
         BNL   *+8                                                              
         MVI   SRVARC+0,C'?'       DISPLAY UNKNOWN ARCHIVE CLASS                
         TM    DDS,DDSTRM                                                       
         BO    *+8                                                              
         MVI   SRVARC+0,C'D'       SHOW ARCHIVED STATUS CHR                     
         OC    PQDATEA,PQDATEA     TEST IF ARCHIVE DATE DEFINED                 
         BNZ   INDDSP3S                                                         
         MVC   SRVARC(8),DC@ARCD   SHOW STATUS STRING IF NO DATE/TIME           
         B     INDDSP3V                                                         
*                                                                               
INDDSP3S MVI   SRVARC+1,C','       DISPLAY DATE/TIME ARCHIVED/BACKUP            
         MVC   HALF3,PQDATEA                                                    
         GOTO1 ADATCON,DMCB,(X'0E',HALF3),(17,WORK)                             
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    *+8                                                              
         MVI   DMCB,2              OLD DATE                                     
         BASR  RE,RF                                                            
         MVC   SRVARC+2(8),WORK                                                 
         OC    PQTIMEA,PQTIMEA                                                  
         BZ    INDDSP3V                                                         
         MVC   DUB(2),PQTIMEA      DISPLAY TIME ARCHIVED                        
         BRAS  RE,TIMEOUT                                                       
         MVI   SRVARC+10,C','                                                   
         MVC   SRVARC+11(5),DUB+2                                               
         B     INDDSP3V                                                         
*                                                                               
INDDSP3T TM    PQTYP1,PQTYAR       TEST IF ARCHIVABLE                           
         BZ    INDDSP3U                                                         
         MVC   SRVARC(8),DC@ARCA                                                
         B     INDDSP3V                                                         
INDDSP3U TM    PQTYP1,PQTYAE       TEST IF ARCHIVE ELIGIBLE                     
         BZ    INDDSP3V                                                         
         MVC   SRVARC(8),DC@ARCE                                                
         B     INDDSP3V                                                         
*                                                                               
INDDSP3V EQU   *                                                                
*                                                                               
INDDSP4  SR    R0,R0                                                            
         ICM   R0,3,PQPAGES                                                     
         EDIT  (R0),(6,SRVNUMP),ALIGN=LEFT                                      
         SR    R0,R0                                                            
         ICM   R0,1,PQLPP                                                       
         EDIT  (R0),(2,SRVLPP),ALIGN=LEFT                                       
         SR    R0,R0                                                            
         ICM   R0,7,PQLINES                                                     
         EDIT  (R0),(7,SRVNUML),ALIGN=LEFT                                      
         SR    R0,R0                                                            
         ICM   R0,3,PQAVCPL                                                     
         EDIT  (R0),(3,SRVCPL),ALIGN=LEFT                                       
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R0,1,PQNCI          ADD THESE TOGETHER                           
         ICM   R1,1,PQNCIX                                                      
         AR    R0,R1                                                            
         BNZ   *+8                                                              
         ICM   R0,1,PQAGES                                                      
         EDIT  (R0),(3,SRVNCIS),ALIGN=LEFT                                      
         OC    SRVNCIS+00(4),SPACES                                             
         MVC   SRVNCIS+04(5),=C'PRTQU'                                          
         MVC   SRVNCIS+08(1),PRTQID+4                                           
         MVI   SRVNCIS+09,C' '                                                  
*                                                                               
         EDIT  (B1,PQMAXCPL),(3,SRVCPLM),ALIGN=LEFT                             
         OC    PQPIDNUM,PQPIDNUM                                                
         BZ    INDDSP5                                                          
         MVC   SRVCPLM+4(2),PQPIDNUM                                            
         GOTO1 AHEXOUT,DMCB,PQPIDNUM+2,SRVCPLM+6,2,=C'MIX'                      
         CLC   PQPIDNUM,TRMPID                                                  
         BNE   *+8                                                              
         MVI   SRVCPLM+10,C'*'      FLAG IT AS MY PID PID=*                     
*                                                                               
INDDSP5  EDIT  (B1,PQLINEW),(3,SRVLINT),ALIGN=LEFT                              
         LA    R1,SRVLINT                                                       
         AR    R1,R0                                                            
         MVI   0(R1),C'F'                                                       
         TM    PQLINET,PQLTFL                                                   
         BO    *+8                                                              
         MVI   0(R1),C'V'                                                       
         MVI   1(R1),C'C'                                                       
         TM    PQLINET,PQLTCC                                                   
         BO    *+8                                                              
         MVI   1(R1),C' '                                                       
*                                                                               
INDDSP6  OC    PQAGERD,PQAGERD     DISPLAY RETAIN DATE/TIME                     
         BZ    INDDSP7                                                          
         LA    RF,MAXDATE                                                       
         TM    PQTYP1,PQTYNCD      SET RF TO NEW/OLD CMPRSD DATE                
         BO    *+8                                                              
         LA    RF,MAXDATEO                                                      
         CLC   PQAGERD,0(RF)       PERMANENT IF RETAINED TO MAXDATE             
         BNE   INDDSP6A                                                         
         MVCDD SRVDATR(9),SR#PERM                                               
         B     INDDSP7                                                          
INDDSP6A MVC   HALF3,PQAGERD                                                    
         GOTO1 ADATCON,DMCB,(X'0E',HALF3),(5,SRVDATR)                           
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
         CLI   SRVDATR,C' '                                                     
         BNE   *+14                                                             
         MVC   SRVDATR(8),SRVDATR+1                                             
         MVI   SRVDATR+8,C' '                                                   
INDDSP6B SR    R0,R0               DISPLAY RETAIN TIME                          
         SR    R1,R1                                                            
         IC    R1,PQAGERT                                                       
         MH    R1,=H'10'           CONVERT 10MIN INCREMENTS                     
         D     R0,=F'60'                                                        
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         BRAS  RE,TIMEOUT                                                       
         MVC   SRVTIMR(5),DUB+2                                                 
*                                                                               
INDDSP7  MVC   HALF3,PQDATEL                                                    
         GOTO1 ADATCON,DMCB,(X'0E',HALF3),(5,SRVDATC)                           
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
         CLI   SRVDATC,C' '                                                     
         BNE   *+14                                                             
         MVC   SRVDATC(8),SRVDATC+1                                             
         MVI   SRVDATC+8,C' '                                                   
*                                                                               
         MVC   DUB(2),PQTIMEL      DISPLAY TIME CREATED                         
         BRAS  RE,TIMEOUT                                                       
         MVC   SRVTIMC(5),DUB+2                                                 
*                                                                               
         CLC   PQRETNL,=X'0000'    DISPLAY LIVE RETENTION HOURS                 
         BNE   *+14                                                             
         MVCDD SRVRETC(4),SR#ZERO                                               
         B     INDDSP7X                                                         
         CLC   PQRETNL,=X'FFFF'                                                 
         BNE   *+14                                                             
         MVCDD SRVRETC(9),SR#PERM                                               
         B     INDDSP7X                                                         
         EDIT  (B2,PQRETNL),(5,SRVRETC),ALIGN=LEFT                              
         LA    R1,SRVRETC                                                       
         AR    R1,R0                                                            
         MVCDD 1(3,R1),SR#HOURS                                                 
INDDSP7X EQU   *                                                                
*                                                                               
INDDSP8  OC    PQDATED,PQDATED     TEST & DISPLAY LOCN PRINTED                  
         BZ    INDDSP8G                                                         
         CLC   PQPRLOC,=X'FFFF'                                                 
         BNE   *+14                                                             
         MVC   SRVLOCP(11),=C'DDS,INHOUSE'                                      
         B     INDDSP8C                                                         
         OC    PQPRLOC,PQPRLOC                                                  
         BNZ   *+14                                                             
         MVCDD SRVLOCP,SR#TRM      SHOW PRINTED AT TERMINAL                     
         B     INDDSP8C                                                         
         MVC   GIUSER,PQPRLOC                                                   
         GOTO1 AGETUSER                                                         
         LA    RF,SRVLOCP                                                       
         MVC   0(8,RF),GIUSERID                                                 
         SR    RE,RE                                                            
         IC    RE,GIULEN                                                        
         AR    RF,RE                                                            
         EDIT  (B1,PQPRNUM),(1,0(RF))                                           
*                                                                               
INDDSP8A TM    PQSTAT,PQSTSE       ADJUST HEADINGS FOR SENT                     
         BZ    INDDSP8C                                                         
         LA    RE,SRVGRP3H                                                      
         LA    RF,SRVGRP4H                                                      
         SR    R1,R1                                                            
INDDSP8B CR    RE,RF                                                            
         BNL   INDDSP8C                                                         
         CLC   8(4,RE),=X'9799A384' TEST LOWER CASE PRTD                        
         BNE   *+10                                                             
         MVC   8(4,RE),=X'A28595A3' SET TO LOWER CASE SENT                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         B     INDDSP8B                                                         
*                                                                               
INDDSP8C EDIT  (B1,PQPRCNT),(3,SRVCNTP),ALIGN=LEFT                              
*                                                                               
INDDSP8D MVC   SRVDEVP(8),PQPRSYM                                               
*                                                                               
INDDSP8E MVC   HALF3,PQDATED                                                    
         GOTO1 ADATCON,DMCB,(X'0E',HALF3),(5,SRVDATP)                           
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    *+8                                                              
         MVI   DMCB,2              OLD DATE                                     
         BASR  RE,RF                                                            
         CLI   SRVDATP,C' '                                                     
         BNE   *+14                                                             
         MVC   SRVDATP(8),SRVDATP+1                                             
         MVI   SRVDATP+8,C' '                                                   
*                                                                               
INDDSP8F MVC   DUB(2),PQTIMED      DISPLAY TIME PRINTED                         
         BRAS  RE,TIMEOUT                                                       
         MVC   SRVTIMP(5),DUB+2                                                 
*                                                                               
INDDSP8G CLC   PQRETND,=X'0000'    DISPLAY DEAD RETENTION HOURS                 
         BNE   *+14                                                             
         MVCDD SRVRETP(4),SR#ZERO                                               
         B     INDDSP8H                                                         
         CLC   PQRETND,=X'FFFF'                                                 
         BNE   *+14                                                             
         MVCDD SRVRETP(9),SR#PERM                                               
         B     INDDSP8H                                                         
         EDIT  (B2,PQRETND),(5,SRVRETP),ALIGN=LEFT                              
         LA    R1,SRVRETP                                                       
         AR    R1,R0                                                            
         MVCDD 1(3,R1),SR#HOURS                                                 
*                                                                               
INDDSP8H TM    DDS,DDSTRM                                                       
         BZ    INDDSP9                                                          
         MVC   SRVTYPE(8),TYPIND   SET TYPE INDICATORS                          
         LA    RF,SRVTYPE                                                       
         MVC   BYTE1,PQTYPE                                                     
         LA    R1,X'80'                                                         
INDDSP8I EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE1,0                                                          
         BO    *+8                                                              
         MVI   0(RF),C'.'          REPLACE WITH . IF ZERO                       
         LA    RF,1(RF)                                                         
         SRA   R1,1                                                             
         BNZ   INDDSP8I                                                         
*                                                                               
         MVI   SRVTYPE+8,C' '                                                   
         MVI   SRVTYPE+9,C' '                                                   
         MVC   SRVTYPE+10(8),TY1IND SET TYPE#1 INDICATORS                       
         LA    RF,SRVTYPE+10                                                    
         MVC   BYTE1,PQTYP1                                                     
         LA    R1,X'80'                                                         
INDDS8I1 EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE1,0                                                          
         BO    *+8                                                              
         MVI   0(RF),C'.'          REPLACE WITH . IF ZERO                       
         LA    RF,1(RF)                                                         
         SRA   R1,1                                                             
         BNZ   INDDS8I1                                                         
*                                                                               
INDDSP8J MVC   SRVATTB(8),ATTIND   SET ATTB INDICATORS                          
         LA    RF,SRVATTB                                                       
         MVC   BYTE1,PQATTB                                                     
         LA    R1,X'80'                                                         
INDDSP8K EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE1,0                                                          
         BO    *+8                                                              
         MVI   0(RF),C'.'          REPLACE WITH . IF ZERO                       
         LA    RF,1(RF)                                                         
         SRA   R1,1                                                             
         BNZ   INDDSP8K                                                         
INDDSP8L LA    RF,SRVATTB+8        POSITION FOR EXTRA REPORT INFO               
         MVI   0(RF),C' '                                                       
         LA    RF,1(RF)                                                         
         TM    PQATTB,PQATERR                                                   
         BZ    INDDSP8M                                                         
         MVC   0(1,RF),PQERRRC     DISPLAY ERROR REASON CODE                    
         OI    0(RF),C' '                                                       
         LA    RF,1(RF)                                                         
INDDSP8M CLI   PQXREPS,0                                                        
         BE    INDDSP8N                                                         
         MVC   0(1,RF),PQXREPS     DISPLAY EXTRA NUMBER OF REPORTS              
         OI    0(RF),C'0'                                                       
         LA    RF,1(RF)                                                         
INDDSP8N B     INDDSPX                                                          
*                                                                               
INDDSP9  LA    R1,WORK                                                          
         MVC   WORK,SPACES                                                      
         TM    PQTYPE,PQTYONL      IF NOT CREATED ONLINE                        
         BO    *+14                                                             
         MVC   0(10,R1),SR@OVNIT   MUST HAVE BEEN OVERNIGHT                     
         B     INDDSP9B                                                         
         TM    PQATTB,PQATJOBO     IS IT A JOBO REPORT                          
         BNO   *+14                                                             
         MVC   0(8,R1),SR@SOON     YES THEN SOON                                
         B     INDDSP9B                                                         
         MVC   0(8,R1),SR@NOW      NO MUST BE A NOW REPORT                      
*                                                                               
INDDSP9B BRAS  RE,NXTFLD           BUMP TO NEXT                                 
         TM    PQTYPE,PQTYUPDT                                                  
         BZ    *+14                                                             
         MVC   0(8,R1),SR@UPD      UPDATIVE SOON TYPE                           
         BRAS  RE,NXTFLD                                                        
*                                                                               
         TM    PQTYPE,PQTYDL                                                    
         BZ    *+14                                                             
         MVC   0(8,R1),SR@DOWNL    DOWNLOAD TYPE                                
         BRAS  RE,NXTFLD                                                        
         BCTR  R1,0                                                             
         MVI   0(R1),C' '          REMOVE LAST ,                                
         MVC   SRVTYPE,WORK                                                     
*                                                                               
         LA    R1,WORK             NOW DO ATTRIBUTES                            
         MVC   WORK,SPACES                                                      
         BRAS  RE,NXTFLD                                                        
         TM    PQATTB,PQATWIDE     TEST WIDE                                    
         BZ    *+14                                                             
         MVC   0(8,R1),SR@WIDE     WIDE                                         
         BRAS  RE,NXTFLD                                                        
         TM    PQATTB,PQATERR      TEST ERROR                                   
         BZ    *+14                                                             
         MVC   0(8,R1),SR@ERROR    ERROR                                        
         BRAS  RE,NXTFLD                                                        
         BCTR  R1,0                                                             
         MVI   0(R1),C' '          REMOVE LAST ,                                
         MVC   SRVATTB,WORK                                                     
*                                                                               
INDDSPX  CLI   BYTE,0              CHECK FOR ERRORS                             
         BNE   ERRDX                                                            
         LA    RE,179              REPORT DISPLAYED                             
         CLI   QIND,0                                                           
         BE    INDDSPX1                                                         
         CLI   ACTN,X'23'          TEST ACTION PURGE                            
         BNE   *+12                                                             
         LA    RE,171              REPORT PURGED                                
         B     INDDSPX1                                                         
         CLI   ACTN,X'29'          TEST ACTION RETAIN                           
         BNE   *+12                                                             
         LA    RE,180              WITH NEW EXPIRY DATE/TIME                    
         B     INDDSPX1                                                         
         CLI   ACTN,X'2A'          TEST ACTION PRINTED                          
         BNE   *+12                                                             
         LA    RE,182              WITH NEW STATUS AND EXPIRY DATE/TIME         
         B     INDDSPX1                                                         
         LA    RE,181              WITH NEW STATUS EXP DT/TM UNCHNGD            
INDDSPX1 GOTO1 AGETTXT,DMCB,(RE),0,(C'I',0),0,0,X'00010000'                     
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
*                                                                               
         J     XIT                                                              
*                                                                               
NXTFLD   CLI   0(R1),C' '          FIND NEXT SPACE CHR                          
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     NXTFLD                                                           
         MVI   0(R1),C','          INSERT A , AND EXIT                          
         LA    R1,1(R1)                                                         
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CLEAR ALL USER ENTRIES                                                        
***********************************************************************         
CLR      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*LR      LA    R4,SRVP2H           CLEAR ALL USER ENTRIES                       
*        BNE   ERR13                                                            
*        LA    R4,SRVP1H                                                        
*        XC    FULL(2),FULL        FIRST PASS CHECK FOR ALL STATIC              
*                                                                               
*LR0     BAS   RE,CXLOOPI                                                       
*        USING PQRECD,R5           R5=A(PRTQUE INDEX ENTRY)                     
*                                                                               
*LR2     BAS   RE,GETXAD                                                        
*        GOTO1 ADATAMGR,DMCB,(NDXRT,DMREAD),PRTQID,CXADDR,CXREC                 
*        CLI   8(R1),0                                                          
*        BNE   CLRERR                                                           
*        CLI   FULL,0              NO CHECKING FOR FORCED CLEAR                 
*        BNE   CLR4                                                             
*        CLI   IFDDSV,C'F'         DDS=F IS THE SECRET CODE                     
*        BE    CLR9                                                             
*                                                                               
*LR4     OC    USERID,USERID       MATCH ON USER ID (OR ALL FOR DDS)            
*        BZ    *+14                                                             
*        CLC   PQSRCID,USERID                                                   
*        BNE   CLR8                                                             
*        CLI   FULL,0                                                           
*        BNE   CLR6                                                             
*        TM    PQSTAT,PQSTLIVE+PQSTPG+PQSTKE                                    
*        BZ    CLR8                                                             
*        B     ERR12               ERROR NOT PURGED/PRINTED                     
*                                                                               
*LR6     BAS   RE,GETCAD                                                        
*        L     R6,ACIREC                                                        
*        GOTO1 ADATAMGR,DMCB,(X'00',DMREAD),PRTQID,CIADDR,(R6)                  
*        CLI   8(R1),0                                                          
*        BNE   CLRERR                                                           
*        XC    0(L'PQINDEX,R6),0(R6)                                            
*        GOTO1 (RF),(R1),(X'00',DMWRT)                                          
*        CLI   8(R1),0                                                          
*        BNE   CLRERR                                                           
*        LH    RE,CXENTRY                                                       
*        MH    RE,CINDXLN                                                       
*        ST    RE,16(R1)           SET DISP TO INDEX ENTRY IN DMCB              
*        LA    RE,CXREC(RE)                                                     
*        XC    0(L'PQINDEX,RE),0(RE)                                            
*        GOTO1 (RF),(R1),,,CXADDR,CXREC                                         
*        CLI   8(R1),0                                                          
*        BNE   CLRERR                                                           
*                                                                               
*LR8     BAS   RE,CXLOOPX          BUMP TO NEXT INDEX ENTRY                     
*        B     CLR4                                                             
*        B     CLR2                END OF INDEX PAGE                            
*        OC    CJCITOT,CJCITOT                                                  
*        BZ    CLR8A               NO PART2 INDEX                               
*        CLI   FULL,0                                                           
*        BE    CLR9                ONLY NEED PART1 FOR STATIC TEST              
*        CLI   FULL+1,0                                                         
*        BNE   CLRA                EXIT IF END OF PART2 INDEX                   
*        MVI   FULL+1,1                                                         
*        BAS   RE,CXLOOPJ          SET TO UPDATE PART2 INDEX                    
*        B     CLR2                                                             
*LR8A    CLI   FULL,0                                                           
*        BNE   CLRA                                                             
*                                                                               
*LR9     MVI   FULL,1              SECOND PASS DOES UPDATING                    
*        B     CLR0                                                             
*                                                                               
*LRA     MVC   MSG(19),=C'ALL REPORTS CLEARED'                                  
*        MVC   SRVMSG,MSG                                                       
*        OI    SRVMSGH+6,X'80'                                                  
*        OI    SRVP1H+6,X'40'                                                   
*        B     EXIT                                                             
*                                                                               
CLRERR   DC    H'0'                                                             
*                                                                               
***********************************************************************         
* DISPLAY REPORT COUNTERS                                                       
***********************************************************************         
CNTDSP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,SAVE                                                          
         MVI   0(R5),C' '                                                       
         MVC   1(99,R5),0(R5)                                                   
*                                                                               
CNTD2    CP    QR,=P'0'            TOTAL                                        
         BE    CNTD3                                                            
         MVCDD 0(4,R5),SR#TOTAL                                                 
         MVI   9(R5),C','                                                       
         EDIT  (P3,QR),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTD3    CP    QPID,=P'0'          PID=*                                        
         BE    CNTD4                                                            
         MVC   0(4,R5),=C'Mine'                                                 
         MVI   9(R5),C','                                                       
         EDIT  (P3,QPID),(5,4(R5))                                              
         LA    R5,10(R5)                                                        
*                                                                               
CNTD4    CP    QA,=P'0'            ACTIVE                                       
         BE    CNTD6                                                            
         MVCDD 0(4,R5),SR#ACTV                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QA),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTD6    CP    QH,=P'0'            HOLD                                         
         BE    CNTD8                                                            
         MVCDD 0(4,R5),SR#HOLD                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QH),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTD8    CP    QK,=P'0'            KEEP                                         
         BE    CNTDA                                                            
         MVCDD 0(4,R5),SR#KEEP                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QK),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTDA    CP    QD,=P'0'            PRINTED                                      
         BE    CNTDC                                                            
         MVCDD 0(4,R5),SR#PRTD                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QD),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTDC    CP    QE,=P'0'            SENT                                         
         BE    CNTDE                                                            
         MVCDD 0(4,R5),SR#SENT                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QE),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTDE    CP    QG,=P'0'            PRINTING                                     
         BE    CNTDF                                                            
         MVCDD 0(4,R5),SR#PNTG                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QG),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTDF    CP    QJ,=P'0'            SOON                                         
         BE    CNTDG                                                            
         MVCDD 0(4,R5),SR#SOON                                                  
         MVI   9(R5),C','                                                       
         EDIT  (P3,QJ),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTDG    CP    QI,=P'0'            INVISIBLE (DDS ONLY)                         
         BE    CNTDH                                                            
         TM    DDS,DDSTRM                                                       
         BZ    CNTDH                                                            
         MVCDD 0(4,R5),SR#INVIS                                                 
         MVI   9(R5),C','                                                       
         EDIT  (P3,QI),(5,4(R5))                                                
         LA    R5,10(R5)                                                        
*                                                                               
CNTDH    BCTR  R5,0                                                             
         MVI   0(R5),C' '                                                       
         LA    R5,SAVE             SQUASH AND PUT IN EQUAL SIGNS                
         LA    R6,99                                                            
         GOTO1 ASQUASH,DMCB,(R5),(C'=',(R6))                                    
*                                                                               
         MVC   SRVINF+14(L'SRVINF-14),0(R5)     MOVE TO TWA FIELD               
         TM    DDS,DDSTRM                                                       
         BZ    CNTDI                                                            
         CLI   PRTQID+4,C'U'       TEST DDS TERM AND MULTIPLE PRTQS             
         BE    CNTDSPX                                                          
         MVC   SRVINF(5),=C'PRTQU'                                              
         MVC   SRVINF+4(1),PRTQID+4                                             
         B     CNTDSPX                                                          
*                                                                               
CNTDI    MVCDD SRVINF(13),SR#RPTCT REPORT COUNT FOR NON DDS TRMS                
*                                                                               
CNTDSPX  J     XIT                                                              
         EJECT                                                                  
                                                                                
*                                                                               
                                                                                
***********************************************************************         
* OUTPUT REPORT STATUS                                                          
***********************************************************************         
STATOUT  NTR1  BASE=*,LABEL=*                                                   
*                                  (STAT)=SHORT STATUS                          
         MVI   STATA,C' '                                                       
         MVC   STATA+1(L'STATA-1),STATA                                         
         LA    RE,STATA            RE=A(NEXT OUTPUT BYTE)                       
         LA    RF,3                RF=L'OUTPUT-1                                
         MVI   STAT+1,C' '                                                      
*                                                                               
         CLI   CISTAT,PQSTPU       PURGED STATUS                                
         BNE   STOU1                                                            
         MVCDD 0(6,RE),SR#PRGED                                                 
         MVC   STAT(2),=C'**'                                                   
         B     STATOUTX                                                         
*                                                                               
STOU1    TM    CISTAT,PQSTAC       MAIN STATUS = ACTV/HOLD/PRTD/PNTG            
         BZ    STOU2                                                            
         MVCDD DUB(4),SR#ACTV                                                   
         MVC   STAT(1),SR2ACTV                                                  
STOU2    TM    CISTAT,PQSTHO                                                    
         BZ    STOU3                                                            
         MVCDD DUB(4),SR#HOLD                                                   
         MVC   STAT(1),SR2HOLD                                                  
STOU3    TM    CISTAT,PQSTPR                                                    
         BZ    STOU4                                                            
         MVCDD DUB(4),SR#PRTD                                                   
         MVC   STAT(1),SR2PRTD                                                  
STOU4    TM    CISTAT,PQSTSE                                                    
         BZ    STOU5                                                            
         MVCDD DUB(4),SR#SENT                                                   
         MVC   STAT(1),SR2SENT                                                  
STOU5    TM    CISTAT,PQSTPG                                                    
         BZ    STOU6                                                            
         MVCDD DUB(4),SR#PNTG                                                   
         MVC   STAT(1),SR2PNTG                                                  
STOU6    EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),DUB                                                      
         LA    RE,1(RE,RF)                                                      
*                                                                               
STOU7    TM    CISTAT,PQSTKE       SUB-STATUS-1 = KEEP                          
         BZ    STOU8                                                            
         MVI   0(RE),C','                                                       
         MVCDD 1(4,RE),SR#KEEP                                                  
         MVC   STAT+1(1),SR@KEEP                                                
         LA    RE,2(RE,RF)                                                      
         TM    CISTAT,PQSTIN                                                    
         BZ    STOU9                                                            
         MVI   0(RE),C','                                                       
         MVCDD 1(4,RE),SR#INVIS                                                 
         MVC   STAT+2(1),SR@INVIS                                               
         LA    RE,2(RE,RF)                                                      
         B     STOU9                                                            
*                                                                               
STOU8    TM    CISTAT,PQSTIN       SUB-STATUS-1 = INVISIBLE                     
         BZ    STOU9                                                            
         MVI   0(RE),C','                                                       
         MVCDD 1(4,RE),SR#INVIS                                                 
         MVC   STAT+1(1),SR@INVIS                                               
         LA    RE,2(RE,RF)                                                      
*                                                                               
STOU9    TM    PQTYPE,PQTYAD                                                    
         B     *+8                 *NOP* BNO - PAKS DONT LIKE THIS CHR          
         MVI   STAT+1,C'A'         SUB-STATUS-1 = ARCHIVED                      
*                                                                               
STATOUTX J     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET DATE/TIME PRTD IN MAIN CI REC                                             
***********************************************************************         
PRINTED  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,CXREC                                                         
         USING PQRECD,R5                                                        
         BRAS  RE,GETTIME          GET TIME IN DUB                              
         GOTO1 ADATAMGR,DMCB,(X'00',DMREAD),PRTQID,CIADDR,(R5)                  
         CLI   8(R1),0                                                          
         BNE   PRNERR                                                           
         LA    RF,DATEC                                                         
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    *+8                                                              
         LA    RF,DATECO                                                        
         MVC   PQDATED,0(RF)       SET DATE                                     
         MVC   PQTIMED,DUB         SET TIME                                     
*                                                                               
         CLC   CIADDR,=X'00010100'                                              
         JE    *+2                                                              
         GOTO1 ADATAMGR,DMCB,(X'00',DMWRT)                                      
         CLI   8(R1),0                                                          
         BNE   PRNERR                                                           
         J     XIT                                                              
*                                                                               
PRNERR   DC    H'0'                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO OUTPUT EXTRA INFO ON REPORT MAKER,FORMS,SECURITY         *         
***********************************************************************         
XTRA     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PQRECD,R5                                                        
         LA    R4,SWORK                                                         
         CLI   SWORK,2             FORMAT FOR GROUP DISPLAY                     
         BE    XTRAA                                                            
*                                                                               
XTRA1    XC    SWORK,SWORK         CLEAR DISPLAY AREA FOR FORMAT#1              
         MVC   DUB(5),PQMAKER      DISPLAY REPORT FORMS INFO                    
         OC    DUB(5),SPACES                                                    
         CLC   DUB(5),SPACES                                                    
         BNE   *+10                                                             
         MVC   DUB(3),=C'...'                                                   
         CLC   DUB+3(2),SPACES                                                  
         BNE   XTRA1B                                                           
XTRA1A   MVC   0(3,R4),DUB         OUTPUT SPP MAKER CODE                        
         MVI   3(R4),C','                                                       
         LA    R4,4(R4)                                                         
         B     XTRA2                                                            
XTRA1B   MVC   0(5,R4),DUB         OUTPUT SPPFF MAKER CODE                      
         MVI   5(R4),C','                                                       
         LA    R4,6(R4)                                                         
         B     XTRA2                                                            
XTRA2    MVC   DUB(4),PQFORMS                                                   
         OC    DUB(4),SPACES                                                    
         CLC   DUB(4),SPACES                                                    
         BE    XTRA3                                                            
         MVC   0(4,R4),DUB         OUTPUT FORMS CODE                            
         MVI   4(R4),C','                                                       
         LA    R4,5(R4)                                                         
XTRA3    EQU   *                   DONT DISPLAY CHARS ANYMORE                   
XTRA4    MVC   HALF(1),PQCOPIES    OUTPUT COPIES IF MORE THAN ONE               
         CLI   HALF,C'0'                                                        
         BL    *+8                 ALLOW C'0' THRU C'9'                         
         NI    HALF,X'0F'                                                       
         CLI   HALF,1                                                           
         BNH   XTRA5                                                            
         EDIT  (B1,HALF),(2,(R4)),ALIGN=LEFT                                    
XTRA5    LA    R4,SWORK+15                                                      
         LA    R0,15                                                            
         CLI   0(R4),C' '                                                       
         BH    XTRA6                                                            
         BCTR  R4,0                                                             
         BCT   R0,*-10                                                          
         B     XTRA7                                                            
XTRA6    CLI   0(R4),C','          REMOVE TRAILING COMMA                        
         BNE   *+8                                                              
         MVI   0(R4),0                                                          
*                                                                               
XTRA7    TM    PQATTB,PQATPW       INDICATE SECURE REPORT                       
         BZ    XTRA7X                                                           
         LA    R4,SWORK+20         SECURITY INFO SET HERE                       
         MVI   0(R4),C'Y'                                                       
         TM    PQSECF1,PQSINONO    TEST IF SECURITY FLAGS ARE VALID             
         BO    XTRA7X                                                           
         TM    PQSECF1,PQSIPIN     TEST IF PIN PROTECTED                        
         BZ    *+12                                                             
         MVI   0(R4),C'N'                                                       
         B     XTRA7X                                                           
         TM    PQSECF1,PQSIPID     TEST IF PID PROTECTED                        
         BZ    *+12                                                             
         MVI   0(R4),C'I'                                                       
         B     XTRA7X                                                           
         TM    PQSECF1,PQSIBNK     B=BANK                                       
         BZ    *+12                                                             
         MVI   0(R4),C'B'                                                       
         B     XTRA7X                                                           
         TM    PQSECF1,PQSISEC     C=CONTROL SECURITY                           
         BZ    *+12                                                             
         MVI   0(R4),C'C'                                                       
         B     XTRA7X                                                           
         TM    PQSECF1,PQSIPAY     P=PAYROLL                                    
         BZ    *+12                                                             
         MVI   0(R4),C'P'                                                       
         B     XTRA7X                                                           
         TM    PQSECF1,PQSISSN     S=SOCIAL SECURITY NUMBER                     
         BZ    *+12                                                             
         MVI   0(R4),C'S'                                                       
         B     XTRA7X                                                           
XTRA7X   EQU   *                                                                
*                                                                               
XTRA8    B     XTRAX                                                            
*                                                                               
XTRAA    XC    SWORK,SWORK         CLEAR DISPLAY AREA FOR FORMAT#2              
         MVC   SWORK(11),=CL11'... ... ...'                                     
         MVC   SWORK+12(4),SPACES  THIS GOES INTO LAST 4 CHRS OF PQDESC         
*                                                                               
         MVC   DUB(3),PQMAKER      OUTPUT SPP MAKER CODE                        
         OC    DUB(3),SPACES                                                    
         CLC   DUB(3),SPACES                                                    
         BE    *+10                                                             
         MVC   0(3,R4),DUB                                                      
*                                                                               
XTRAB    CLI   PQREPTY,C'A'        OUTPUT REPORT TYPE                           
         BL    *+10                                                             
         MVC   4(1,R4),PQREPTY                                                  
*                                                                               
XTRAC    CLC   PQFORMS,SPACES      OUTPUT FLAG IF NEEDS SPECIAL FORM            
         BNH   XTRAC3                                                           
         MVI   5(R4),C'Y'          SET SPECIAL FORM REQUIRED                    
         LA    RE,XTRAFTAB                                                      
XTRAC1   CLI   0(RE),C' '          TEST END OF FORMS TABLE                      
         BE    XTRAC3                                                           
         CLC   0(4,RE),PQFORMS                                                  
         BE    XTRAC2                                                           
         LA    RE,L'XTRAFTAB(RE)                                                
         B     XTRAC1                                                           
XTRAC2   MVC   5(1,R4),4(RE)       OUTPUT FORM TYPE CODE                        
XTRAC3   EQU   *                                                                
*&&UK                                                                           
XTRACK1  CLC   PQMAKER(3),=C'A56'  EXTRACT FORMS FOR A56                        
         BNE   XTRACK2                                                          
         MVI   5(R4),C'R'          SET SPECIAL FORM                             
         MVC   SWORK+12(4),PQFORMS                                              
         B     XTRACK9                                                          
XTRACK2  CLC   PQDESC(3),=C'A55'   EXTRACT FORMS FOR A55                        
         BNE   XTRACK3                                                          
*NOP*    MVC   5(1,R4),PQDESC+3    SET LEDGER IN FORMS COLUMN                   
         MVI   5(R4),C'C'          SET SPECIAL FORM                             
         MVC   SWORK+12(4),PQFORMS                                              
         B     XTRACK9                                                          
XTRACK3  CLC   PQSRCID,=H'4000'    EXTRACT FORMS FOR U=LASER                    
         BNE   XTRACK4                                                          
         MVI   5(R4),C'C'          SET SPECIAL FORM                             
*&&UK*&& B     *+10                                                             
         MVC   SWORK+12(4),PQFORMS                                              
         B     XTRACK9                                                          
XTRACK4  EQU   *                                                                
XTRACK9  EQU   *                                                                
*&&                                                                             
*&&US                                                                           
XTRACS1  CLC   PQDESC(3),=C'A56'   EXTRACT LEDGER FOR A56                       
         BNE   XTRACS2                                                          
         CLC   PQMAKER(3),=C'A56'                                               
         BNE   XTRACS2                                                          
         CLI   PQMAKER+4,C' '                                                   
         BNH   XTRACS2                                                          
         MVI   5(R4),C'R'          SET SPECIAL FORM                             
         MVC   SWORK+15(1),PQMAKER+4                                            
         B     XTRACS9                                                          
XTRACS2  CLC   PQDESC(3),=C'A55'   EXTRACT LEDGER FOR A55                       
         BNE   XTRACS3                                                          
         CLI   PQFORMS+3,C'$'                                                   
         BNE   XTRACS3                                                          
*NOP*    MVC   5(1,R4),PQFORMS+2   SET LEDGER IN FORMS COLUMN                   
         MVI   5(R4),C'C'          SET SPECIAL FORM                             
         MVC   SWORK+13(3),PQFORMS                                              
         B     XTRACS9                                                          
XTRACS3  CLC   PQSRCID,=H'4000'    EXTRACT VALUE FOR U=LASER                    
         BNE   XTRACS4                                                          
*NOP*    MVC   5(1,R4),PQFORMS     SET LEDGER IN FORMS COLUMN                   
         MVI   5(R4),C'C'          SET SPECIAL FORM                             
         MVC   SWORK+13(3),PQFORMS                                              
         B     XTRACS9                                                          
XTRACS4  EQU   *                                                                
XTRACS9  EQU   *                                                                
*&&                                                                             
XTRAD    TM    PQTYP1,PQTYAE       OUTPUT ARCHIVE STATUS FLAG                   
         BNO   *+8                                                              
         MVI   6(R4),C'E'          INDICATE ELIGIBLE FOR ARCHIVE                
         TM    PQTYP1,PQTYAR                                                    
         BNO   *+8                                                              
         MVI   6(R4),C'A'          INDICATE ARCHIVABLE                          
         TM    PQTYP1,PQTYAD                                                    
         BNO   *+8                                                              
         MVI   6(R4),C'D'          INDICATE ARCHIVED                            
*                                                                               
XTRAE    LA    R4,8(R4)            SECURITY INFO SET HERE                       
         TM    PQATTB,PQATPW       INDICATE SECURE REPORT                       
         BZ    XTRAEX                                                           
         TM    PQSECF1,PQSINONO    TEST IF SECURITY FLAGS ARE VALID             
         BO    XTRAEX                                                           
         TM    PQSECF1,PQSIPIN     TEST IF PIN PROTECTED                        
         BZ    *+12                                                             
         MVI   0(R4),C'N'                                                       
         B     XTRAEX                                                           
         TM    PQSECF1,PQSIPID     TEST IF PID PROTECTED                        
         BZ    *+12                                                             
         MVI   0(R4),C'I'                                                       
         B     XTRAEX                                                           
         TM    PQSECF1,PQSIBNK     B=BANK                                       
         BZ    *+12                                                             
         MVI   1(R4),C'B'                                                       
         B     XTRAEX                                                           
         TM    PQSECF1,PQSISEC     C=CONTROL SECURITY                           
         BZ    *+12                                                             
         MVI   1(R4),C'C'                                                       
         B     XTRAEX                                                           
         TM    PQSECF1,PQSIPAY     P=PAYROLL                                    
         BZ    *+12                                                             
         MVI   1(R4),C'P'                                                       
         B     XTRAEX                                                           
         TM    PQSECF1,PQSISSN     S=SOCIAL SECURITY NUMBER                     
         BZ    *+12                                                             
         MVI   1(R4),C'S'                                                       
         B     XTRAEX                                                           
XTRAEX   EQU   *                                                                
*                                                                               
XTRAF    OC    PQPIDNUM,PQPIDNUM   TEST IF PID ATTACHED TO REPORT               
         BNZ   *+12                                                             
         MVI   2(R4),C'.'          SET PID UNKNOWN                              
         B     XTRAFX                                                           
         MVI   2(R4),C'Y'          SET PID ATTACHED TO REPORT                   
         CLC   PQPIDNUM,TRMPID                                                  
         BNE   XTRAFX                                                           
         MVI   2(R4),C'*'          SET LOGON PID ATTACHED TO REPORT             
         B     XTRAFX                                                           
XTRAFX   EQU   *                                                                
*                                                                               
XTRAX    J     XIT                                                              
*                                                                               
XTRAFTAB DS    0CL5                                                             
         DC    C'DOWN',C'.'                                                     
         DC    C'DSML',C'.'                                                     
         DC    C'1P  ',C'.'                                                     
         DC    C'1PLN',C'.'                                                     
         DC    C'1PP ',C'.'                                                     
         DC    C'1S  ',C'.'                                                     
         DC    C'1SML',C'.'                                                     
         DC    C'1SP ',C'.'                                                     
         DC    C'2P  ',C'.'                                                     
         DC    C'2PLN',C'.'                                                     
         DC    C'2PP ',C'.'                                                     
         DC    C'2S  ',C'.'                                                     
         DC    C'2SML',C'.'                                                     
         DC    C'2SP ',C'.'                                                     
*&&US*&& DC    C'3LBL',C'L'        LABLES                                       
*&&UK*&& DC    C'LABS',C'L'        LABLES                                       
*&&UK*&& DC    C'LBLS',C'L'        LABLES                                       
XTRAFTAX DC    C'    ',C'.'                                                     
         EJECT                                                                  
                                                                                
*&&NOP                                                                          
***********************************************************************         
* PRTQ SIZE DISPLAY - OLD VERSION USING PQSCAN VALUES.                *         
***********************************************************************         
NSIZE    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SRVEX2,SIZEH0                                                    
         MVC   SRVHD1,SIZEH1                                                    
         L     RF,ACTREC                                                        
         XC    0(48,RF),0(RF)                                                   
         MVI   0(RF),X'FF'                                                      
         LR    RE,RF                                                            
         LA    RE,48(RE)                                                        
         GOTO1 AQSCAN,DMCB,(RC),(RF),(RE)                                       
         LA    R4,SRVSA1H                                                       
         USING SIZED,R4                                                         
         L     R6,ACTREC                                                        
         LA    R6,48(R6)                                                        
         USING QSCOUNTD,R6                                                      
*                                                                               
         LA    RE,PRTQLST+8                                                     
         ST    RE,APRTQLST                                                      
         CLI   ACTN1,2                                                          
         BNE   NSIZE1                                                           
         CLI   PQSQUEUE,0                                                       
         BE    NSIZE1                                                           
         MVC   PRTQID+4(1),PQSQUEUE                                             
         B     *+10                                                             
NSIZE1   MVC   PRTQID+4(1),1(RE)                                                
         MVI   PRTQID+5,C' '                                                    
         MVC   SZQUE,PRTQID                                                     
         GOTO1 ADATAMGR,PQDMCB,(0,BUFFER),PRTQID,,,CXREC                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFDATA,CXREC                                                   
         MVC   CIDATA,CXREC+12                                                  
*                                                                               
         SAM31                                                                  
         MVC   FIWRES,PRTQID                                                    
         BRAS  RE,FIRSET                                                        
         JNE   *+2                                                              
         MVC   FIWCIA,CIADDR                                                    
         BRAS  RE,FIRCN                                                         
         SAM24                                                                  
*                                                                               
         CLI   ACTN1,1                                                          
         BE    NSIZEPC                                                          
         CLI   ACTN1,2                                                          
         BE    NSIZEFIL                                                         
                                                                                
*-----------------------------------                                            
* PRTQ SIZE CONTROL INTERVAL COUNTS                                             
*-----------------------------------                                            
         SR    R0,R0               TOTAL PART1                                  
         ICM   R0,3,CICITOT                                                     
         EDIT  (R0),(6,SZTOT1)                                                  
*                                                                               
         LH    RF,CICITOT          PART1 INUSE                                  
         SH    RF,CICINDX                                                       
         ZAP   DUB,QSC1                                                         
         CVB   R1,DUB                                                           
         SR    RF,R1                                                            
         EDIT  (RF),(6,SZAVA1),ZERO=NOBLANK AVAIL PART1                         
*                                                                               
         ZAP   DUB,QSC3            ACTIVE PART1                                 
         CVB   RF,DUB                                                           
         SR    R1,RF               SUBTRACT ACTIVES FROM INUSE                  
         EDIT  (R1),(6,SZPRT1),ZERO=NOBLANK                                     
*                                                                               
         EDIT  (P3,QSC3),(6,SZACT1),ZERO=NOBLANK                                
         EDIT  (B2,CICINDX),(6,SZIND1)                                          
*                                                                               
         SR    R0,R0               TOTAL PART2                                  
         ICM   R0,3,CJCITOT                                                     
         EDIT  (R0),(6,SZTOT2)                                                  
         SR    RF,RF                                                            
         ICM   RF,3,CJCITOT                                                     
         ZAP   DUB,QSC2                                                         
         CVB   R1,DUB                                                           
         SR    RF,R1                                                            
         EDIT  (RF),(6,SZAVA2),ZERO=NOBLANK AVAIL PART2                         
*                                                                               
         ZAP   DUB,QSC4            ACTIVE PART2                                 
         CVB   RF,DUB                                                           
         SR    R1,RF               SUBTRACT ACTIVES FROM INUSE                  
         EDIT  (R1),(6,SZPRT2),ZERO=NOBLANK                                     
*                                                                               
         EDIT  (P3,QSC4),(6,SZACT2),ZERO=NOBLANK ACTIV PART2                    
         B     NSIZENXT                                                         
                                                                                
*----------------------------------------                                       
* PRTQ SIZE CONTROL INTERVAL PERCENTAGES                                        
*----------------------------------------                                       
NSIZEPC  SR    R0,R0               TOTAL PART1                                  
         ICM   R0,3,CICITOT                                                     
         EDIT  (R0),(6,SZTOT1)                                                  
*                                                                               
         LH    RF,CICITOT                                                       
         SH    RF,CICINDX                                                       
         ZAP   DUB,QSC1            PART1 INUSE                                  
         CVB   R1,DUB                                                           
         SR    RF,R1                                                            
         SR    RE,RE                                                            
         ST    R1,FULL             SAVE INUSE                                   
         MH    RF,=H'1000'                                                      
         LH    R1,CICITOT                                                       
         DR    RE,R1                                                            
         LA    R1,SZAVA1                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         L     RF,FULL             RESTORE INUSE                                
         ZAP   DUB,QSC3            ACTIVE PART1                                 
         CVB   R1,DUB                                                           
         SR    RF,R1               SUBTRACT ACTIVES FROM INUSE                  
         SR    RE,RE                                                            
         MH    RF,=H'1000'                                                      
         LH    R1,CICITOT                                                       
         DR    RE,R1                                                            
         LA    R1,SZPRT1                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         ZAP   DUB,QSC3                                                         
         CVB   RF,DUB                                                           
         SR    RE,RE                                                            
         MH    RF,=H'1000'                                                      
         LH    R1,CICITOT                                                       
         DR    RE,R1                                                            
         LA    R1,SZACT1                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         SR    RE,RE                                                            
         LH    RF,CICINDX                                                       
         MH    RF,=H'1000'                                                      
         LH    R1,CICITOT                                                       
         DR    RE,R1                                                            
         LA    R1,SZIND1                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         SR    R0,R0               TOTAL PART1                                  
         ICM   R0,3,CJCITOT                                                     
         EDIT  (R0),(6,SZTOT2)                                                  
*                                                                               
         LH    RF,CJCITOT                                                       
         ZAP   DUB,QSC2            PART2 INUSE                                  
         CVB   R1,DUB                                                           
         SR    RF,R1                                                            
         ST    R1,FULL             SAVE INUSE                                   
         SR    RE,RE                                                            
         MH    RF,=H'1000'                                                      
         LH    R1,CJCITOT                                                       
         DR    RE,R1                                                            
         LA    R1,SZAVA2                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         L     RF,FULL             RESTORE INUSE                                
         ZAP   DUB,QSC4            ACTIVE PART2                                 
         CVB   R1,DUB                                                           
         SR    RF,R1               SUBTRACT ACTIVES FROM INUSE                  
         SR    RE,RE                                                            
         MH    RF,=H'1000'                                                      
         LH    R1,CJCITOT                                                       
         DR    RE,R1                                                            
         LA    R1,SZPRT2                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         ZAP   DUB,QSC4                                                         
         CVB   RF,DUB                                                           
         SR    RE,RE                                                            
         MH    RF,=H'1000'                                                      
         LH    R1,CJCITOT                                                       
         DR    RE,R1                                                            
         LA    R1,SZACT2                                                        
         BRAS  RE,EDITPCS                                                       
         B     NSIZENXT                                                         
                                                                                
*----------------------------------                                             
* DISPLAY FILE SIZE INFO                                                        
*----------------------------------                                             
NSIZEFIL MVC   SRVEX2,SIZEH2                                                    
         MVC   SRVHD1,SIZEH3                                                    
         LA    R4,SRVSA1H                                                       
         USING PQNLD,R4                                                         
         MVC   PQNLDATA(13),=C'Trks/Part1 CI'                                   
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CITRKS                                                        
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(13),=C'Trks/Part2 CI'                                   
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CJTRKS                                                        
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(14),=C'Trks for index'                                  
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(14),=C'Trks for Part1'                                  
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CICITOT                                                       
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(14),=C'Trks for Part2'                                  
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CJCITOT                                                       
         MH    R0,CJTRKS                                                        
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(13),=C'Record length'                                   
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CIBLKLN                                                       
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(13),=C'Records/track'                                   
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CIHIREC                                                       
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         MVC   MSG(30),=C'File size attributes displayed'                       
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     NSIZEX                                                           
*                                                                               
SIZEOUT  EDIT  (R0),(7,0(RF))             SUBROUTINE TO EDIT NUMBERS            
         BR    RE                                                               
*                                                                               
NSIZENXT LA    R4,93(R4)                  NEXT LINE                             
         LA    R6,48(R6)                  NEXT COUNTS                           
         ICM   RE,15,APRTQLST             NEXT QUEUE                            
         LA    RE,8(RE)                                                         
         ST    RE,APRTQLST                                                      
         CLI   0(RE),0                                                          
         BNE   NSIZE1                                                           
         MVC   MSG(30),=C'File size attributes displayed'                       
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
NSIZEX   J     XIT                                                              
*                                                                               
*              CL40'.........+.........+.........+.........+'                   
SIZEH0   DC    CL40'              ---------- Part#1 CIs ----'                   
         DC    CL39'-------   ------- Part#2 CIs -------   '                    
SIZEH1   DC    CL40'Actn Queue    Total  Avail Vunrbl Unaval'                   
         DC    CL39'  Index   Total  Avail Vunrbl Unaval   '                    
SIZEH2   DC    CL79' '                                                          
SIZEH3   DC    CL79'Actn Prtq attribute   Value'                                
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* PRTQ SIZE DISPLAY - NEW VERSION USING PQMON VALUES IN SHMEM         *         
***********************************************************************         
NSIZE    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SRVEX2,SIZEH0       SET HEADERS                                  
         MVC   SRVHD1,SIZEH1                                                    
         MVI   PQSFLAG,0                                                        
         SAM31                                                                  
*                                                                               
         LA    R4,SRVSA1H          POINT TO FIRST LIST FIELD                    
         USING SIZED,R4                                                         
*                                                                               
         L     R7,FIWSHA           SHARED MEMORY AREA                           
         USING SIHDRD,R7                                                        
         L     R6,SIHNOFR          NUMBER OF RESOURCES (R6)                     
         LA    R7,L'SIHDR(,R7)                                                  
         USING SITABD,R7                                                        
*                                                                               
NSIZE1   MVC   SZQUE,SITFIL        FILENAME                                     
*                                                                               
         CLI   ACTN1,1             SIZE,PERCENT                                 
         BE    NSIZEPC                                                          
         CLI   ACTN1,2             SIZE,FILE                                    
         BE    NSIZEFIL                                                         
                                                                                
*-----------------------------------                                            
* PRTQ SIZE CONTROL INTERVAL COUNTS                                             
*-----------------------------------                                            
         L     RF,SIT1CIC          TOTAL PART1                                  
         LR    R5,RF                                                            
         EDIT  (RF),(6,SZTOT1)                                                  
*                                                                               
         L     RF,SITXCIC          PART1 INDEX                                  
         EDIT  (RF),(6,SZIND1),ZERO=NOBLANK                                     
*                                                                               
         LNR   RF,RF                                                            
         A     RF,SITP1AV          PART1 AVAILABLE                              
         SR    R5,RF                                                            
         EDIT  (RF),(6,SZAVA1),ZERO=NOBLANK                                     
*                                                                               
         L     RF,SITP1VU          PART1 VULNERABLE                             
         SR    R5,RF                                                            
         EDIT  (RF),(6,SZPRT1),ZERO=NOBLANK                                     
*                                                                               
         LR    RF,R5               THE REST ARE ACTIVE                          
         EDIT  (RF),(6,SZACT1),ZERO=NOBLANK                                     
*                                                                               
         L     RF,SIT2CIC          TOTAL PART2                                  
         LR    R5,RF                                                            
         EDIT  (RF),(6,SZTOT2)                                                  
*                                                                               
         L     RF,SITP2AV          PART2 AVAILABLE                              
         SR    R5,RF                                                            
         EDIT  (RF),(6,SZAVA2),ZERO=NOBLANK                                     
*                                                                               
         L     RF,SITP2VU          PART2 VULNERABLE                             
         SR    R5,RF                                                            
         EDIT  (RF),(6,SZPRT2),ZERO=NOBLANK                                     
*                                                                               
         LR    RF,R5               THE REST ARE ACTIVE                          
         EDIT  (RF),(6,SZACT2),ZERO=NOBLANK                                     
*                                                                               
         B     NSIZENXT                                                         
                                                                                
*----------------------------------------                                       
* PRTQ SIZE CONTROL INTERVAL PERCENTAGES                                        
*----------------------------------------                                       
NSIZEPC  MVI   PQSFLAG,C'%'                                                     
         L     RF,SIT1CIC          TOTAL PART1                                  
         LR    R5,RF                                                            
         ST    RF,FULL             SAVE TOTAL IN FULL                           
         EDIT  (RF),(6,SZTOT1)                                                  
*                                                                               
         L     RF,SITXCIC          PART1 INDEX                                  
         MHI   RF,1000                                                          
         XR    RE,RE                                                            
         D     RE,FULL                                                          
         LA    R1,SZIND1                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         L     RF,SITXCIC          PART1 INDEX                                  
         LNR   RF,RF                                                            
         A     RF,SITP1AV          PART1 AVAILABLE                              
         SR    R5,RF                                                            
         MHI   RF,1000                                                          
         XR    RE,RE                                                            
         D     RE,FULL                                                          
         LA    R1,SZAVA1                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         L     RF,SITP1VU          PART1 VULNERABLE                             
         SR    R5,RF                                                            
         MHI   RF,1000                                                          
         XR    RE,RE                                                            
         D     RE,FULL                                                          
         LA    R1,SZPRT1                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         LR    RF,R5               THE REST ARE ACTIVE                          
         MHI   RF,1000                                                          
         XR    RE,RE                                                            
         D     RE,FULL                                                          
         LA    R1,SZACT1                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         L     RF,SIT2CIC          TOTAL PART2                                  
         LR    R5,RF                                                            
         ST    RF,FULL             SAVE TOTAL IN FULL                           
         EDIT  (RF),(6,SZTOT2)                                                  
*                                                                               
         L     RF,SITP2AV          PART2 AVAILABLE                              
         SR    R5,RF                                                            
         MHI   RF,1000                                                          
         XR    RE,RE                                                            
         D     RE,FULL                                                          
         LA    R1,SZAVA2                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         L     RF,SITP2VU          PART2 VULNERABLE                             
         SR    R5,RF                                                            
         MHI   RF,1000                                                          
         XR    RE,RE                                                            
         D     RE,FULL                                                          
         LA    R1,SZPRT2                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         LR    RF,R5               THE REST ARE ACTIVE                          
         MHI   RF,1000                                                          
         XR    RE,RE                                                            
         D     RE,FULL                                                          
         LA    R1,SZACT2                                                        
         BRAS  RE,EDITPCS                                                       
*                                                                               
         B     NSIZENXT                                                         
*----------------------------------                                             
* DISPLAY FILE SIZE INFO                                                        
*----------------------------------                                             
NSIZEFIL MVC   SRVEX2,SIZEH2                                                    
         MVC   SRVHD1,SIZEH3                                                    
         LA    R4,SRVSA1H                                                       
         USING PQNLD,R4                                                         
         MVC   PQNLDATA(13),=C'Trks/Part1 CI'                                   
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CITRKS                                                        
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(13),=C'Trks/Part2 CI'                                   
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CJTRKS                                                        
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(14),=C'Trks for index'                                  
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(14),=C'Trks for Part1'                                  
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CICITOT                                                       
         SH    R0,CICINDX                                                       
         MH    R0,CITRKS                                                        
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(14),=C'Trks for Part2'                                  
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CJCITOT                                                       
         MH    R0,CJTRKS                                                        
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(13),=C'Record length'                                   
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CIBLKLN                                                       
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(13),=C'Records/track'                                   
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CIHIREC                                                       
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         LA    R4,93(R4)                                                        
         MVC   PQNLDATA(13),=C'Part2 Blk/Trk'                                   
         OI    PQNLHDR+6,X'80'                                                  
         LH    R0,CJHIREC                                                       
         LA    RF,PQNLDATA+15                                                   
         BRAS  RE,SIZEOUT                                                       
*                                                                               
         MVC   MSG(30),=C'File size attributes displayed'                       
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
         B     NSIZEX                                                           
*                                                                               
SIZEOUT  EDIT  (R0),(7,0(RF))             SUBROUTINE TO EDIT NUMBERS            
         BR    RE                                                               
*                                                                               
NSIZENXT LA    R4,93(R4)                  NEXT LINE                             
         LA    R7,L'SITAB(,R7)                                                  
         JCT   R6,NSIZE1                  DO FOR EACH RESOURCE                  
*                                                                               
         MVC   MSG(30),=C'File size attributes displayed'                       
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'                                                  
         OI    SRVP1H+6,X'40'                                                   
NSIZEX   J     XIT                                                              
         DROP  R7                                                               
*                                                                               
*              CL40'.........+.........+.........+.........+'                   
SIZEH0   DC    CL40'              ---------- Part#1 CIs ----'                   
         DC    CL39'-------   ------- Part#2 CIs -------   '                    
SIZEH1   DC    CL40'Actn Queue    Total  Avail Vunrbl Unaval'                   
         DC    CL39'  Index   Total  Avail Vunrbl Unaval   '                    
SIZEH2   DC    CL79' '                                                          
SIZEH3   DC    CL79'Actn Prtq attribute   Value'                                
         EJECT                                                                  
***********************************************************************         
* EXITS, SHARED ROUTINES, CONSTANTS AND OTHER COMMONLY SHARED VALUES            
***********************************************************************         
         DROP  RB                                                               
COMMON   DS    0D                                                               
*                                                                               
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
* TIMEOUT                                                                       
***********************************************************************         
TIMEOUT  XC    DUB+2(6),DUB+2      EXPAND BINARY TIME IN DUB(2)                 
         CLI   DUB,23                                                           
         BH    TIMEOUTX                                                         
         CLI   DUB+1,59                                                         
         BH    TIMEOUTX                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,DUB                                                           
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+2(2),DUB1+6(2)                                               
*                                                                               
         MVI   DUB+4,C':'                                                       
         SR    R0,R0                                                            
         IC    R0,DUB+1                                                         
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+5(2),DUB1+6(2)                                               
*                                                                               
TIMEOUTX BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* GETTIME                                                                       
***********************************************************************         
GETTIME  ST    RE,SAVERE                                                        
         TBIN  SECS                                                             
         SR    R0,R0                                                            
         D     R0,=F'60'           R1=BINARY MINS                               
         ST    R1,FULL1                                                         
         SR    R0,R0                                                            
         D     R0,=F'60'           R1=BINARY HOURS                              
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* EDITPCS                                                                       
***********************************************************************         
EDITPCS  LTR   RF,RF               SHOW 0% AS 0%                                
         BNE   *+12                                                             
         MVC   4(2,R1),=C'0%'                                                   
         BR    RE                                                               
         CH    RF,=H'1000'         SHOW 100% AS 100%                            
         BNE   *+12                                                             
         MVC   2(4,R1),=C'100%'                                                 
         BR    RE                                                               
         EDIT  (RF),(6,0(R1)),1,TRAIL=C'%'                                      
         CH    RF,=H'10'                                                        
         BNLR  RE                                                               
         MVI   2(R1),C'0'          FILL IN MISSING ZERO IF .X%                  
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ERROR EXITS                                                                   
***********************************************************************         
ERR0     LA    RE,SREIIS           INVALID INPUT FIELD SYNTAX                   
         B     ERRX                                                             
ERR1     LA    RE,168              MISSING REPORT TYPE                          
         B     ERRX                                                             
ERR2     LA    RE,169              INVALID REPORT TYPE                          
         B     ERRX                                                             
ERR3     LA    RE,170              DUPLICATE REPORT TYPE                        
         B     ERRX                                                             
ERR4     LA    RE,171              INCOMPATIBLE REPORT TYPE                     
         B     ERRX                                                             
ERR6     LA    RE,SRERNF           REPORT NOT FOUND                             
         B     ERRX                                                             
ERR7     MVI   BYTE,172            REPORT NOT ACTIVE                            
         B     ERRD                                                             
ERR8     MVI   BYTE,173            REPORT NOT HOLD OR PRINTED                   
         B     ERRD                                                             
ERR9     MVI   BYTE,174            REPORT IS ALREADY KEEP                       
         B     ERRD                                                             
ERR10    MVI   BYTE,175            REPORT NOT KEEP                              
         B     ERRD                                                             
ERR11    LA    RE,SRENRF           NO REPORTS FOUND                             
         B     ERRX                                                             
ERR12    LA    RE,176              ALL REPORTS MUST BE PRNTD OR PRGD            
         B     ERRX                                                             
ERR13    LA    RE,SREIRI           INVALID REPORT ID                            
         B     ERRX                                                             
ERR14    MVI   BYTE,177            REPORT IS PRNTNG CANT CHNG STATUS            
         B     ERRD                                                             
ERR15    MVI   BYTE,193            NOT AVAILABLE FOR CLASS N                    
         B     ERRD                                                             
ERR16    MVI   BYTE,194            REPORT MUST BE INVISIVLE                     
         B     ERRD                                                             
ERR17    MVI   BYTE,195            REPORT NOT INVISIBLE                         
         B     ERRD                                                             
ERR18    MVI   BYTE,195            REPORT NOT ELIGIBLE FOR ARCHIVE              
         B     ERRD                                                             
ERR19    MVI   BYTE,195            NOT ALLOWED TO CHANGE ARCHIVE STATUS         
         B     ERRD                                                             
ERR20    MVI   BYTE,195            NOT ALLOWED TO CHANGE BACKUP STATUS          
         B     ERRD                                                             
ERRF     LA    RE,SREIFK1          INVALID FILTER KEYWORD=                      
         LA    RF,12(R6)                                                        
         ST    RF,TXTADR                                                        
         MVI   TXTLEN,10                                                        
         B     ERRX                                                             
ERRFA    LA    RE,SREIFSK1         INVALID FILTER SIGN KEYWORD=                 
         LA    RF,12(R6)                                                        
         ST    RF,TXTADR                                                        
         MVI   TXTLEN,10                                                        
         B     ERRX                                                             
ERRFB    LA    RE,SREIVFK1         INVALID VALUE FOR FILTER KEYWORD=            
         LA    RF,12(R6)                                                        
         ST    RF,TXTADR                                                        
         MVI   TXTLEN,10                                                        
         B     ERRX                                                             
*                                                                               
ERRD     TM    INTFLAG,INTCONT     DISPLAY REPORT BEFORE EXIT                   
         BNO   INDDSP                                                           
ERRDX    EQU   *                   INDDSP WILL RETURN HERE                      
         LA    R4,SRVP2H                                                        
         SR    RE,RE                                                            
         IC    RE,BYTE                                                          
         TM    INTFLAG,INTSEL                                                   
         BNO   ERRX                                                             
         LA    R4,SRVP3H           CURSOR ON P3 IF SELECT                       
*                                                                               
ERRX     OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'                                                      
         TM    INTFLAG,INTRUN      IF ACTION WAS INTERNAL                       
         BNO   *+8                                                              
         OI    INTFLAG,INTERR      FLAG ERROR FOR ROOT                          
*                                                                               
         GOTO1 AGETTXT,DMCB,(RE),0,(C'E',0),(TXTLEN,TXTADR),0,X'010000'         
EXIT     MVC   USERID,HALF1        RESTORE USERID                               
*                                                                               
EXIT1    XMOD1 1                                                                
*                                                                               
INDERR   DC    H'0'                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* SHARED MEMORY ROUTINES                                                        
***********************************************************************         
       ++INCLUDE DDSHFIR                                                        
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CONSTANTS AND LITERALS                                                        
***********************************************************************         
MAXSEQ   DC    F'65000'                                                         
*AXDATE  DC    X'BF9F'             2059-12-31 DATE IN NEW CMPRSD FORMAT         
MAXDATE  DC    X'FF9F'             SAME FOR NOW                                 
MAXDATEO DC    X'FF9F'             2027-12-31 DATE IN OLD CMPRSD FORMAT         
DSPMAX   DC    PL2'16'                                                          
QTMAX    DC    PL2'600'                                                         
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
BUFFER   DC    CL8'BUFFER'                                                      
RANDOM   DC    CL8'RANDOM'                                                      
DC@XSORT DC    CL5'XSORT'                                                       
DC@SHIT  DC    CL4'SHIT'                                                        
DC@JOBI  DC    CL4'JOBI'                                                        
DC@JOBO  DC    CL4'JOBO'                                                        
DC@EXT   DC    CL4'EXT  '                                                       
DC@ARCA  DC    CL8'ARCA    '                                                    
DC@ARCD  DC    CL8'ARCD    '                                                    
DC@ARCE  DC    CL8'ARCE    '                                                    
DC@ARCI  DC    CL8'ARCI    '                                                    
DC@BKUP  DC    CL8'BKUP    '                                                    
DC@REP   DC    CL8'REP     '                                                    
DC@PID   DC    CL8'PID     '                                                    
DC@PIN   DC    CL8'PIN     '                                                    
DC@NCD   DC    CL8'NCD     '                                                    
FFS      DC    16X'FF'                                                          
DOTS     DC    16C'.'                                                           
SPACES   DC    64C' '                                                           
*                                                                               
SIZEL    DC    CL78'PART1 CI TOTAL=1234,AVAIL=1234,INUSE=1234,INDEX=123X        
               4        '                                                       
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* TABLE OF VALID SORT KEYWORDS                                                  
* LA RF,KEYWORD,(+VE ACTN),(-VE ACTN),LEN,N/D                                   
***********************************************************************         
         DS    0H                                                               
SORTTBL  DS    0CL8                                                             
         DC    X'41F0',S(SR@ALPHA),X'01',X'01',X'05',X'00'                      
         DC    X'41F0',S(SR@CRTD),X'02',X'03',X'07',X'00'                       
         DC    X'41F0',S(SR@PRTD),X'04',X'05',X'07',X'00'                       
         DC    X'41F0',S(SR@RTAIN),X'06',X'07',X'06',X'00'                      
         DC    X'41F0',S(SR@SIZE),X'08',X'09',X'04',X'00'                       
SORTBLX  DC    X'00'                                                            
                                                                                
***********************************************************************         
* ATTRIBUTE AND TYPE INDICATORS                                                 
***********************************************************************         
ATTIND   DC    C'PNUKWEOI'         ATTRIBUTE INDICATORS                         
TYPIND   DC    C'U..DQXXN'         TYPE INDICATORS                              
TY1IND   DC    C'EADBRLN.'         TYP1 INDICATORS                              
                                                                                
***********************************************************************         
* TABLE OF VALID KEYWORDS FOR FILTERS                                           
* XL4    KEYWORD NAME IN DD DYNAMIC FORMAT                                      
* XL1    KEYWORD NUMBER                                                         
* XL1    KEYWORD SIGN BITS X'80'=NE,X'40'=LT,X'20'=GT                           
* XL1    KEYWORD VALD BITS X'01'=DDSONLY                                        
***********************************************************************         
         DS    0H                                                               
FILTTBL  DS    0CL8                                                             
         DC    X'41F0',S(SR@CLASS),X'01',X'80',X'00',X'00' CLASS                
         DC    X'41F0',S(SR@DATE),X'02',X'E0',X'00',X'00'  DATE                 
         DC    X'41F0',S(SR@DDS),X'03',X'00',X'01',X'00'   DDS                  
         DC    X'41F0',S(SR@SIZE),X'04',X'E0',X'01',X'00'  SIZE FACTOR          
         DC    X'41F0',S(SR@SORT),X'05',X'00',X'00',X'00'  SORT ORDER           
         DC    X'41F0',S(DC@XSORT),X'F5',X'00',X'01',X'00' SORT U=XXX           
         DC    X'41F0',S(SR@TIME),X'06',X'60',X'00',X'00'  TIME                 
         DC    X'41F0',S(SR@FMT),X'07',X'00',X'01',X'00'   DISP FRMT            
         DC    X'41F0',S(SR@CDATE),X'08',X'E0',X'00',X'00' DT CREATED           
         DC    X'41F0',S(SR@PDATE),X'09',X'E0',X'00',X'00' DT PRINTED           
         DC    X'41F0',S(SR@SDATE),X'09',X'E0',X'00',X'00' DT SENT              
         DC    X'41F0',S(SR@RDATE),X'0A',X'E0',X'00',X'00' DT RETAINED          
         DC    X'41F0',S(DC@PID),X'0B',X'00',X'00',X'00'   PID                  
         DC    X'41F0',S(DC@PIN),X'0C',X'00',X'00',X'00'   PIN                  
         DC    X'00'                                                            
                                                                                
***********************************************************************         
* TABLE OF VALID REPORT TYPE (STATUS) VALUES AND BIT VALUES                     
***********************************************************************         
         DS    0H                                                               
TYPETBL  DS    0CL6                                                             
         DC    X'41F0',S(SR8ALL),X'FF',X'00'   ALL                              
         DC    X'41F0',S(SR@LIVE),X'C0',X'00'  LIVE=ACTV/HOLD                   
         DC    X'41F0',S(SR@ACTV),X'80',X'00'  ACTI                             
         DC    X'41F0',S(SR4ACTV),X'80',X'00'  ACTV                             
         DC    X'41F0',S(SR@HOLD),X'40',X'00'  HOLD                             
         DC    X'41F0',S(SR@DEAD),X'30',X'00'  DEAD=PRTD/SENT                   
         DC    X'41F0',S(SR@PRTD),X'20',X'00'  PRINTED                          
         DC    X'41F0',S(SR4PRTD),X'20',X'00'  PRTD                             
         DC    X'41F0',S(SR@SENT),X'10',X'00'  SENT                             
         DC    X'41F0',S(SR@KEEP),X'08',X'00'  KEEP                             
         DC    X'41F0',S(SR@INVIS),X'02',X'00' INVISIBLE                        
         DC    X'41F0',S(SR@PNTG),X'01',X'00'  PRINTING                         
         DC    X'41F0',S(SR4PNTG),X'01',X'00'  PNTG                             
TYPETBLX DC    X'00'                                                            
                                                                                
***********************************************************************         
* TABLE OF VALID REPORT TYPE#1 VALUES AND BIT VALUES                            
***********************************************************************         
         DS    0H                                                               
TYP1TBL  DS    0CL6                                                             
         DC    X'41F0',S(DC@ARCE),X'80',X'00'  ARCE                             
         DC    X'41F0',S(DC@ARCA),X'40',X'00'  ARCA                             
         DC    X'41F0',S(DC@ARCD),X'20',X'00'  ARCD                             
         DC    X'41F0',S(DC@ARCI),X'E0',X'00'  ARCI    (ANY ARC BITS)           
         DC    X'41F0',S(SR@ARCE),X'80',X'00'  ARCHIVEL                         
         DC    X'41F0',S(SR@ARCA),X'40',X'00'  ARCHIVAB                         
         DC    X'41F0',S(SR@ARCD),X'20',X'00'  ARCHIVED                         
         DC    X'41F0',S(SR@ARCI),X'E0',X'00'  ARCHIVE (ANY ARC BITS)           
         DC    X'41F0',S(DC@BKUP),X'10',X'00'  BKUP                             
         DC    X'41F0',S(SR@BKUP),X'10',X'00'  BACKED UP TO TAPE                
         DC    X'41F0',S(DC@REP),X'08',X'00'   CREATED BY REP SYSTEM            
         DC    X'41F0',S(DC@NCD),X'02',X'00'   NEW COMP DATE                    
TYP1TBLX DC    X'00'                                                            
                                                                                
                                                                                
***********************************************************************         
* TABLE OF VALID REPORT ATTRIBUTES OR TYPE AND BIT VALUES                       
***********************************************************************         
         DS    0H                                                               
ATTBTBL  DS    0CL6                                                             
         DC    X'41F0',S(SR@WIDE),X'08',X'00'  WIDE                             
         DC    X'41F0',S(SR@ERROR),X'04',X'00' ERROR                            
         DC    X'41F0',S(DC@SHIT),X'47',X'00'  DUNG                             
         DC    X'41F0',S(SR@PSWD),X'80',X'00'  PASSWORD (=SECURE)               
         DC    X'41F0',S(SR@SEC),X'80',X'00'   SECURE                           
         DC    X'41F0',S(DC@JOBI),X'01',X'00'  JOBINPUT (CONTAINS JCL)          
         DC    X'41F0',S(SR@PENDG),X'01',X'00' PENDING                          
         DC    X'41F0',S(DC@JOBO),X'02',X'00'  JOBOUTPUT                        
         DC    X'41F0',S(SR@SOON),X'02',X'00'  SOON                             
*                                                                               
         DC    X'41F0',S(SR@NOW),X'00',X'01'   NOW                              
         DC    X'41F0',S(SR@UPD),X'00',X'80'   UPDATIVE                         
         DC    X'41F0',S(DC@EXT),X'00',X'02'   EXTENDED                         
         DC    X'41F0',S(SR@OVNIT),X'00',X'04' OVERNIGHT                        
         DC    X'41F0',S(SR@DOWNL),X'00',X'10' DOWNLOADABLE                     
         DC    X'41F0',S(SR@SQL),X'00',X'08'   SQL                              
ATTBTBLX DC    X'00'                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* TABLE OF COMPATIBLE ACTNS & TYPES                                             
***********************************************************************         
COMPTBL  DS    0CL2                                                             
*                                                                               
         DC    X'2100'             DISPLAY...OPTIONAL TYPE                      
         DC    X'21FF'             ALL                                          
         DC    X'21C0'             LIVE                                         
         DC    X'21C8'             LIVE,KEEP                                    
         DC    X'2180'             ACTV                                         
         DC    X'2188'             ACTV,KEEP                                    
         DC    X'2140'             HOLD                                         
         DC    X'2148'             HOLD,KEEP                                    
         DC    X'2130'             DEAD                                         
         DC    X'2138'             DEAD,KEEP                                    
         DC    X'2120'             PRTD                                         
         DC    X'2128'             PRTD,KEEP                                    
         DC    X'2110'             SENT                                         
         DC    X'2118'             SENT,KEEP                                    
         DC    X'2101'             PNTG                                         
         DC    X'2108'             KEEP                                         
*                                                                               
         DC    X'2200'             HOLD......OPTIONAL TYPE                      
         DC    X'2280'             ACTV                                         
         DC    X'2288'             ACTV,KEEP                                    
*                                                                               
         DC    X'2301'             PURGE.....REQUIRED TYPE                      
         DC    X'23FF'             ALL                                          
         DC    X'23C0'             LIVE                                         
         DC    X'23C8'             LIVE,KEEP                                    
         DC    X'2380'             ACTV                                         
         DC    X'2388'             ACTV,KEEP                                    
         DC    X'2340'             HOLD                                         
         DC    X'2348'             HOLD,KEEP                                    
         DC    X'2330'             DEAD                                         
         DC    X'2338'             DEAD,KEEP                                    
         DC    X'2320'             PRTD                                         
         DC    X'2328'             PRTD,KEEP                                    
         DC    X'2310'             SENT                                         
         DC    X'2318'             SENT,KEEP                                    
         DC    X'2308'             KEEP                                         
*                                                                               
         DC    X'2401'             ACTIVATE..REQUIRED TYPE                      
         DC    X'2430'             DEAD                                         
         DC    X'2438'             DEAD,KEEP                                    
         DC    X'2420'             PRTD                                         
         DC    X'2428'             PRTD,KEEP                                    
         DC    X'2410'             SENT                                         
         DC    X'2418'             SENT,KEEP                                    
         DC    X'2440'             HOLD                                         
         DC    X'2448'             HOLD,KEEP                                    
*                                                                               
         DC    X'2501'             KEEP......REQUIRED TYPE                      
         DC    X'25FF'             ALL                                          
         DC    X'25C0'             LIVE                                         
         DC    X'2530'             DEAD                                         
         DC    X'2580'             ACTV                                         
         DC    X'2540'             HOLD                                         
         DC    X'2520'             PRTD                                         
         DC    X'2510'             SENT                                         
*                                                                               
         DC    X'2600'             UNKEEP....OPTIONAL TYPE                      
         DC    X'2608'             KEEP                                         
         DC    X'2688'             ACTV,KEEP                                    
         DC    X'2648'             HOLD,KEEP                                    
         DC    X'2628'             PRTD,KEEP                                    
         DC    X'2618'             SENT,KEEP                                    
*                                                                               
         DC    X'2701'             CLEAR.....REQUIRED TYPE                      
         DC    X'27FF'             ALL                                          
*                                                                               
         DC    X'2800'             SIZE......OPTIONAL TYPE                      
         DC    X'28FF'             ALL                                          
         DC    X'28C0'             LIVE                                         
         DC    X'28C8'             LIVE,KEEP                                    
         DC    X'2880'             ACTV                                         
         DC    X'2888'             ACTV,KEEP                                    
         DC    X'2840'             HOLD                                         
         DC    X'2848'             HOLD,KEEP                                    
         DC    X'2830'             DEAD                                         
         DC    X'2838'             DEAD,KEEP                                    
         DC    X'2820'             PRTD                                         
         DC    X'2828'             PRTD,KEEP                                    
         DC    X'2810'             SENT                                         
         DC    X'2818'             SENT,KEEP                                    
         DC    X'2808'             KEEP                                         
         DC    X'2801'             PNTG                                         
*                                                                               
         DC    X'2901'             RETAIN....REQUIRED TYPE                      
         DC    X'29FF'             ALL                                          
         DC    X'29C0'             LIVE                                         
         DC    X'29C8'             LIVE,KEEP                                    
         DC    X'2980'             ACTV                                         
         DC    X'2988'             ACTV,KEEP                                    
         DC    X'2940'             HOLD                                         
         DC    X'2948'             HOLD,KEEP                                    
         DC    X'2930'             DEAD                                         
         DC    X'2938'             DEAD,KEEP                                    
         DC    X'2920'             PRTD                                         
         DC    X'2928'             PRTD,KEEP                                    
         DC    X'2910'             SENT                                         
         DC    X'2918'             SENT,KEEP                                    
         DC    X'2908'             KEEP                                         
*                                                                               
         DC    X'2A00'             PRINTED...OPTIONAL TYPE                      
         DC    X'2A80'             ACTV                                         
         DC    X'2A88'             ACTV,KEEP                                    
*                                                                               
         DC    X'2B01'             UNERROR...REQUIRED TYPE                      
         DC    X'2BFF'             ALL                                          
         DC    X'2BC0'             LIVE                                         
         DC    X'2BC8'             LIVE,KEEP                                    
         DC    X'2B80'             ACTV                                         
         DC    X'2B88'             ACTV,KEEP                                    
         DC    X'2B40'             HOLD                                         
         DC    X'2B48'             HOLD,KEEP                                    
         DC    X'2B30'             DEAD                                         
         DC    X'2B38'             DEAD,KEEP                                    
         DC    X'2B20'             PRTD                                         
         DC    X'2B28'             PRTD,KEEP                                    
         DC    X'2B10'             SENT                                         
         DC    X'2B18'             SENT,KEEP                                    
         DC    X'2B08'             KEEP                                         
*                                                                               
         DC    X'2C01'             VISIBLE...REQUIRED TYPE                      
         DC    X'2C02'             INVIS                                        
*                                                                               
         DC    X'2D01'             INVISABL..REQUIRED TYPE                      
         DC    X'2D80'             ACTV                                         
         DC    X'2D40'             HOLD                                         
*                                                                               
         DC    X'2E01'             UNSECURE..REQUIRED TYPE                      
         DC    X'2EC0'             LIVE                                         
         DC    X'2E80'             ACTV                                         
         DC    X'2E40'             HOLD                                         
         DC    X'2E30'             DEAD                                         
         DC    X'2E20'             PRTD                                         
         DC    X'2E10'             SENT                                         
         DC    X'2E08'             KEEP                                         
*                                                                               
         DC    X'4100'             ARCA......OPTIONAL TYPE                      
         DC    X'4180'             ACTV                                         
         DC    X'4140'             HOLD                                         
*                                                                               
         DC    X'4200'             UNARCA....OPTIONAL TYPE                      
         DC    X'4280'             ACTV                                         
         DC    X'4240'             HOLD                                         
*                                                                               
         DC    X'4300'             ARCD......OPTIONAL TYPE                      
         DC    X'4380'             ACTV                                         
         DC    X'4340'             HOLD                                         
*                                                                               
         DC    X'4400'             UNARCD....OPTIONAL TYPE                      
         DC    X'4480'             ACTV                                         
         DC    X'4440'             HOLD                                         
*                                                                               
         DC    X'4500'             ARCE......OPTIONAL TYPE                      
         DC    X'4580'             ACTV                                         
         DC    X'4540'             HOLD                                         
*                                                                               
         DC    X'4600'             UNARCE....OPTIONAL TYPE                      
         DC    X'4680'             ACTV                                         
         DC    X'4640'             HOLD                                         
*                                                                               
         DC    X'4700'             BKUP......OPTIONAL TYPE                      
         DC    X'4780'             ACTV                                         
         DC    X'4740'             HOLD                                         
*                                                                               
         DC    X'4800'             UNBKUP....OPTIONAL TYPE                      
         DC    X'4880'             ACTV                                         
         DC    X'4840'             HOLD                                         
*                                                                               
         DC    X'49FF'             NCD                                          
         DC    X'4AFF'             OCD                                          
*                                                                               
COMPTBLX DC    X'FFFF'                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* DICTIONARY DCDDL CONSTANTS                                                    
***********************************************************************         
       ++INCLUDE SRPQUDD                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* PQNLD DSECT                                                                   
***********************************************************************         
PQNLD    DSECT                                                                  
         DS    CL8                                                              
PQNSELL  DS    CL4                                                              
PQNLHDR  DS    CL8                                                              
PQNLDATA DS    0CL73                                                            
*                                                                               
PQNLID   DS    CL9                                                              
         DS    CL1                                                              
PQNLCD   DS    CL11                                                             
         DS    CL1                                                              
PQNLCLAS DS    CL1                                                              
PQNLPTY  DS    CL1                                                              
PQNLTYPE DS    CL1                                                              
         DS    CL1                                                              
PQNLNAME DS    CL11                                                             
         DS    CL1                                                              
PQNLPAGE DS    CL5                                                              
         DS    CL1                                                              
PQNLSTAT DS    CL2                                                              
         DS    CL1                                                              
PQNLRET  DS    CL2                                                              
         DS    CL1                                                              
PQNLPRTD DS    CL11                                                             
         DS    CL1                                                              
PQNLFMS  DS    CL11                                                             
                                                                                
***********************************************************************         
* RTED DSECT                                                                    
***********************************************************************         
RTED     DSECT                                                                  
RTEDATA  DS    0CL12                                                            
RTEUSER  DS    XL2                                                              
RTESORT  DS    XL4                                                              
RTEREPNO DS    XL2                                                              
RTESTAT  DS    XL1                                                              
RTECIADR DS    XL2                                                              
RTETYP1  DS    XL1                                                              
                                                                                
***********************************************************************         
* SIZED DSECT                                                                   
***********************************************************************         
SIZED    DSECT                                                                  
SZSHDR   DS    CL8                                                              
SZACT    DS    CL4                                                              
SZHDR    DS    CL8                                                              
SZDATA   DS    0CL73                                                            
SZQUE    DS    CL7                                                              
         DS    CL1                                                              
SZTOT1   DS    CL6                                                              
         DS    CL1                                                              
SZAVA1   DS    CL6                                                              
         DS    CL1                                                              
SZPRT1   DS    CL6                                                              
         DS    CL1                                                              
SZACT1   DS    CL6                                                              
         DS    CL1                                                              
SZIND1   DS    CL6                                                              
         DS    CL2                                                              
SZTOT2   DS    CL6                                                              
         DS    CL1                                                              
SZAVA2   DS    CL6                                                              
         DS    CL1                                                              
SZPRT2   DS    CL6                                                              
         DS    CL1                                                              
SZACT2   DS    CL6                                                              
         DS    CL2                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
       ++INCLUDE SRPQUWK                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* SCREENS                                                                       
***********************************************************************         
SRPQUFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRPQUFFD                                                       
         ORG   SRVEX2H                                                          
       ++INCLUDE SRPQUFDD                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
*FASYSFAC                                                                       
*FASRS                                                                          
*DDCOMFACS                                                                      
*DDFLDHDR                                                                       
*CTGENFILE                                                                      
*SRERREQUS                                                                      
*IHAASCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASRS                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SRERREQUS                                                      
       ++INCLUDE SEACSFILE                                                      
         IHAASCB LIST=YES                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SRPQU02   11/11/20'                                      
         END                                                                    
