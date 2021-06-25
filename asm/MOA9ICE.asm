*          DATA SET MOA9ICE    AT LEVEL 007 AS OF 04/12/18                      
*PHASE MOA9ICEA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
         TITLE 'MONEYFLOW - CONTROL JOB TO RUN AT END OF MONTH ONLY'            
********************************************************************            
* THIS MODULE CHECKS TO SEE IF IT IS THE END OF MONTH AND IF IT    *            
* IS IT THEN INVOLKES ICETOOL.                                     *            
*                                                                  *            
* DYNAMICALLY ALLOCATE DATASETS WITH THE AGENCY LABEL, ALPHA, DATE *            
*     IN THE DSN.                                                  *            
*                                                                  *            
* THEN IT CALLS ICETOOL TO CREATE THE OUTPUT DATASETS              *            
*                                                                  *            
* IF RUN=PROD (Default) then the HLQ is DDSPROD.MOFLOW.*           *            
*                                                                  *            
* IF RUN=TEST   then the default HLQ is DDSTEST.FTP.*              *            
*                                                                  *            
********************************************************************            
                                                                                
MOA9ICE  CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         PRINT NOGEN                                                            
         NBASE 0,**MOA9**,=V(REGSAVE)                                           
         XR    R8,R8               RETURN CODE VALUE                            
*                                                                               
MOICE    GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    MOICE100                                                         
         CLC   =C'XX',CARD                                                      
         BE    MOICE100                                                         
         CLI   CARD,C'*'           Comment                                      
         BE    MOICE                                                            
         CLC   =C'DSPACE=',CARD                                                 
         BE    MOICE090                                                         
         CLC   =C'RUN=TEST',CARD                                                
         BE    MOICE010                                                         
         CLC   =C'RUN=PROD',CARD                                                
         BE    MOICE010                                                         
         CLC   =C'DDSIO=',CARD                                                  
         BE    MOICE020                                                         
         CLC   =C'TODAY=',CARD        TODAY=YYMMDD                              
         BE    MOICE030                                                         
         CLC   =C'TESTCOPY=',CARD     Default is yes                            
         BE    MOICE032                                                         
         CLC   =C'OUTFILE=',CARD                                                
         BE    MOICE040                                                         
         CLC   =C'NET=',CARD                                                    
         BE    MOICE050                                                         
         CLC   =C'GRS=',CARD                                                    
         BE    MOICE052                                                         
         CLC   =C'ALPHA=',CARD                                                  
         BE    MOICE060                                                         
         CLC   =C'AGYID=',CARD     FROM ACCESS RECROD                           
         BE    MOICE062                                                         
         CLC   =C'ID=',CARD        ID=                                          
         BE    MOICE070                                                         
         CLC   =C'ORIGIN=',CARD    ORIGIN=                                      
         BE    MOICE072                                                         
         CLC   =C'CYL=',CARD       CYL=325 default                              
         BE    MOICE022                                                         
         DC    H'0'                                                             
*                                                                               
MOICE010 MVC   RUNTYPE,CARD+4                                                   
         B     MOICE                                                            
*                                                                               
MOICE020 L     RE,=V(DDSIO)                                                     
         MVC   0(8,RE),CARD+6                                                   
         B     MOICE                                                            
*                                                                               
MOICE022 LA    RF,CARD+10                                                       
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         SHI   RF,1                                                             
         B     *-12                                                             
*                                                                               
         LA    RE,CARD+4                                                        
         SR    RF,RE                  RE=Length - 1                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)                                                      
         CVB   RF,DUB                                                           
         STCM  RF,7,ALLOCS            Save number of cylinders                  
         B     MOICE                                                            
                                                                                
*                                                                               
MOICE030 CLC   =C'EVERYDAY',CARD+6    Run everyday report                       
         BNE   MOICE031                                                         
         MVI   EVERYDAY,YES                                                     
         B     MOICE                                                            
*                                                                               
MOICE031 MVC   TODAY,CARD+6                                                     
         B     MOICE                                                            
*                                                                               
MOICE032 MVC   TESTCOPY,CARD+9     Copy production to DDSTEST.FTP.*             
         B     MOICE                                                            
*                                                                               
MOICE040 MVC   USERFILE,CARD+8                                                  
         MVI   OVERRIDE,YES        File over-ride used                          
         LA    RE,USERFILE                                                      
         XR    R1,R1                                                            
MOICE042 CLI   0(RE),C' '                                                       
         BH    MOICE044                                                         
         CLC   =C'DYYMMDD',0(RE)                                                
         BNE   MOICE043                                                         
         AHI   RE,1                BUMP PAST "D" FOR DATE                       
         ST    RE,@OUTDATE         SAVE LOCATION IF SOFT DATE                   
         AHI   RE,6                SKIP THE REST TOO                            
         AHI   R1,7                MAKE SURE TO INCREASE THE LENGTH             
*                                                                               
MOICE043 AHI   RE,1                                                             
         AHI   R1,1                                                             
         B     MOICE042                                                         
*                                                                               
MOICE044 CHI   R1,L'USERFILE                                                    
         JH    *+2                 TOO LONG                                     
         STC   R1,OUTLEN           LENGTH OF DSN                                
         B     MOICE                                                            
*                                                                               
MOICE050 CLI   CARD+4,YES          NET?                                         
         BE    MOICE               Then we are good                             
         MVC   OUT$TYPE,=C'GRS'    Must be gross then  DDSPROD.*                
         MVC   OU2$TYPE,=C'GRS'                        DDSTEST.*                
         B     MOICE                                                            
*                                                                               
MOICE052 CLI   CARD+4,YES          GRS?                                         
         BNE   MOICE               Then we are good                             
         MVC   OUT$TYPE,=C'GRS'    Must be gross then  DDSPROD.*                
         MVC   OU2$TYPE,=C'GRS'                        DDSTEST.*                
         B     MOICE                                                            
*                                                                               
MOICE060 MVC   OUTALFA,CARD+6      Alpha, DDSPROD.*                             
         MVC   OU2ALFA,CARD+6             DDSTEST.*                             
         CLI   OUTALFA,C' '        MAKE SURE BOTH FILLED IN                     
         JNH   *+2                                                              
         CLI   OUTALFA+1,C' '                                                   
         JNH   *+2                                                              
         B     MOICE                                                            
*                                                                               
MOICE062 MVC   OUTAGYID,CARD+6     DDSPROD.                                     
*        MVC   OU2AGYID,CARD+6     DDSTEST.                                     
         LA    R1,L'OUTAGYID                                                    
         LA    RE,OUTAGYID                                                      
MOICE063 CLI   0(RE),C' '          ARE ALL 4 CHAR ARE FILLED IN?                
         JNH   *+2                 NO                                           
         AHI   RE,1                                                             
         BCT   R1,MOICE063                                                      
         B     MOICE                                                            
*                                                                               
MOICE070 MVC   MCUSERID,CARD+3                                                  
         B     MOICE076                                                         
*                                                                               
MOICE072 LA    RE,CARD+7           Convert ORIGIN id to binary value            
         XR    RF,RF                                                            
MOICE073 CLI   0(RE),C' '                                                       
         BH    MOICE074                                                         
         CLI   0(RE),C'0'                                                       
         JL    *+2                                                              
         CLI   0(RE),C'9'                                                       
         JH    *+2                                                              
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         B     MOICE073                                                         
*                                                                               
MOICE074 LTR   RF,RF                                                            
         JZ    *+2                                                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,CARD+7(0)       DUB CONTAINS PACKED VALUE                    
         CVB   RF,DUB                                                           
         STH   RF,MCORIGIN                                                      
*                                                                               
MOICE076 LA    R2,CTLIO            R2=A(ID RECORD)                              
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',CTFILES,(R2)             
*                                                                               
         LA    R2,CTLIO            R2=A(ID RECORD)                              
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         OC    CTIKID,MCUSERID     USE USER-ID CODE IF GIVEN                    
         BNZ   *+14                                                             
         OC    CTIKNUM,MCORIGIN    OR ORIGIN ID IF NOT                          
         JZ    *+2                                                              
         MVC   SVCTKEY,CTIKEY                                                   
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'CTFILE',(R2),(R2)                 
         JNE   *+2                                                              
*                                                                               
         LA    R2,CTIDATA          Point to elements                            
         USING CTAGYD,R2                                                        
MOICE078 CLI   CTAGYEL,0                                                        
         JE    *+2                                                              
         CLI   CTAGYEL,CTAGYELQ    X'06' AGECNY ALPHA                           
         BE    MOICE080                                                         
         LLC   RF,CTAGYLEN                                                      
         AR    R2,RF                                                            
         B     MOICE078                                                         
*                                                                               
MOICE080 MVC   OUTALFA,CTAGYID     Agency alpha  DDSPROD.*                      
         MVC   OU2ALFA,CTAGYID     Agency alpha  DDSTEST.*                      
*                                                                               
         LA    R2,CTLIO            R2=A(ID RECORD)                              
         USING CT5REC,R2                                                        
         XC    CT5KEY,CT5KEY       BUILD KEY OF ACCESS RECORD                   
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,OUTALFA                                                 
         MVC   SVCTKEY,CT5KEY                                                   
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'CTFILE',(R2),(R2)                 
         JNE   *+2                                                              
*                                                                               
         LA    R2,CT5DATA          Point to elements                            
         USING CTAGCD,R2                                                        
MOICE086 CLI   CTAGCEL,0                                                        
         JE    *+2                                                              
         CLI   CTAGCEL,CTAGCELQ    X'1B' AGECNY ALPHA                           
         BE    MOICE088                                                         
         LLC   RF,CTAGCLEN                                                      
         AR    R2,RF                                                            
         B     MOICE086                                                         
*                                                                               
MOICE088 MVC   OUTAGYID,CTAGCCOD   Agency id code for DDSPROD.                  
*        MVC   OU2AGYID,CTAGCCOD   Agency id code for DDSTEST.                  
         B     MOICE                                                            
                                                                                
                                                                                
         USING SSBD,RE                                                          
MOICE090 L     RE,=A(SSB)                                                       
         MVC   SSODSPAC,CARD+7                                                  
         USING CTAGCD,R2                                                        
         B     MOICE                                                            
         DROP  RE                                                               
                                                                                
*                                                                               
MOICE100 CLC   TODAY,SPACES                                                     
         BH    MOICE110                                                         
         GOTO1 =V(DATCON),DMCB,(5,0),(X'20',TODAY)                              
*                                                                               
* GET LAST DAY OF INPUT                                                         
*                                                                               
         GOTO1 =V(DATCON),DMCB,(X'30',TODAY),(X'20',EOFMONTH),(1,0)             
         CLI   EVERYDAY,YES                                                     
         BE    MOICE110            Yes                                          
         CLC   TODAY,EOFMONTH                                                   
         BNE   MOICEXIT                                                         
*                                                                               
MOICE110 CLI   OVERRIDE,YES                                                     
         BE    MOICE150                                                         
         LA    RE,OUTDATE                                                       
         ST    RE,@OUTDATE                                                      
         CLC   OUTAGYID,=C'????'   Agency id set?                               
         JE    *+2                                                              
         CLC   OUTALFA,=C'????'    Alpha set?                                   
         JE    *+2                                                              
         EJECT                                                                  
*==================================================================             
* NOW DYNAMICALLY ALLOCATE OUTPUT FILE WITH DATES                               
*                                                                               
* DSN LOOKS LIKE    DDSPROD.MOFLOW.aaaa.A9aa.DYYMMDD.$$$                        
* IF RUN=TEST then  DDSTEST.FTP.A9aa.DYYMMDD.$$$                                
*                                                                               
* USER CAN OVER-RIDE WITH OUTFILE=                                              
*   1) User can imbed DYYMMDD into the file                                     
*   2) ".aaaa." is the access id from the access record unless                  
*      you over-ride it with AGYID=                                             
*   3) "A9aa" where aa is the alpha characters. Can be                          
*      over-ridden with ALPHA=                                                  
*   4) $$$ is NET (Default) or GRS (gross dollars)                              
*==================================================================             
MOICE150 LT    RE,@OUTDATE                                                      
         BZ    MOICE160                                                         
         BRAS  R9,LDOM             LDOM - Last Day Of Month?                    
         BE    MOICE152            Yes, so keep letter as C'D'                  
         SHI   RE,1                                                             
         MVI   0(RE),C'E'          Distiguish file if not LDOM                  
         AHI   RE,1                                                             
         LA    RF,OU2DATE-1                                                     
         MVI   0(RF),C'E'          Test file                                    
*                                                                               
MOICE152 MVC   0(L'OUTDATE,RE),TODAY                                            
         MVC   OU2DATE,TODAY       This one stays as is DDSTEST.*               
*                                                                               
MOICE160 CLI   OVERRIDE,YES                                                     
         BNE   MOICE162                                                         
         MVC   OUTFILE,USERFILE                                                 
         B     MOICE180                                                         
         EJECT ,                                                                
*********************************************************************           
* Last day of the month, no braner form                             *           
* TODAY is YYMMDD format in text                                                
*********************************************************************           
LDOM     CLC   =C'0131',TODAY+2     Jan31                                       
         BER   R9                                                               
         CLC   =C'0228',TODAY+2     Feb28                                       
         BER   R9                                                               
         CLC   =C'0229',TODAY+2     Feb29                                       
         BER   R9                                                               
         CLC   =C'0331',TODAY+2     Mar31                                       
         BER   R9                                                               
         CLC   =C'0430',TODAY+2     Apr30                                       
         BER   R9                                                               
         CLC   =C'0531',TODAY+2     May31                                       
         BER   R9                                                               
         CLC   =C'0630',TODAY+2     Jun30                                       
         BER   R9                                                               
         CLC   =C'0731',TODAY+2     Jul31                                       
         BER   R9                                                               
         CLC   =C'0831',TODAY+2     Aug31                                       
         BER   R9                                                               
         CLC   =C'0930',TODAY+2     Sep30                                       
         BER   R9                                                               
         CLC   =C'1031',TODAY+2     Oct31                                       
         BER   R9                                                               
         CLC   =C'1130',TODAY+2     Nov30                                       
         BER   R9                                                               
         CLC   =C'1231',TODAY+2     Dec31                                       
         BER   R9                                                               
         BR    R9                  No, not last day of month                    
*********************************************************************           
* Check to make sure everything matches up                                      
*   First record must be header. Check date and dollar type                     
*********************************************************************           
         USING A9HDR,R3                                                         
MOICE162 LARL  R2,INSPTFIL         Spot                                         
         OPEN  ((R2),(INPUT))                                                   
         LA    R3,CTLIO                                                         
         GET   (2),(3)                                                          
         CLC   A9ID,=C'SA9HDR'     Must be header record for spot               
         JNE   *+2                                                              
         GOTO1 =V(DATCON),DMCB,(0,A9RUNDTE),(X'20',DUB)                         
         CLC   TODAY,DUB           Today set value must match file              
         JNE   *+2                                                              
         CLI   OVERRIDE,YES                                                     
         BE    SPTAPEX                                                          
         CLC   A9$TYPE,OUT$TYPE    Must be correct dollar type N or G           
         JNE   *+2                                                              
         GET   (2),(3)             Get next record                              
*                                                                               
         USING A9RECD,R3                                                        
         CLC   A9AGY,OUTALFA       Alpha must match                             
         JNE   *+2                                                              
SPTAPEX  CLOSE ((R2))                                                           
*                                                                               
         USING A9HDR,R3                                                         
         LARL  R2,INNETFIL         Net                                          
         OPEN  ((R2),(INPUT))                                                   
         LA    R3,CTLIO                                                         
         GET   (2),(3)                                                          
         CLC   A9ID,=C'NA9HDR'     Must be header record for spot               
         JNE   *+2                                                              
         GOTO1 =V(DATCON),DMCB,(0,A9RUNDTE),(X'20',DUB)                         
         CLC   TODAY,DUB           Today set value must match file              
         JNE   *+2                                                              
         CLI   OVERRIDE,YES                                                     
         BE    NETAPEX                                                          
         CLC   A9$TYPE,OUT$TYPE    Must be correct dollar type N or G           
         JNE   *+2                                                              
         GET   (2),(3)             Get next record                              
*                                                                               
         USING A9RECD,R3                                                        
         CLC   A9AGY,OUTALFA       Alpha must match                             
         JNE   *+2                                                              
NETAPEX  CLOSE ((R2))                                                           
*                                                                               
         USING A9HDR,R3                                                         
         LARL  R2,INPRTFIL         Print                                        
         OPEN  ((R2),(INPUT))                                                   
         LA    R3,CTLIO                                                         
         GET   (2),(3)                                                          
         CLC   A9ID,=C'PA9HDR'     Must be header record for spot               
         JNE   *+2                                                              
         GOTO1 =V(DATCON),DMCB,(0,A9RUNDTE),(X'20',DUB)                         
         CLC   TODAY,DUB           Today set value must match file              
         JNE   *+2                                                              
         CLI   OVERRIDE,YES                                                     
         BE    PPTAPEX                                                          
         CLC   A9$TYPE,OUT$TYPE    Must be correct dollar type N or G           
         JNE   *+2                                                              
         GET   (2),(3)             Get next record                              
*                                                                               
         USING A9RECD,R3                                                        
         CLC   A9AGY,OUTALFA       Alpha must match                             
         JNE   *+2                                                              
PPTAPEX  CLOSE ((R2))                                                           
         EJECT ,                                                                
*********************************************************************           
* Check to see if we need to change HLQ.                                        
* allocate files                                                                
*********************************************************************           
MOICE180 CLI   OVERRIDE,YES                                                     
         BE    MOICE200                                                         
         CLI   RUNTYPE,C'T'        Test?                                        
         BE    MOICE190            Yes                                          
         CLI   TESTCOPY,NO         Production, copy to DDSTEST?                 
         BE    MOICE200            No, so DUMMY out OUTFILE2                    
         B     MOICE210            Continue as normal                           
*                                                                               
MOICE190 MVC   OUTFILE,OUTFILE2                                                 
*********************************************************************           
* Do we need to make one of the files DD DUMMY ?                                
* If card OUTFILE= OR RUN=TEST then  OUTFILE2 DUMMY                             
* If neither then create both DDSPROD.MOFLOW.* and DDSTEST.FTP.*                
*********************************************************************           
MOICE200 LA    R4,DDNAME2          OUTFILE2 - DDSTEST.FTP.*                     
         L     RF,=V(DYNALLOC)     TRY ALLOCATING NEW DATASET                   
         GOTO1 (RF),DMCB,          OUTFILE2 is DD DUMMY                +        
               (C'N',(R4)),                                            +        
               0,                                                      +        
               0,                                                      +        
               0,                                                      +        
               0                                                                
         OC    12(4,R1),12(R1)     ANY DYNALLOC ERROR?                          
         JNZ   *+2                 YOU BET YA                                   
                                                                                
*                                  OUTFILE                                      
MOICE210 LA    R4,DDNAME           DDSPROD.MOFLOW.*                             
         L     RF,=V(DYNALLOC)     TRY ALLOCATING NEW DATASET                   
         GOTO1 (RF),DMCB,                                              +        
               (X'80',(R4)),                                           +        
               (X'47',ALLOCS),                                         +        
               (X'40',OUTFILE),                                        +        
               0,                                                      +        
               0                                                                
         OC    12(4,R1),12(R1)     ANY DYNALLOC ERROR?                          
         JNZ   *+2                 YOU BET YA                                   
*                                                                               
         CLI   OVERRIDE,YES        Yes                                          
         BE    MOICE300            Done                                         
         CLI   RUNTYPE,C'T'        Yes                                          
         BE    MOICE300            Done                                         
         CLI   TESTCOPY,NO         OUTFILE2 is DUMMY already                    
         BE    MOICE300                                                         
                                                                                
*                                  OUTFILE2                                     
MOICD220 LA    R4,DDNAME2          DDSTEST.FTP.*                                
         GOTO1 (RF),DMCB,          Allocate DDSTEST.FTP.* file too     +        
               (X'80',(R4)),                                           +        
               (X'47',ALLOCS),                                         +        
               (X'40',OUTFILE2),                                       +        
               0,                                                      +        
               0                                                                
         OC    12(4,R1),12(R1)     ANY DYNALLOC ERROR?                          
         JNZ   *+2                 YOU BET YA                                   
         EJECT ,                                                                
*********************************************************************           
* Now that we have a file, run icetool to create output file                    
*********************************************************************           
MOICE300 SR    R1,R1               USE TOOLIN INTERFACE                         
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF                                                            
         JNZ   *+2                 At least one                                 
*                                                                               
MOICEXIT XBASE RC=(R8)                                                          
         EJECT                                                                  
********************************************************************            
* constants and equates                                                         
********************************************************************            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
K        EQU   1024                                                             
*                                                                               
@OUTDATE DC    A(0)                                                             
DDNAME   DC    CL8'OUTFILE'                                                     
USERFILE DC    CL44' '             User over-ride file                          
OUTFILE  DC    CL44' '             User over-ride file                          
         ORG   OUTFILE                                                          
OUTHLQ   DC    C'DDSPROD.MOFLOW.'                                               
OUTAGYID DC    C'????'                                                          
OUTA9PGM DC    C'.A9'                                                           
OUTALFA  DC    C'??'                                                            
         DC    C'.D'                                                            
OUTDATE  DC    C'YYMMDD'                                                        
         DC    C'.'                                                             
OUT$TYPE DC    C'NET'              DEFAULT IS NET                               
         ORG                                                                    
*********************************************************************           
* DDSTEST file for FQA, TST, UAT                                                
*********************************************************************           
DDNAME2  DC    CL8'OUTTEST'                                                     
OUTFILE2 DC    CL44' '                                                          
         ORG   OUTFILE2                                                         
OU2HLQ   DC    C'DDSTEST.FTP.A9'                                                
OU2ALFA  DC    C'??'                                                            
         DC    C'.D'                                                            
OU2DATE  DC    C'YYMMDD'                                                        
         DC    C'.'                                                             
OU2$TYPE DC    C'NET'              DEFAULT IS NET                               
         ORG                                                                    
OUTLEN   DS    AL1                 LENGTH OF DSN                                
ALLOCS   DC    AL3(325),AL3(120)   PRIMARY CYL/2ND CYL                          
*                                                                               
EVERYDAY DC    AL1(NO)                                                          
OVERRIDE DC    AL1(NO)                                                          
TESTCOPY DS    AL1(YES)                                                         
SPACES   DC    CL80' '                                                          
CTFILES  DC    C'NCTFILE X'                                                     
         EJECT                                                                  
*********************************************************************           
* UTL AND SSB BLOCKS                                                            
*********************************************************************           
         DC    CL16'*UTL**UTL**UTL**'                                           
UTL      DC    (TUTLXALN)X'00'                                                  
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    X'0A'               SYSTEM 10 (CONTROL)                          
         ORG                                                                    
UTLL     EQU   *-UTL                                                            
                                                                                
         DS    0D                                                               
         DC    CL16'*SSB**SSB**SSB**'                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSB+(SSOSTAT2-SSBD)                                              
         DC    AL1(SSOSNRCV)       SET RECOVERY OFF                             
         ORG   SSB+(SSOFLAG3-SSBD)                                              
         DC    AL1(SSO3XUTL)       OFFLINE EXTENDED UTL                         
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         EJECT                                                                  
*********************************************************************           
* DCB for input tapes, used for validity checking                               
*********************************************************************           
INSPTFIL DCB   DDNAME=INSPTFIL,DSORG=PS,RECFM=FB,MACRF=GM,             +        
               EODAD=SPTAPEX                                                    
                                                                                
INPRTFIL DCB   DDNAME=INPRTFIL,DSORG=PS,RECFM=FB,MACRF=GM,             +        
               EODAD=PPTAPEX                                                    
                                                                                
INNETFIL DCB   DDNAME=INNETFIL,DSORG=PS,RECFM=FB,MACRF=GM,             +        
               EODAD=NETAPEX                                                    
*********************************************************************           
* GENERAL VARIABLES                                                             
*********************************************************************           
DMCB     DS    6A                                                               
         ORG   *-24                                                             
DM1      DS    A                                                                
DM2      DS    A                                                                
DM3      DS    A                                                                
DM4      DS    A                                                                
DM5      DS    A                                                                
DM6      DS    A                                                                
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
CARD     DS    CL80                                                             
RUNTYPE  DS    CL4                                                              
WORK     DS    XL32                                                             
TODAY    DS    CL6                                                              
EOFMONTH DS    CL6                                                              
SVCTKEY  DS    XL25                                                             
MCUSERID DS    XL10'00'                                                         
MCORIGIN DC    H'0'                                                             
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
CTLIO    DS    XL2048                                                           
                                                                                
         EJECT                                                                  
       ++INCLUDE MOA9SYMD                                                       
                                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
*      ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         ORG   SSBD                                                             
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007MOA9ICE   04/12/18'                                      
         END                                                                    
