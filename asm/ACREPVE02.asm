*          DATA SET ACREPVE02  AT LEVEL 002 AS OF 04/22/20                      
*PHASE ACVE02A                                                                  
         TITLE 'VENDOR LISTING REPORT'                                          
*                                                                               
***********************************************************************         
* ID   LVL DATE    JIRA         DESCRIPTION                           *         
* **** *** ******* ************ ***************************************         
* JSAY 001 28JUN19 <SPEC-32193> INITIAL VERSION                       *         
* JSAY 002 20MAR20 <SPEC-44450> FIX FOR CITY,STATE AND ZIP            *         
*                                                                     *         
***********************************************************************         
***********************************************************************         
*REQUEST OPTIONS:-                                                    *         
*                                                                     *         
*QLEDGER      LEDGER                                                  *         
*QSTART       START DATE (MMDDYY)                                     *         
*QEND         END DATE   (MMDDYY)                                     *         
*QOPT1        IF YES, DO NOT SEND MQ MESSAGE                          *         
***********************************************************************         
*                                                                               
ACVE02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACVE**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING WRKD,RC             RC=A(TEMP W/S)                               
         ST    R5,PRELOC                                                        
*                                                                               
         CLI   MODE,RUNFRST                                                     
         JE    RUNF                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         JE    REQF                                                             
*                                                                               
         CLI   MODE,LEDGFRST                                                    
         JE    LDGF                                                             
*                                                                               
         CLI   MODE,PROCACC                                                     
         JE    PACC                                                             
*                                                                               
         CLI   MODE,REQLAST                                                     
         JE    REQL                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         JE    RUNL                                                             
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
RUNF     DS    0H                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
*                                                                               
         MVC   VMQRPT,MCVMQRPT     MQ INTERFACE                                 
         MVC   DUB,=CL8'ADFORM'                                                 
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         JZ    *+2                                                              
*                                                                               
         MVC   ADFORM,4(R1)        STORE THE ADDRESS OF ADFORM ROUTINE          
         J     EXIT                                                             
*                                                                               
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
REQF     DS    0H                                                               
         ZAP   TOTRECS,=P'0'                                                    
         MVI   HDRSET,C'N'         OUTPUT HEADER NOT SET                        
*                                                                               
         GOTO1 =A(FILTER),DMCB,(RC) SEE IF THIS COMPANY QUALIFIES               
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         TM    TAPEOUT+48,X'10'    TEST WAS OKAY?                               
         JZ    *+2                 NO DIE HERE                                  
*                                                                               
REQF10   XC    SVSTDT,SVSTDT                                                    
         CLC   QSTART,SPACES                                                    
         BNH   REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,SVSTDT)                                
*                                                                               
REQF20   MVC   SVENDDT,=X'FFFFFF'                                               
         CLC   QEND,SPACES                                                      
         BNH   REQFX                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,SVENDDT)                                 
*                                                                               
REQFX    B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* LEDGER FIRST                                                        *         
***********************************************************************         
LDGF     DS    0H                                                               
         L     R2,ADLEDGER                                                      
         MVC   SAVECMP(L'SAVECMP*3),SPACES                                      
         MVC   SAVECMP(1),0(R2)                                                 
         MVC   SAVEUNT(1),1(R2)                                                 
         MVC   SAVELDG(1),2(R2)                                                 
         MVC   SVUNTLDG,1(R2)                                                   
*                                                                               
         LA    R2,PAYTAB                                                        
LDGF02   CLI   0(R2),X'FF'                                                      
         JE    LDGF05                                                           
         CLC   SVUNTLDG,0(R2)                                                   
         JE    LDGF06                                                           
*                                                                               
LDGF04   LA    R2,PAYTABLN(R2)                                                  
         J     LDGF02                                                           
*                                                                               
LDGF05   MVI   MODE,LEDGLAST                                                    
         MVI   FCRDTRNS,NOQ                                                     
         J     EXIT                                                             
*                                                                               
LDGF06   L     R2,ADCMPNAM         EXTRACT COMPANY NAME                         
         LA    R5,SAVECMP+2                                                     
         JAS   RE,ACEXNAM                                                       
         MVC   P(L'SAVECMP),SAVECMP                                             
*                                                                               
         L     R2,ADUNTNAM         EXTRACT UNIT NAME                            
         LA    R5,SAVEUNT+2                                                     
         JAS   RE,ACEXNAM                                                       
         MVC   P(L'SAVEUNT),SAVEUNT                                             
*                                                                               
         L     R2,ADLDGNAM         EXTRACT LEDGER NAME                          
         LA    R5,SAVELDG+2                                                     
         JAS   RE,ACEXNAM                                                       
         MVC   P(L'SAVELDG),SAVELDG                                             
*                                                                               
         MVC   SVSYSMED,SPACES     INITIALZE WITH SPACES                        
         JAS   RE,GETSYS           GET THE SYSTEM                               
         J     EXIT                                                             
*                                                                               
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
*                                                                               
         USING ACTRECD,R2                                                       
         USING VNDRECD,R4                                                       
PACC     DS    0H                                                               
         MVC   SVACTMED,SPACES     INITIALIZE THE TEMP MEDIA                    
*                                                                               
         BRAS  RE,CLROUT                                                        
*                                                                               
         L     R2,ADACC            POINT TO THE ACCOUNT RECORD                  
         LA    R3,PAYTAB                                                        
PACC1    CLI   0(R3),X'FF'                                                      
         JE    PACC3                                                            
         CLC   1(2,R2),0(R3)                                                    
         JE    PACC4                                                            
*                                                                               
PACC2    LA    R3,PAYTABLN(R3)                                                  
         J     PACC1                                                            
*                                                                               
PACC3    MVI   MODE,ACCLAST                                                     
         MVI   FCRDTRNS,NOQ                                                     
         J     EXIT                                                             
*                                                                               
PACC4    LA    R4,VNDREC           BUFFER RECORD                                
         MVC   VNDID,ACTKACT       VENDOR ID                                    
         MVC   VNDSYS,SVSYS        PASS THE SYSTEM                              
*                                                                               
         MVC   SVACTMED,ACTKACT    FIRST CHAR OF ACCOUNT                        
         JAS   RE,GETMED           GET THE MEDIA                                
         MVC   VNDMED,SVACTMED     SET THE MEDIA FROM ACCOUNT                   
*                                                                               
         AH    R2,DATADISP         POINT TO FIRST ELEMENT                       
PACC00   CLI   0(R2),0             END OF RECORD                                
         JE    PACC90                                                           
*                                                                               
         LLC   R1,1(R2)            GET THE ELEMENT LENGTH                       
         LTR   R1,R1               IS ELEMENT LENGTH > 0?                       
         JZ    PACC10              NO - READ NEXT                               
*                                                                               
         CLI   0(R2),NAMELQ        VENDOR NAME                                  
         JE    PACC20                                                           
*                                                                               
         CLI   0(R2),ADRELQ        ADDRESS                                      
         JE    PACC30                                                           
*                                                                               
         CLI   0(R2),OTHELQ        OTHERS ELEMENT                               
         JE    PACC40                                                           
*                                                                               
         CLI   0(R2),RSTELQ        STATUS - LAST DATE                           
         JE    PACC50                                                           
*                                                                               
         CLI   0(R2),OMEELQ        ONLINE MEMO ELEMENT                          
         JE    PACC60                                                           
*                                                                               
         CLI   0(R2),FFTELQ        FREE FORM- CONTACT,PHONE,EMAIL               
         JE    PACC70                                                           
*                                                                               
         CLI   0(R2),RACELQ        ACTIVITY - ADD DATE                          
         JE    PACC80                                                           
*                                                                               
PACC10   LLC   R1,1(R2)            BUMP TO NEXT  ELEMENT                        
         AR    R2,R1                                                            
         J     PACC00                                                           
*                                                                               
         USING NAMELD,R2                                                        
PACC20   DS    0H                  VENDOR NAME                                  
         SHI   R1,NAMLN1Q+1                                                     
         EXMVC R1,VNDCMPNM,NAMEREC                                              
         J     PACC10                                                           
*                                                                               
PACC30   DS    0H                  ADDRESS                                      
         JAS   RE,GETADR           GET THE ADDRESS                              
*                                                                               
         MVC   VNDADLN1,SVADR1                                                  
         MVC   VNDADLN2,SVADR2                                                  
         MVC   VNDADCTY,SVCITY                                                  
         MVC   VNDADST,SVSTATE                                                  
         MVC   VNDADZIP,SVZIP                                                   
         MVC   VNDCTRY,SVCTRY                                                   
         J     PACC10                                                           
*                                                                               
         USING OTHELD,R2                                                        
PACC40   DS    0H                  MEDIA                                        
         CLI   SVSYS,C'J'          FOR PRODUCTION READ OTHELD                   
         BNE   *+10                                                             
         MVC   SVMED,OTHNUM+6-OTHELD(R2)                                        
*                                                                               
         CLI   SVMED,X'40'                                                      
         JNH   PACC410                                                          
*                                                                               
         MVC   SVACTMED,SVMED                                                   
         JAS   RE,GETMED           GET MEDIA                                    
*                                                                               
PACC410  MVC   SVMED,SVACTMED                                                   
*                                                                               
         MVC   VNDMED,SVMED                                                     
         J     PACC10                                                           
*                                                                               
         USING RSTELD,R2                                                        
PACC50   DS    0H                  STATUS - LAST DATE                           
         CLC   RSTTDATE,SVSTDT     ACT DATE < USER SPEC. START DATE?            
         BL    EXIT                YES                                          
         CLC   RSTTDATE,SVENDDT    ACT DATE > USER SPEC. END DATE?              
         BH    EXIT                YES                                          
         GOTO1 DATCON,DMCB,(1,RSTTDATE),(20,VNDLSTDT)  YYYYMMDD                 
         J     PACC10                                                           
*                                                                               
         USING OMEELD,R2                                                        
PACC60   DS    0H                  ONLINE MEMO ELEMENT                          
         CLC   OMEMO,SPACES                                                     
         JNH   PACC10                                                           
*                                                                               
         SHI   R1,OMELN1Q+1        REDUCE THE ELEMENT OVERHEAD                  
         CHI   R1,L'VNDMEMO-1                                                   
         JNH   *+8                                                              
         LHI   R1,L'VNDMEMO-1      MAX LENGTH CAN'T GO BEYOND 62                
*                                                                               
         EXMVC R1,VNDMEMO,OMEMO                                                 
         J     PACC10                                                           
*                                                                               
         USING FFTELD,R2                                                        
PACC70   DS    0H                  FREE FORM- CONTACT,PHONE,EMAIL               
         LLC   R1,FFTDLEN          FREE FROM TEXT DATA LENGTH                   
         LTR   R1,R1               IS LENGTH > 0?                               
         JZ    PACC10                                                           
*                                                                               
         BCTR  R1,0                                                             
         CLI   FFTTYPE,FFTTCONT    IS THIS A CONTACT NAME                       
         JE    PACC710             YES                                          
*                                                                               
         CLI   FFTTYPE,FFTTPTEL    IS THIS A PHONE NUMBER                       
         JE    PACC720             YES                                          
*                                                                               
         CLI   FFTTYPE,FFTTEML     IS THIS A EMAIL ADDRESS                      
         JNE   PACC10              NO                                           
*                                                                               
         CHI   R1,L'VNDEMAIL                                                    
         JNH   *+8                                                              
         LHI   R1,L'VNDEMAIL       MAX LENGTH CAN'T GO BEYOND 67                
*                                                                               
         EXMVC R1,VNDEMAIL,FFTDATA EMAIL ADDRESS                                
         J     PACC10                                                           
*                                                                               
PACC710  CHI   R1,L'VNDCONT-1                                                   
         JNH   *+8                                                              
         LHI   R1,L'VNDCONT-1      MAX LENGTH CAN'T GO BEYOND 29                
*                                                                               
         EXMVC R1,VNDCONT,FFTDATA  CONTACT NAME                                 
         J     PACC10                                                           
*                                                                               
PACC720  CHI   R1,L'VNDPHONE-1                                                  
         JNH   *+8                                                              
         LHI   R1,L'VNDPHONE-1     MAX LENGTH CAN'T GO BEYOND 11                
*                                                                               
         EXMVC R1,VNDPHONE,FFTDATA PHONE NUMBER                                 
         J     PACC10                                                           
*                                                                               
         USING RACELD,R2                                                        
PACC80   DS    0H                  ACTIVITY - ADD DATE                          
         CLI   RACTYPE,RACTADD     IS IT RECORD CREATIONS DATE?                 
         JNE   PACC10              NO- SKIP                                     
*                                                                               
         GOTO1 DATCON,DMCB,(1,RACDATE),(20,VNDCREDT)   YYYYMMDD                 
         J     PACC10                                                           
*                                                                               
PACC90   CLI   HDRSET,C'Y'                                                      
         JE    PACC100                                                          
         CLC   VNDREC(L'SPACES),SPACES                                          
         JE    EXIT                                                             
         LA    RE,TMPREC           INITIALIZE THE BUFFER RECORD                 
         LHI   RF,L'TMPREC                                                      
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0               CLEAR VNDREC TO SPACES                       
         MVC   TMPREC(L'DELIMIT),DELIMIT                                        
         PUT   TAPEOUT,TMPREC                                                   
         MVI   HDRSET,C'Y'                                                      
*                                                                               
         MVC   TMPREC(L'HEADER),HEADER                                          
         PUT   TAPEOUT,TMPREC                                                   
*                                                                               
PACC100  BRAS  RE,INSPIPE          INSERT PIPE DELIMETERS TO OUTPUT             
         PUT   TAPEOUT,VNDREC      WRITE THE RECORDS                            
         AP    TOTRECS,=P'1'                                                    
         MVC   P+1(130),VNDREC     PRINT FIRST 130 BYTES                        
         GOTO1 ACREPORT                                                         
         MVC   P+1(113),VNDREC+130 PRINT NEXT 113 BYTES                         
         GOTO1 ACREPORT                                                         
         MVC   P+1(VNDRECQ-243),VNDREC+243 PRINT REMAINING BYTES                
         GOTO1 ACREPORT                                                         
         J     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
REQL     DS    0H                                                               
         CLOSE (TAPEOUT)           CLOSE THE TAPEOUT FILE                       
         CP    TOTRECS,=P'0'       DO NOT SEND FILE IF IT IS EMPTY              
         JE    EXIT                                                             
         CLI   QOPT1,YESQ          IF QOPT1 = YES, DO NOT SEND MQ               
         JE    EXIT                                                             
         GOTOR MQRPQ,DMCB,(RC)     SFTP FILE TO HUB                             
         J     EXIT                                                             
*                                                                               
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
RUNL     DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        ROUTINE TO EXTRACT NAME                                                
*        R2=A(ELEMENT),R5=A(OUTPUT)                                             
***********************************************************************         
*                                                                               
ACEXNAM  DS    0H                                                               
         MVC   0(36,R5),SPACES     INITIALIZE THE O/P WITH SPACES               
         LLC   RF,1(R2)                                                         
         SH    RF,=H'3'            REDUCE THE ELEMENT OVERHEAD                  
         BMR   RE                                                               
         EXMVC RF,0(R5),2(R2)                                                   
         BR    RE                                                               
*                                                                               
***********************************************************************         
*        INSERT PIPES TO OUPUT LINES                                  *         
***********************************************************************         
                                                                                
INSPIPE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,PIPETAB                                                       
INSPIP10 ZICM  R1,0(RE),2             GET DISP TO WHERE PIPE IS NEEDED          
         LA    RF,VNDREC                                                        
         AR    RF,R1                                                            
         MVI   0(RF),PIPEEQU       MOVE C'|' DELIMETER                          
         LA    RE,L'PIPETAB(RE)                                                 
         CLC   =C'EOT',0(RE)       HAVE WE REACHED EOT                          
         JNE   INSPIP10                                                         
*                                                                               
INSPIPEX XIT1                                                                   
*                                                                               
PIPETAB  DS    0XL2                                                             
         DC    AL2(VNDPIPE1-VNDRECD)                                            
         DC    AL2(VNDPIPE2-VNDRECD)                                            
         DC    AL2(VNDPIPE3-VNDRECD)                                            
         DC    AL2(VNDPIPE4-VNDRECD)                                            
         DC    AL2(VNDPIPE5-VNDRECD)                                            
         DC    AL2(VNDPIPE6-VNDRECD)                                            
         DC    AL2(VNDPIPE7-VNDRECD)                                            
         DC    AL2(VNDPIPE8-VNDRECD)                                            
         DC    AL2(VNDPIPE9-VNDRECD)                                            
         DC    AL2(VNDPIPEA-VNDRECD)                                            
         DC    AL2(VNDPIPEB-VNDRECD)                                            
         DC    AL2(VNDPIPEC-VNDRECD)                                            
         DC    AL2(VNDPIPED-VNDRECD)                                            
         DC    AL2(VNDPIPEE-VNDRECD)                                            
         DC    AL2(VNDPIPEF-VNDRECD)                                            
         DC    C'EOT'                                                           
*                                                                               
PIPEEQU  EQU   C'|'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CLEAR OUTPUT RECORD                                          *         
***********************************************************************         
CLROUT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,VNDREC           INITIALIZE THE BUFFER RECORD                 
         LHI   RF,VNDRECQ                                                       
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0               CLEAR VNDREC TO SPACES                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GET MEDIA                                                    *         
***********************************************************************         
*                                                                               
GETMED   NTR1                                                                   
*                                                                               
*  MEDIA HAS TO BE IN RANGE A-Z OR 0-9 OTHERWISE SET MEDIA = '*'                
*                                                                               
         CLI   SVACTMED,C'9'          TEST IF MEDIA IS ALPHANUMERIC             
         JH    GETMED00                                                         
*                                                                               
         CLI   SVACTMED,C'0'                                                    
         JNL   GETMEDX             0-9 OK                                       
*                                                                               
         CLI   SVACTMED,C'Z'                                                    
         JH    GETMED00                                                         
*                                                                               
         CLI   SVACTMED,C'A'                                                    
         JL    GETMED00                                                         
*                                                                               
         CLI   SVACTMED,C'S'                                                    
         JNL   GETMEDX             S-Z OK                                       
*                                                                               
         CLI   SVACTMED,C'R'                                                    
         JH    GETMED00                                                         
*                                                                               
         CLI   SVACTMED,C'J'                                                    
         JNL   GETMEDX             J-R OK                                       
*                                                                               
         CLI   SVACTMED,C'I'                                                    
         JNH   GETMEDX             A-I OK                                       
*                                                                               
GETMED00 MVI   SVACTMED,C'*'                                                    
*                                                                               
GETMEDX  J     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        GET SYSTEM                                                   *         
***********************************************************************         
*                                                                               
GETSYS   NTR1                                                                   
         MVC   SVSYS,SAVELDG              FOR PAYABLES, USE LEDGER              
         CLI   SVSYS,C'U'                 FOR NET                               
         JNE   *+8                                                              
         MVI   SVSYS,C'N'                 MAKE IT N                             
*                                                                               
         CLI   SVSYS,C'Q'                 FOR CANADIAN PRINT                    
         JNE   *+8                                                              
         MVI   SVSYS,C'P'                 MAKE IT P                             
*                                                                               
         CLI   SVSYS,C'T'                 FOR CANADIAN SPOT                     
         JNE   *+8                                                              
         MVI   SVSYS,C'S'                 MAKE IT S                             
*                                                                               
         CLC   SVUNTLDG,=C'SV'            IS THIS PRODUCTION ?                  
         JNE   XIT                                                              
         MVI   SVSYS,C'J'                                                       
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        PRINT ADDRESS - LINE1, LINE2, CITY, STATE ZIP                *         
***********************************************************************         
*                                                                               
         USING ADRELD,R2                                                        
GETADR   NTR1                                                                   
         MVC   SVADR1(SVADRLQ),SPACES                                           
         MVC   TPADDR0,SPACES           INITIALZED WITH SPACES                  
*                                                                               
         TM    ADRSTAT,ADRCSZ           DO WE HAVE CITY, STATE & ZIP            
         JO    ADR30                                                            
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,ADRNUM              NUMBER OF LINES                         
         JZ    ADRX                     EXIT, IF NO ADDRESS                     
*                                                                               
         CLI   ADRNUM,1                 ONLY ONE LINE                           
         JNE   *+14                                                             
         CLC   ADRADD1,SPACES           AND IT'S SPACES                         
         JNH   ADRX                     NO ADDRESS                              
*                                                                               
         LA    R3,TPADDR                                                        
         LA    R5,ADRADD1                                                       
*                                                                               
ADR00    MVC   0(L'TPADDR,R3),0(R5)     UP TO 4 LINES OF ADDRESS                
         AHI   R5,L'ADRADD1                                                     
         AHI   R3,L'TPADDR                                                      
         JCT   R0,ADR00                                                         
*                                                                               
         LA    R3,TPADDR                POINT TO ADDRESS                        
         LHI   R6,L'ADRADD1             LENGTH OF ONE ADDRESS LINE              
         LA    R5,TPADDR+(3*L'TPADDR)   FOURTH ADDRESS LINE                     
         OC    TPADDR+(1*L'TPADDR),SPACES  MAKE SURE STATE IS UPPERCASE         
         OC    TPADDR+(2*L'TPADDR),SPACES  IN EITHER ADDR LINE 2,3 OR 4         
         OC    TPADDR+(3*L'TPADDR),SPACES                                       
*                                                                               
* WE WILL BE PRINTING THE ADDRESS LINE 1 & 2 EVEN FOR INVALID ADDRESS           
*                                                                               
ADR10    CR    R3,R5                    POINTING TO FIRST LINE?                 
         JE    ADR40                                                            
*                                                                               
         GOTO1 ADFORM,DMCB,((R6),(R5)),(30,SVCITY),SVSTATE,(9,SVZIP),  +        
               (2,SVCTRY),0                                                     
*                                                                               
         TM    0(R1),X'01'              01=FOREIGN, NO VALIDATION DONE          
         JO    ADR20                                                            
*                                                                               
         TM    0(R1),X'80'              80=SERIOUS ERROR                        
         JO    ADR15                                                            
*                                                                               
         CLC   SVCITY,SPACES                                                    
         JH    ADR20                                                            
*                                                                               
ADR15    CLC   0(L'TPADDR,R5),SPACES    IF ZERO, THEN NO ADDRESS LINE           
         JNH   *+8                                                              
*                                                                               
         AHI   R6,L'TPADDR              SEND ANOTHER LINE                       
         SHI   R5,L'TPADDR              BACK UP                                 
         J     ADR10                                                            
*                                                                               
ADR20    TM    0(R1),X'02'                                                      
         JO    ADR40                                                            
         MVC   SVADR1(L'TPADDR),0(R3)   SAVE ADDRESS LINE 1                     
         AHI   R3,L'TPADDR                                                      
         CR    R3,R5                                                            
         JE    ADRX                                                             
*                                                                               
         MVC   SVADR2(L'TPADDR),0(R3)   SAVE ADDRESS LINE 2                     
         J     ADRX                                                             
*                                                                               
ADR30    CLC   ADRLINE1,SPACES                                                  
         JNH   *+10                                                             
         MVC   SVADR1,ADRLINE1          SAVE ADDRESS LINE1                      
*                                                                               
         CLC   ADRLINE2,SPACES                                                  
         JNH   *+10                                                             
         MVC   SVADR2,ADRLINE2          SAVE ADDRESS LINE1                      
*                                                                               
         CLC   ADRCITY,SPACES                                                   
         JNH   *+10                                                             
         MVC   SVCITY(L'ADRCITY),ADRCITY SAVE CITY                              
*                                                                               
         CLC   ADRSTATE,SPACES                                                  
         JNH   *+10                                                             
         MVC   SVSTATE,ADRSTATE         SAVE STATE                              
*                                                                               
         CLC   ADRZIP,SPACES                                                    
         JNH   ADRX                                                             
         MVC   SVZIP,ADRZIP             SAVE ZIP                                
         J     ADRX                                                             
*                                                                               
ADR40    MVC   SVCITY(SVCTRY+2-SVCITY),SPACES                                   
         J     ADRX                                                             
*                                                                               
ADRX     J     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
* FILTER ROUTINE - DETERMINE IF WE WANT THIS COMPANY                 *          
*      AT ENTRY - R2 = ABKBUF (ADDRESS OF BUFFER RECORD FROM SORT)   *          
**********************************************************************          
*                                                                               
FILTER   NMOD1 0,**FILT**                                                       
         L     RC,0(R1)            RESET RC                                     
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
*                                                                               
         USING SSOOFF,RE           CREATE AN EDICT HEADER OR MQ                 
         ICM   RE,15,MCSSB         HEADER                                       
         JZ    FILT10              NO DETAILS PRESENT                           
*                                                                               
         CLI   SSODSPAC,C'C'       CSC REGION ?                                 
         JE    FILT00              YES, SET FILE AS A TEST                      
*                                  NO ,                                         
         CLI   SSODSPAC,C'Q'       FQA REGION ?                                 
         JE    FILT00              YES, SET FILE AS A TEST                      
*                                  NO ,                                         
         CLI   SSODSPAC,C'T'       TST REGION ?                                 
         JNE   FILT10              NO , MUST BE PRODUCTION                      
*                                  YES,                                         
FILT00   DS    0H                                                               
         MVC   DYDDSN2,=CL5'TEST.' MARK IT AS A TEST DATASET                    
*                                                                               
         USING ALPHATABD,R3                                                     
FILT10   DS    0H                                                               
         MVC   DYDDSN4,MCIDPOWC    POWER CODE                                   
         LA    R0,ALPHACNT         LOAD ALPHA ENTRIES COUNT                     
         LA    R3,ALPHATAB         ALPHA TABLE                                  
         MVC   WKRECTY,SPACES                                                   
*                                                                               
FILT20   CLC   MCAGYALP,ALPHID     ALPHA ID FOUND?                              
         JNE   FILT30                                                           
         MVC   WKRECTY,ALPHAREC    YES, SET MQ REC TYPE                         
         J     FILT40              CONTINUE                                     
*                                                                               
FILT30   LA    R3,ALPHALN(R3)      NO, BUMP TO NEXT ENTRY IN TABLE              
         BCT   R0,FILT20                                                        
         DROP  R3                                                               
*                                                                               
FILT40   MVC   DSNME,=CL8'TAPEOUT' PAD DD NAME WITH SPACES                      
         DROP  RF                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(26,SVDTIME)                                   
         MVC   DYDDTE,SVDATE       SAVE DATE DETAILS                            
         MVC   DYDTIM,SVTIME       SAVE TIME DETAILS                            
*                                                                               
* REMOVE SPACES FROM THE DETAILS                                                
*                                                                               
         MVC   WORK(DSNDSLNQ),DYDDSN3   SAVE AT TEMP WORK AREA                  
         MVC   DYDDSN3(DSNDSLNQ),SPACES CLEAR DATA AREA                         
         LA    R0,DSNDSLNQ         LEANGTH OF THE DATA AREA                     
         LA    RE,DYDDSN3          POINT START OF THE DATA AREA                 
         LA    RF,WORK             POINT TEMP WORK AREA                         
*                                                                               
FILT50   CLI   0(RF),X'40'         SPACES ?                                     
         JNH   FILT60              YES, PROCESS NEXT BYTE                       
*                                  NO ,                                         
         MVC   0(1,RE),0(RF)       MOVE DATA                                    
         AHI   RE,1                POINT NEXT BYTE FROM DATA AREA               
*                                                                               
FILT60   AHI   RF,1                POINT NEXT BYTE FROM TEMP WORK AREA          
         JCT   R0,FILT50           CHECK NEXT BYTE                              
*                                                                               
         LA    R6,TAPEOUT          LOCATE THE TAPEOUT FILE                      
         LHI   RF,VNDRECQ                                                       
         STH   RF,TAPEOUT+62       SET THE RECORD BUFFER SIZE                   
         STH   RF,TAPEOUT+82       SET THE RECORD LENGTH                        
*                                                                               
* DYNAMICALLY ALLOCATE THE FILE NAME                                            
*                                                                               
         LA    R5,DYDDSN           POINT DATASET NAME DETAILS                   
         MVI   BYTE,X'45'              CYLINDER (X'40')                         
*                                      CL44 DSN (X'04')                         
*                                      3RD PARM (X'01')                         
         MVC   DUB,=X'000005000001'    PRI=5,SEC=1                              
*                                                                               
         GOTO1 DYNALLOC,DMCB,(X'80',DSNME),(BYTE,DUB),(X'80',(R5))              
*                                                                               
FILTX    XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*********************************************************************           
* WRITE OUT MQ HEADER TO THE MQ FILE                                *           
*********************************************************************           
MQRPQ    NMOD1 0,**MQRP**                                                       
*                                                                               
         L     RC,0(R1)            RESET THE RC                                 
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,MQFILID),(X'E0',0),0                 
*                                                                               
         CLI   DMCB+8,0            MQ OPEN FAILS ?                              
         JE    MQRPQ00             NO, CONTINUE                                 
*                                  YES ,                                        
         MVC   AUTOREAS,OPENFAIL   SEND EMAIL NOTIFICATION AND DUMP             
         J     MQERREND                                                         
*                                                                               
MQRPQ00  MVC   MQMRECTY,WKRECTY         SET MQM REC TYPE                        
         MVC   MQMDSN,SPACES            CLEAR WORK AREA                         
         MVC   MQMDSN(DSNLNQ),DYDDSN3   MOVE DATASET NAME W/O SFTPDISK          
*                                                                               
         MVC   MQDATE,DYDDTE       DSECT DSNMD IS NOT IN SYNC WITH              
         MVC   MQTIME,DYDTIM       DYDDSN - TWO OFF                             
*                                                                               
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQMESS,MQMLNQ,0                          
         CLI   DMCB+8,0            MQ PUT FAILS ?                               
         JE    MQRPQ02             NO, CONTINUE                                 
*                                  YES,                                         
         MVC   AUTOREAS,PUTFAIL    SEND EMAIL NOTIFICATION                      
         J     MQERREND            - AND DUMP                                   
*                                                                               
MQRPQ02  GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
*                                                                               
         CLI   DMCB+8,0            IF MQ PUT FAILS                              
         JE    MQRPQX                                                           
         MVC   AUTOREAS,CLOSFAIL                                                
*                                                                               
MQERREND GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(AUTOLENG),AUTONOTE)                 
         J     *+2                 DIE HERE                                     
*                                                                               
MQRPQX   XIT1                                                                   
***********************************************************************         
* MQ CONSTANTS                                                        *         
***********************************************************************         
MQFILID  DC    CL16'VENDLIST********'                                           
AUTONOTE DC    C'AUTONOTE*JSHA,JSAY'                                            
AUTOREAS DS    CL15                                                             
AUTOLENG EQU   *-AUTONOTE                                                       
OPENFAIL DC    CL(L'AUTOREAS)'MQ OPEN FAILED'                                   
PUTFAIL  DC    CL(L'AUTOREAS)'MQ PUT ERROR'                                     
CLOSFAIL DC    CL(L'AUTOREAS)'MQ CLOSE FAILED'                                  
SETUFAIL DC    CL(L'AUTOREAS)'NOT SETUP TO MQ'                                  
***********************************************************************         
* MQ TABLES                                                           *         
***********************************************************************         
MQMESS   DS    0D                                                               
         DC    CL6'DANOT1'         RECORD TYPE                                  
         DC    CL3'ACC'            MQ KEY                                       
MQMRECTY DC    CL4'TEST'                                                        
         DC    CL8'BILLING '                                                    
         DC    CL8'        '                                                    
MQDATE   DC    CL6' '                                                           
MQTIME   DC    CL6' '                                                           
         DC    CL64' '             EMPTY                                        
MQMDSN   DC    CL128' '            DATASET NAME                                 
MQMLNQ   EQU   *-MQMESS                                                         
         DC    AL1(EOF)                                                         
***********************************************************************         
* CONSTANTS                                                           *         
* THIS WILL BE THE DEFAULT DATASET NAME IF THERE IS NO OVERRIDE FOUND *         
* ON THE AFM/BANK RECORD.                                             *         
***********************************************************************         
*                                                                               
DYDDSN   DS    0CL41                                                            
DYDDSN1  DC    CL9'SFTPDISK.'                                                   
DYDDSN2  DC    CL5'PROD.'          PROD OR TEST BASED ON THE RUN                
DYDDSN3  DC    CL4'VND.'           TRNSMISSION TYPE                             
DYDDSN4  DC    CL4'XXXX'           POWER CODE                                   
         DC    CL1'.'              .                                            
         DC    CL1'D'              D PREFIX FOR TODAY'S DATE                    
DYDDTE   DC    CL6' '              TODAY'S DATE W/  D PREFIX                    
         DC    CL1'.'              .                                            
         DC    CL1'T'              T PREFIX FOR CURRENT TIME                    
DYDTIM   DC    CL7' '              CURRENT TIME                                 
         DC    CL4'.CSV'           CSV FILE                                     
*                                                                               
DSNDSLNQ EQU   *-DYDDSN3           LENGTH W/O DSN CONSTANTS                     
DSNLNQ   EQU   *-DYDDSN            TOTAL LENGTH                                 
*                                                                               
DYRTDSN  DC    CL25' '                                                          
DYRTGEN  DC    CL8' '        G0001V00                                           
DYRTND   DC    CL19' '       SPARE                                              
DYRTLNQ  EQU   *-DYRTDSN                                                        
*                                                                               
***********************************************************************         
* DCB                                                                 *         
***********************************************************************         
*                                                                               
         PRINT GEN                                                              
TAPEOUT  DCB   DDNAME=TAPEOUT,         DOS SYS004                      +        
               DSORG=PS,                                               +        
               RECFM=FB,                                               +        
               LRECL=VNDRECQ,                                          +        
               BLKSIZE=VNDRECQ,        DOS BLKSIZE=00100               +        
               MACRF=(PM)                                                       
         DS    0F                                                               
*                                                                               
         DC    C'*VNDREC'                                                       
VNDREC   DS    CL(VNDLENQ)         BUFFER AREA                                  
VNDRECQ EQU    *-VNDREC                                                         
TMPREC   DS    CL(VNDLENQ)         TEMP BUFFER                                  
*                                                                               
***********************************************************************         
*              EXITS                                                  *         
***********************************************************************         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
EXIT     XMOD1                                                                  
         EJECT                                                                  
*******0***************************************************************         
*        LITERALS                                                     *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
PAYTAB   DC    CL2'SP'                                                          
PAYTABLN EQU   *-PAYTAB                                                         
         DC    CL2'SQ'                                                          
         DC    CL2'SS'                                                          
         DC    CL2'ST'                                                          
         DC    CL2'SU'                                                          
         DC    CL2'SV'                                                          
         DC    CL2'SW'                                                          
         DC    CL2'SX'                                                          
         DC    CL2'SY'                                                          
         DC    X'FF'                                                            
*                                                                               
ALPHATAB DC    CL2'*B',CL4'DDSB'                                                
ALPHALN  EQU   *-ALPHATAB                                                       
         DC    CL2'*2',CL4'DDS2'                                                
         DC    CL2'OB',CL4'DDSO'                                                
         DC    CL2'O2',CL4'DD2O'                                                
ALPHACNT EQU   (*-ALPHATAB)/6                                                   
*                                                                               
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
*                                                                               
TOTRECS  DC    PL6'0'                                                           
HDRSET   DC    CL1'N'                                                           
DELIMIT  DC    CL5'SEP=|'                                                       
HEADER   DC   0CL154               HEADER                                       
         DC    CL11'VENDOR NAME'                                                
         DC    CL1'|'                                                           
         DC    CL09'VENDOR ID'                                                  
         DC    CL1'|'                                                           
         DC    CL10'MEDIA CODE'                                                 
         DC    CL1'|'                                                           
         DC    CL14'ADDRESS LINE 1'                                             
         DC    CL1'|'                                                           
         DC    CL14'ADDRESS LINE 2'                                             
         DC    CL1'|'                                                           
         DC    CL04'CITY'                                                       
         DC    CL1'|'                                                           
         DC    CL05'STATE'                                                      
         DC    CL1'|'                                                           
         DC    CL03'ZIP'                                                        
         DC    CL1'|'                                                           
         DC    CL05'PHONE'                                                      
         DC    CL1'|'                                                           
         DC    CL04'MEMO'                                                       
         DC    CL1'|'                                                           
         DC    CL10'CONTACT NO'                                                 
         DC    CL1'|'                                                           
         DC    CL09'E-MAIL ID'                                                  
         DC    CL1'|'                                                           
         DC    CL12'COUNTRY CODE'                                               
         DC    CL1'|'                                                           
         DC    CL11'CREATE DATE'                                                
         DC    CL1'|'                                                           
         DC    CL18'LAST ACTIVITY DATE'                                         
         DC    CL1'|'                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        DSECT TO COVER SPACEND                                       *         
***********************************************************************         
WRKD     DSECT                                                                  
PRELOC   DS    F                                                                
ADFORM   DS    V                                                                
VMQRPT   DS    V                   A(MQ FILE INTERFACE)                         
*                                                                               
SAVECMP  DS    CL38                                                             
SAVEUNT  DS    CL38                                                             
SAVELDG  DS    CL38                                                             
*                                                                               
SVSTDT   DS    PL3        QSTART (YMD)                                          
SVENDDT  DS    PL3        QEND  (YMD)                                           
*                                                                               
SVUNTLDG DS    CL2                 UNIT LEDGER                                  
SVACTMED DS    CL1                 MEDIA FROM ACCOUNT                           
*                                                                               
SVSYSMED DS    0CL2                                                             
SVSYS    DS    CL1                 SYSTEM                                       
SVMED    DS    CL1                        MEDIA                                 
*                                                                               
TPADDR0  DS    0CL(4*L'ADRADD1)                                                 
TPADDR   DS    4CL(L'ADRADD1)      ADDRESS LINES                                
SVADR1   DC    CL(L'ADRLINE1)' '   ADDRESS LINE 1                               
SVADR2   DC    CL(L'ADRLINE2)' '   ADDRESS LINE 2                               
SVCITY   DC    CL30' '             CITY                                         
SVSTATE  DC    CL2' '                  STATE                                    
SVZIP    DC    CL9' '                       ZIP                                 
SVCTRY   DC    CL2' '                          COUNTRY                          
SVADRLQ  EQU   *-SVADR1                                                         
*                                                                               
FLAG     DS    XL1                 FLAG TO SHOW STATUS BITS                     
FLGTPE   EQU   X'80'               ACCOUNT NOT SET UP FOR TAPE                  
FLGSWCH  EQU   X'40'               ROUTINE HAS SWITCHED                         
FLGPOS   EQU   X'20'               ACCOUNT IS A POS PAY ACCOUNT                 
FLGMULTI EQU   X'10'               MULTPLE ACCOUNTS IN RUN                      
FLGSKP   EQU   X'08'               SKIP TAPE RECORDS                            
FLGFRM   EQU   X'04'               BANK IS A FORMAT                             
FLGREC   EQU   X'02'               READING BANK RECORD                          
FLGNOP   EQU   X'01'               DO NOT INCLUDE IN REPORT                     
*                                                                               
FLAG2    DS    XL1                 FLAG TO SHOW STATUS BITS                     
FLGBLD   EQU   X'80'               NEED TO GO TO BLDSPEC                        
*                                                                               
SVDTIME  DS    0CL14               DATE/TIME                                    
SVYY     DS    CL2                                                              
SVDATE   DS    CL6                 DATE                                         
SVTIME   DS    CL6                      TIME                                    
SVDSN    DS    CL15                DATASET NAME                                 
DSNME    DS    CL8                 DDNAME FOR LINKAREA                          
*                                                                               
EOF      EQU   X'FF'                                                            
WKRECTY  DS    CL4                                                              
*                                                                               
***********************************************************************         
* VENDOR DSECT                                                        *         
***********************************************************************         
*                                                                               
VNDRECD  DSECT                                                                  
VNDCMPNM DS    CL36                VENDOR COMPANY NAME                          
VNDPIPE1 DS    CL1                 PIPE 01                                      
VNDID    DS    CL12                VENDOR ID                                    
VNDPIPE2 DS    CL1                 PIPE 02                                      
VNDSMED  DS    0CL2                SYSTEM/MEDIA                                 
VNDSYS   DS    CL1                 SYSTEM                                       
VNDMED   DS    CL1                 MEDIA                                        
VNDPIPE3 DS    CL1                 PIPE 05                                      
VNDADLN1 DS    CL40                COMPANY ADDR LINES 1                         
VNDPIPE4 DS    CL1                 PIPE 06                                      
VNDADLN2 DS    CL40                COMPANY ADDR LINES 2                         
VNDPIPE5 DS    CL1                 PIPE 07                                      
VNDADCTY DS    CL29                CITY                                         
VNDPIPE6 DS    CL1                 PIPE 08                                      
VNDADST  DS    CL2                     STATE                                    
VNDPIPE7 DS    CL1                 PIPE 09                                      
VNDADZIP DS    CL9                          ZIP                                 
VNDPIPE8 DS    CL1                 PIPE 10                                      
VNDPHONE DS    CL11                PHONE                                        
VNDPIPE9 DS    CL1                 PIPE 11                                      
VNDMEMO  DS    CL62                MEMO                                         
VNDPIPEA DS    CL1                 PIPE 12                                      
VNDCONT  DS    CL29                CONTACT                                      
VNDPIPEB DS    CL1                 PIPE 13                                      
VNDEMAIL DS    CL67                EMAIL                                        
VNDPIPEC DS    CL1                 PIPE 14                                      
VNDCTRY  DS    CL2                 COUNTRY CODE                                 
VNDPIPED DS    CL1                 PIPE 15                                      
VNDCREDT DS    CL8                 VENDOR CREATE DATE                           
VNDPIPEE DS    CL1                 PIPE 16                                      
VNDLSTDT DS    CL8                 LAST ACTIVITY DATE                           
VNDPIPEF DS    CL1                 PIPE 17                                      
VNDLENQ  EQU   *-VNDRECD           RECORD LENGTH                                
*                                                                               
ALPHATABD DSECT                                                                 
ALPHID    DS    CL2                                                             
ALPHAREC  DS    CL4                                                             
*                                                                               
***********************************************************************         
*        INCLUDED DSECTS                                                        
***********************************************************************         
* ACADFORMD                                                                     
* ACGENBOTH                                                                     
* ACGENFILE                                                                     
* ACGENMODES                                                                    
* ACREPWORKD                                                                    
* ACGENRAC                                                                      
* DDMASTD                                                                       
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACADFORMD                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENRAC                                                       
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPVE02 04/22/20'                                      
         END                                                                    
