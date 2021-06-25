*          DATA SET PPINF11    AT LEVEL 006 AS OF 06/16/10                      
*PHASE T41A11A                                                                  
*INCLUDE OFFOUT                                                                 
*INCLUDE HEXOUT                                                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41A11 - CHANGE LOG  '                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SMYE 6/16/10  DELETE REFERENCE TO POWER CODES NO LONGER ON DDS SYSTEM         
*                                                                               
* KWAN 10/13/05 TWO CHARACTER OFFICE CODE MODIFICATIONS                         
*                                                                               
* SMYE   04/02 - NEW LIMIT ACCESS SECURITY                                      
*                                                                               
* KWAN   08/99 - REMOVE COS2= OVERHEAD WHEN DISPLAYING COS2                     
*                                                                               
* KWAN   03/99 - ADD COST2 FACTOR OPTIONS TO FILTER FIELD                       
*                                                                               
* BPLA    9/98 - CHANGE FROZEN FILTER TO WORK LIKE SFH AND FIN                  
*                FRZ=Y AND FRZ=N                                                
*                                                                               
* SMYE   08/98 - FROZEN CLIENT COLUMN "HEADER" SHOWN FOR ALL AGENCIES           
*                ADD "FROZEN" TO DATA FIELD AND FILTER                          
*                                                                               
* SMYE   05/98 - FROZEN CLIENT COLUMN "HEADER" SHOWN ONLY FOR                   
*                "WESTERN" AGENCIES (TEMPORARY)                                 
*                                                                               
* SMYE   04/98 - ADD FROZEN CLIENT (FRZ) DATA FIELD AND FILTER                  
*                OPEN SFH (*NOP*) TO ALL AGENCIES (NOT JUST WESTERN)            
*                                                                               
* SMYE   11/97 - ADD SPECIAL FINANCIAL HANDLING (SFH) DATA FIELD                
*                AND FILTER FOR WESTERN AGENCIES ONLY                           
*                                                                               
* SMYE   03/97 - ADD 2-CHAR. CLIENT OFFICE DISPLAY IN PFSEND                    
*                                                                               
* SMYE   02/97 - ADD CLIENT, CLIENT GROUP, OFFICE AND OFFICE LIST               
*                SECURITY (LIMIT ACCESS) IN CDHAVREC                            
*                                                                               
         TITLE 'T41A11 - PRINTPAK INFO CLIENT HEADERS'                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41A11   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 CLTWRKX-CLTWRKD,T41A11,R8,RR=R9                                  
*                                                                               
         LR    R3,RC                                                            
         USING CLTWRKD,R3                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T41AFFD,RA                                                       
         USING OFFICED,OFCBLK                                                   
*                                                                               
         ST    R9,RELO                                                          
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 VCALLOV,DMCB        GET OFFICER ADDRESS                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   OFFICER,DMCB        SAVE ADDRESS OF OFFICER                      
*                                                                               
         LA    RE,REC2                                                          
         LA    RF,2000                                                          
         XCEFL                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         BRAS  RE,FLT_COFC         FILTER - CLIENT OFFICE CODE                  
         BE    *+16                                                             
         LHI   RE,6+1+2+1+2        MAX DATA INPUT: OFFICE=XX-YY                 
         L     R6,AFLDDATA         ADDRESS OF FILTER FIELD DATA                 
         B     FLTERR                                                           
*                                                                               
         B     CHKGRP                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
CHKGRP   DS    0H                  GROUP FILTER                                 
         MVI   WORK,C' '                                                        
         MVC   WORK+1(20),WORK                                                  
         MVC   WORK(7),=C'GROUP=X'                                              
         XC    CGRPFLT,CGRPFLT     CLEAR OFFICE FILTER                          
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(6,=C'GROUP=')                         
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKGRP5             ALTERNATE GROUP FILTER                       
         L     R4,4(R1)                                                         
         LA    R4,6(R4)                                                         
         BAS   R9,GETALPH                                                       
         L     R4,4(R1)            REINITIAL R4                                 
         LA    R4,6(R4)                                                         
         L     R6,4(R1)                                                         
         LA    RE,8                USED IN FLTERR                               
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         CHI   R5,1                                                             
         BH    FLTERR                                                           
         MVC   CGRPFLT(1),0(R4)                                                 
         B     CHKFIN                                                           
         EJECT                                                                  
CHKGRP5  DS    0H                  GROUP FILTER                                 
         MVI   WORK,C' '                                                        
         MVC   WORK+1(20),WORK                                                  
         MVC   WORK(5),=C'GRP=X'                                                
         XC    CGRPFLT,CGRPFLT     CLEAR OFFICE FILTER                          
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(4,=C'GRP=')                           
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKFIN                                                           
         L     R4,4(R1)                                                         
         LA    R4,4(R4)                                                         
         BAS   R9,GETALPH                                                       
         L     R4,4(R1)            REINITIAL R4                                 
         LA    R4,4(R4)                                                         
         L     R6,4(R1)                                                         
         LA    RE,8                USED IN FLTERR                               
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         CHI   R5,1                                                             
         BH    FLTERR                                                           
         MVC   CGRPFLT(1),0(R4)                                                 
         B     CHKFIN                                                           
         EJECT                                                                  
*                                                                               
CHKFIN   DS    0H                  FINANCIAL FILTER                             
         MVI   WORK,C' '                                                        
         MVC   WORK+1(20),WORK                                                  
         MVC   WORK(07),=C'FIN=Y,N'                                             
         XC    CFINFLT,CFINFLT     CLEAR FINANCIAL FILTER                       
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(4,=C'FIN=')                           
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKSFH                                                           
         L     R4,4(R1)                                                         
         LA    R4,4(R4)                                                         
         BAS   R9,GETALPH                                                       
         L     R4,4(R1)            REINITIAL R4                                 
         LA    R4,4(R4)                                                         
         L     R6,4(R1)                                                         
         LA    RE,8                USED IN FLTERR                               
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         CHI   R5,1                                                             
         BH    FLTERR                                                           
         CLI   0(R4),C'Y'                                                       
         BE    CHKF5                                                            
         CLI   0(R4),C'N'                                                       
         BNE   FLTERR                                                           
CHKF5    MVC   CFINFLT(1),0(R4)                                                 
         B     CHKSFH                                                           
         EJECT                                                                  
*                                                                               
CHKSFH   DS    0H                 SPECIAL FINANCIAL HANDLING FILTER             
         MVI   WORK,C' '                                                        
         MVC   WORK+1(20),WORK                                                  
         MVC   WORK(07),=C'SFH=Y,N'                                             
         XC    CSFHFLT,CSFHFLT     CLEAR SFH FILTER                             
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(4,=C'SFH=')                           
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKFRZ                                                           
         L     R4,4(R1)                                                         
         LA    R4,4(R4)                                                         
         BAS   R9,GETALPH                                                       
         L     R4,4(R1)            REINITIAL R4                                 
         LA    R4,4(R4)                                                         
         L     R6,4(R1)                                                         
         LA    RE,8                USED IN FLTERR                               
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         CHI   R5,1                                                             
         BH    FLTERR                                                           
         CLI   0(R4),C'Y'                                                       
         BE    CHKSF5                                                           
         CLI   0(R4),C'N'                                                       
         BNE   FLTERR                                                           
CHKSF5   MVC   CSFHFLT(1),0(R4)                                                 
         B     CHKFRZ                                                           
         EJECT                                                                  
*                                                                               
CHKFRZ   DS    0H                 FROZEN FILTER                                 
         MVI   WORK,C' '                                                        
         MVC   WORK+1(20),WORK                                                  
         MVC   WORK(07),=C'FRZ=Y,N'                                             
         XC    CFRZFLT,CFRZFLT     CLEAR FRZ FILTER                             
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(4,=C'FRZ=')                           
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKACCO                                                          
         L     R4,4(R1)                                                         
         LA    R4,4(R4)                                                         
         BAS   R9,GETALPH                                                       
         L     R4,4(R1)            REINITIAL R4                                 
         LA    R4,4(R4)                                                         
         L     R6,4(R1)                                                         
         LA    RE,8                USED IN FLTERR                               
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         CHI   R5,1                                                             
         BH    FLTERR                                                           
         CLI   0(R4),C'Y'                                                       
         BE    CHKFR5                                                           
         CLI   0(R4),C'N'                                                       
         BNE   FLTERR                                                           
CHKFR5   MVC   CFRZFLT(1),0(R4)                                                 
         B     CHKACCO                                                          
         EJECT                                                                  
*                                                                               
CHKACCO  DS    0H                  ACC OFFICE                                   
         MVI   WORK,C' '                                                        
         MVC   WORK+1(20),WORK                                                  
         MVC   WORK(10),=C'AOFFICE=XX'                                          
         XC    CACCOFF,CACCOFF     CLEAR ACC OFFICE FILTER                      
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(8,=C'AOFFICE=')                       
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKACC5             ALTERNATE ACC OFFICE FITER                   
         L     R4,4(R1)                                                         
         LA    R4,8(R4)                                                         
         BAS   R9,GETALPH                                                       
         L     R4,4(R1)            REINITIAL R4                                 
         LA    R4,8(R4)                                                         
         L     R6,4(R1)                                                         
         LA    RE,11               USED IN FLTERR                               
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         CHI   R5,2                                                             
         BH    FLTERR                                                           
         MVC   CACCOFF(2),0(R4)                                                 
         OC    CACCOFF,=C'  '                                                   
         B     CHKCOSF                                                          
         EJECT                                                                  
CHKACC5  DS    0H                  GROUP FILTER                                 
         MVI   WORK,C' '                                                        
         MVC   WORK+1(20),WORK                                                  
         MVC   WORK(9),=C'ACCOFF=XX'                                            
         XC    CACCOFF,CACCOFF     CLEAR OFFICE FILTER                          
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(7,=C'ACCOFF=')                        
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKCOSF                                                          
         L     R4,4(R1)                                                         
         LA    R4,7(R4)                                                         
         BAS   R9,GETALPH                                                       
         L     R4,4(R1)            REINITIAL R4                                 
         LA    R4,7(R4)                                                         
         L     R6,4(R1)                                                         
         LA    RE,10               USED IN FLTERR                               
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         CHI   R5,2                                                             
         BH    FLTERR                                                           
         MVC   CACCOFF(2),0(R4)                                                 
         OC    CACCOFF,=C'  '                                                   
         B     CHKCOSF                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
CHKCOSF  DS    0H                  COST2 FACTOR FILTER                          
         MVI   WORK,C' '                                                        
         MVC   WORK+1(20),WORK                                                  
         MVC   WORK(08),=C'COS2=Y,N'                                            
         XC    CCOS2FLT,CCOS2FLT                                                
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(5,=C'COS2=')                          
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKDATA                                                          
         L     R4,4(R1)                                                         
         LA    R4,5(R4)            BYPASS "COS2="                               
         BAS   R9,GETALPH                                                       
         L     R4,4(R1)            REINITIAL R4                                 
         LA    R4,5(R4)                                                         
         L     R6,4(R1)                                                         
         LA    RE,8                USED IN FLTERR                               
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         CHI   R5,1                                                             
         BH    FLTERR                                                           
         CLI   0(R4),C'Y'                                                       
         BE    CHKCF10                                                          
         CLI   0(R4),C'N'                                                       
         BNE   FLTERR                                                           
CHKCF10  MVC   CCOS2FLT(1),0(R4)                                                
         B     CHKDATA                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
CHKDATA  MVI   WORK,C' '                                                        
         MVC   WORK+1(20),WORK                                                  
         MVC   WORK(18),=C'(PROF,OFF,GRP,FIN)'                                  
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(5,=C'DATA=')                          
         OC    4(4,R1),4(R1)                                                    
         BNZ   PROFRTN                                                          
*                                                                               
         OC    COFFFLT1,COFFFLT1   CLT OFFICE CODE FILTER PRESENT?              
         BNZ   CHKD5                                                            
         CLI   CGRPFLT,0           SEE IF ANY FILTER ENTERED                    
         BNE   CHKD5                                                            
         CLI   CFINFLT,0           SEE IF ANY FILTER ENTERED                    
         BNE   CHKD5                                                            
         CLI   CSFHFLT,0           SEE IF ANY FILTER ENTERED                    
         BNE   CHKD5                                                            
         CLI   CFRZFLT,0           SEE IF ANY FILTER ENTERED                    
         BNE   CHKD5                                                            
         CLI   CACCOFF,0           SEE IF ANY FILTER ENTERED                    
         BNE   CHKD5                                                            
         CLI   CCOS2FLT,0          COST2 FACTOR FILTER ENTERED?                 
         BNE   CHKD5                                                            
         CLI   SINIFLTH+5,0        CHK FOR ANY INPUT IN FILTER LINE             
         BE    CHKD5                                                            
         LA    R2,SINIFLTH                                                      
         LA    R3,INVERR           BAD FILTER MUST HAVE BEEN INPUT              
         B     ERROR                                                            
*                                                                               
CHKD5    DS    0H                                                               
         USING FLDHDRD,R2                                                       
*                                                                               
*        CLIENT DEFAULT ROUTINES                                                
*                                                                               
         MVI   CDFRST,1                                                         
         LA    R7,REC2                                                          
         XC    FULL,FULL                                                        
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING CLTHDRD,R5                                                       
         MVC   PCLTKAGY,SVAGY                                                   
         MVC   PCLTKMED,SVEBCMED                                                
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,SVCLT                                                   
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY(24),PREVKEY                                                  
         XC    PREVKEY,PREVKEY                                                  
CDHIGH   BAS   RE,HIGH                                                          
         B     CDHAVREC                                                         
CDSEQ    BAS   RE,SEQ                                                           
*                                                                               
CDHAVREC LA    R5,KEY                                                           
         CLC   PCLTKAGY(3),SVAGY   CHK SAME AGY/MED                             
         BNE   CDEND               NO DONE                                      
         CLI   PCLTKRCD,X'02'                                                   
         BNE   CDEND                                                            
         MVI   CDFRST,0                                                         
         BAS   RE,GETREC                                                        
         L     R5,AREC                                                          
*                                                                               
* TEST LIMIT ACCESS FOR BYPASS                                                  
*                                                                               
ACCCHK   DS    0H                  CHECK FOR ANY LIMIT ACCESS                   
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   ACCXCL                                                           
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    ACCX                NOTHING TO CHECK                             
*                                                                               
ACCXCL   DS    0H                  TEST FOR "IGNORE CHECK" ENTRY                
         CLI   6(RA),C'*'          OFFICE OR CLT GRP LIMIT ACCESS ?             
         BNE   ACCTST              NO                                           
         CLI   PCLTOFF+2,C'*'      IGNORE OFFICE CHECK ?                        
         BNE   ACCTST              NO                                           
         CLI   7(RA),C'A'          SEE IF CLT GRP (*AN)                         
         BL    ACCX                NO - MUST BE OFFICE - IGNORE CHECK           
         CLI   7(RA),C'Z'                                                       
         BH    ACCX                     MUST BE OFFICE - IGNORE CHECK           
         CLI   8(RA),C'0'          SEE IF 3RD BYTE IS NUMERIC                   
         BL    ACCX                NO - NOT CLT GRP - IGNORE CHECK              
         CLI   8(RA),C'9'                                                       
         BH    ACCX                     NOT CLT GRP - IGNORE CHECK              
*                                                                               
ACCTST   DS    0H                                                               
         MVC   BYTE4,PCLTOFF       SAVE CLIENT OFFICE                           
         BRAS  RE,CKTRAFID         TRAFFIC ID SIGN-ON ?                         
         BNE   ACCTSTD             NO                                           
         BRAS  RE,TRAFFACC         LOOK FOR CLIENT TRAFFIC OFFICE CODE          
         CLI   BYTE3,0             ANYTHING FOUND ?                             
         BE    ACCTSTD             NO                                           
         MVC   PCLTOFF,BYTE3       USE CLIENT TRAFFIC OFFICE CODE TO..          
ACCTSTD  DS    0H                  TEST OFFICE SECURITY (ANY ACCESS)            
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(25),KEY      SAVE CLIENT KEY                               
         BAS   RE,PPCLIVER                                                      
         BE    ACCREST             ACCESS OK - RESTORE SEQUENCE                 
         XC    KEY,KEY                                                          
         MVC   KEY(25),SVKEY                                                    
         BAS   RE,HIGH             RESTORE SEQUENCE                             
*                                                                               
         B     CDSEQ               BYPASS                                       
*                                                                               
ACCREST  DS    0H                                                               
         MVC   PCLTOFF,BYTE4      "RESTORE" CLIENT OFFICE                       
         XC    KEY,KEY                                                          
         MVC   KEY(25),SVKEY                                                    
         BAS   RE,HIGH             RESTORE SEQUENCE                             
*                                                                               
ACCX     DS    0H                                                               
*                                                                               
         BRAS  RE,TSTCOFCF         TEST CLT OFFICE CODE FILTER                  
         BNE   CDSEQ                                                            
*                                                                               
CDHAVR7  CLI   CGRPFLT,0                                                        
         BE    CDHAVR8                                                          
         CLC   PCLTBLGP,CGRPFLT                                                 
         BNE   CDSEQ                                                            
*                                                                               
CDHAVR8  CLI   CFINFLT,0                                                        
         BE    CDHAVR9                                                          
         CLI   CFINFLT,C'Y'                                                     
         BNE   CDHAVR8C                                                         
         CLI   PCLTFIN,C'Y'                                                     
         BNE   CDSEQ               ONLY FINANCIAL CLTS                          
         B     CDHAVR9                                                          
*                                                                               
CDHAVR8C CLI   CFINFLT,C'N'                                                     
         BNE   CDHAVR9                                                          
         CLI   PCLTFIN,0                                                        
         BNE   CDSEQ                                                            
*                                                                               
CDHAVR9  CLI   CSFHFLT,0                                                        
         BE    CDHAVRA                                                          
         CLI   CSFHFLT,C'Y'        ONLY SFH CLIENTS ?                           
         BNE   CDHAVR9C            NO                                           
         TM    PCLTSTAT,X'01'      SFH CLIENT ?                                 
         BNO   CDSEQ               NO - SKIP                                    
         B     CDHAVRA                                                          
*                                                                               
CDHAVR9C CLI   CSFHFLT,C'N'                                                     
         BNE   CDHAVRA                                                          
         TM    PCLTSTAT,X'01'      SFH CLIENT ?                                 
         BO    CDSEQ               YES - SKIP                                   
*                                                                               
CDHAVRA  CLI   CFRZFLT,C'Y'        ONLY FROZEN CLIENTS ?                        
         BNE   CDHAVRB             NO                                           
         TM    PCLTSTAT,X'02'      FROZEN CLIENT ?                              
         BNO   CDSEQ               NO - SKIP                                    
*                                                                               
CDHAVRB  CLI   CFRZFLT,C'N'        NO FROZEN CLIENTS ?                          
         BNE   CDHAVRC             NO                                           
         TM    PCLTSTAT,X'02'      FROZEN CLIENT ?                              
         BO    CDSEQ               YES- SKIP                                    
*                                                                               
CDHAVRC  CLI   CACCOFF,0           ACC OFFICE FILTER                            
         BE    CDHAVRD                                                          
         CLC   PCLTAOFC,CACCOFF                                                 
         BNE   CDSEQ                                                            
*                                                                               
CDHAVRD  CLI   CCOS2FLT,0          COST2 FACTOR FILTER                          
         BE    CDHAVRE                                                          
         CLI   CCOS2FLT,C'Y'                                                    
         BNE   CDHAVRDC                                                         
         TM    PCLTSTAT,X'04'      COST2 $?                                     
         BO    CDHAVRE                                                          
         TM    PCLTSTAT,X'08'      COST2 FACTOR?                                
         BO    CDHAVRE                                                          
         B     CDSEQ               COST2 IS NOT PRESEMT, GET NEXT REC           
*                                                                               
CDHAVRDC CLI   CCOS2FLT,C'N'       COST2 IS A NEGATIVE FILTER?                  
         BNE   CDHAVRE                                                          
         TM    PCLTSTAT,X'04'      COST2 $ (COS2=Y)                             
         BO    CDSEQ                                                            
         TM    PCLTSTAT,X'08'      COST2 FACTOR (COS2=9.999999)                 
         BO    CDSEQ                                                            
*                                                                               
*                                                                               
*                                                                               
CDHAVRE  DS    0X                  IF NEW OPTIONS NEED TO BE ADDED              
*                                                                               
*                                                                               
*                                                                               
         XC    0(24,R7),0(R7)      MOVE DATA TO TABLE                           
         MVC   0(3,R7),PCLTKCLT                                                 
         MVI   4(R7),C'/'                                                       
         MVC   5(19,R7),PCLTNAME                                                
         LA    R7,24(R7)                                                        
         L     RE,FULL             COUNT TABLE ENTRIES                          
         LA    RE,1(RE)                                                         
         ST    RE,FULL                                                          
         CHI   RE,42                                                            
         BL    CDSEQ                                                            
         MVC   PREVKEY,KEY                                                      
*                                                                               
CDEND    CLI   CDFRST,0                                                         
         BE    CDEND2                                                           
         B     MODEXIT                                                          
CDEND2   L     R9,FULL                                                          
*                                                                               
* FORMAT ALPHA SCREEN                                                           
*                                                                               
         GOTO1 FRMTALPH,DMCB,(24,REC2),(R9),14,(3,DMWORK)                       
         LA    R2,SINHDRH                                                       
         LA    R7,DMWORK                                                        
         LA    R4,3                                                             
         LA    RE,FLDDATA+1                                                     
         LA    RF,LINLEN(RE)                                                    
*                                                                               
* MOVE IN HEADERS                                                               
*                                                                               
CDEND3   CLI   0(R7),0                                                          
         BE    CDSEND                                                           
         MVC   0(16,RE),=C'CLIENT CODE/NAME'                                    
         MVC   0(16,RF),=C'----------------'                                    
         LA    RE,26(RE)                                                        
         LA    RF,26(RF)                                                        
         LA    R7,4(R7)                                                         
         BCT   R4,CDEND3                                                        
CDSEND   FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
         EJECT                                                                  
CDSEND2  LA    RE,FLDDATA+1                                                     
         LA    R7,DMWORK                                                        
         LA    R4,3                                                             
         CLI   0(R7),0             DONE - EXIT                                  
         BE    MODEXIT                                                          
CDSEND3  CLI   0(R7),0             LINE DONE                                    
         BE    CDSEND4             YES - SEND IT                                
         L     RF,0(R7)                                                         
         MVC   0(24,RE),0(RF)                                                   
         SR    RF,RF                                                            
         IC    RF,0(R7)                                                         
         BCTR  RF,0                                                             
         L     R6,0(R7)                                                         
         LA    R6,24(R6)                                                        
         ST    R6,0(R7)                                                         
         STC   RF,0(R7)                                                         
         LA    R7,4(R7)                                                         
         LA    RE,26(RE)                                                        
         BCT   R4,CDSEND3                                                       
*                                                                               
CDSEND4  FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     CDSEND2                                                          
         EJECT                                                                  
*                                                                               
* CLIENT PROFILE DISPLAY SCREEN ROUTINE                                         
*                                                                               
PROFRTN  L     R6,4(R1)            GET FILTER ENTRY                             
         MVI   CFSCRSW,0           INIT COST2 FACTOR DISP SCR SWITCH            
         CLC   5(4,R6),=C'PROF'                                                 
         BE    PF1                                                              
         CLC   5(3,R6),=C'OFF'     OFFICE                                       
         BE    PF1                                                              
         CLC   5(3,R6),=C'GRP'     GROUP                                        
         BE    PF1                                                              
         CLC   5(5,R6),=C'GROUP'   GROUP                                        
         BE    PF1                                                              
         CLC   5(3,R6),=C'FIN'     FINANCIAL                                    
         BE    PF1                                                              
         CLC   5(3,R6),=C'SFH'     SPECIAL FINANCIAL HANDLING                   
         BE    PF1                                                              
         CLC   5(3,R6),=C'FRZ'     FROZEN CLIENT                                
         BE    PF1                                                              
         CLC   5(6,R6),=C'FROZEN'  FROZEN CLIENT                                
         BE    PF1                                                              
         CLC   5(4,R6),=C'AOFF'    ACC OFFICE                                   
         BE    PF1                                                              
         CLC   5(4,R6),=C'ACCO'    ACC OFFICE                                   
         BE    PF1                                                              
*                                                                               
* ALL THE ABOVE DISPLAY SAME DATA                                               
*                                                                               
         CLC   5(4,R6),=C'COS2'    COST2 FACTOR                                 
         BNE   *+12                                                             
         MVI   CFSCRSW,C'Y'        COST2 FACTOR DISP SCREEN SW IS ON            
         B     PF1                                                              
*                                                                               
*                                                                               
*                                                                               
         B     DATAERR             KEY WORD IS NOT DEFINED FOR "DATA="          
*                                                                               
* GET PROFILE FILTERS                                                           
*                                                                               
PF1      LA    R5,SINIFLT                                                       
         XC    CPROFLT,CPROFLT                                                  
         MVI   CDFRST,1                                                         
POS1     GOTO1 GETFLTR,DMCB,(64,(R5)),(4,=C'CHAR')                              
         XC    WORK,WORK                                                        
         MVC   WORK(07),=C'CHARNN='                                             
         OC    4(4,R1),4(R1)                                                    
         BNZ   POS1A                                                            
         MVC   WORK(7),=C'BYTENN='                                              
         GOTO1 GETFLTR,DMCB,(64,(R5)),(4,=C'BYTE')                              
         OC    4(4,R1),4(R1)                                                    
         BZ    POSEX                                                            
POS1A    L     R5,4(R1)            SET FOR NEXT SCAN                            
         LR    R6,R5               POINT TO FIELD                               
         LA    RE,8                SET FIELD LENGTH FOR ERROR                   
         LA    R5,1(R5)                                                         
         MVC   HALF,=C'00'                                                      
         CLI   5(R6),C'='                                                       
         BNE   *+14                                                             
         MVC   HALF+1(1),4(R6)                                                  
         B     POS2                                                             
         CLI   6(R6),C'='                                                       
         BNE   FLTERR              ERROR                                        
         MVC   HALF,4(R6)                                                       
*                                                                               
POS2     CLI   HALF,C'0'           EDIT FIELD NUMBER                            
         BL    FLTERR                                                           
         CLI   HALF,C'9'                                                        
         BH    FLTERR                                                           
         CLI   HALF+1,C'0'                                                      
         BL    FLTERR                                                           
         CLI   HALF+1,C'9'                                                      
         BH    FLTERR                                                           
         PACK  DUB,HALF                                                         
         CVB   R9,DUB                                                           
         LTR   R9,R9                                                            
         BZ    FLTERR                                                           
         CHI   R9,32                                                            
         BH    FLTERR                                                           
         BCTR  R9,0                                                             
         LA    R9,CPROFLT(R9)                                                   
POS3     CLI   0(R6),C'='                                                       
         BE    *+12                                                             
         LA    R6,1(R6)                                                         
         B     POS3                                                             
         LA    R6,1(R6)                                                         
POS4     MVC   0(1,R9),0(R6)                                                    
         LA    R9,1(R9)                                                         
         LA    R6,1(R6)                                                         
         CLI   0(R6),0                                                          
         BE    POSEX                                                            
         CLI   0(R6),C','                                                       
         BE    POS1                                                             
         B     POS4                                                             
         EJECT                                                                  
*                                                                               
* FILTERS HAVE BEEN SET NOW PROCESS RECORDS                                     
*                                                                               
POSEX    XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING CLTHDRD,R5                                                       
         MVC   PCLTKAGY(3),SVAGY       AGY/MED                                  
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,SVCLT                                                   
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
         LA    R4,14               SET MAX LINES                                
PFHIGH   BAS   RE,HIGH                                                          
         B     PFHAVREC                                                         
PFSEQ    BAS   RE,SEQ                                                           
*                                                                               
PFHAVREC LA    R5,KEY                                                           
         CLC   PCLTKAGY(3),SVAGY   CHK SAME AGY/MED                             
         BNE   PFEND                                                            
         CLI   PCLTKRCD,X'02'                                                   
         BNE   PFEND                                                            
         BAS   RE,GETREC                                                        
         L     R5,AREC                                                          
*                                                                               
         BRAS  RE,TSTCOFCF         TEST CLT OFFICE CODE FILTER                  
         BNE   PFSEQ                                                            
*                                                                               
         CLI   CGRPFLT,0                                                        
         BE    PFHAVR8                                                          
         CLC   PCLTBLGP,CGRPFLT                                                 
         BNE   PFSEQ                                                            
*                                                                               
PFHAVR8  CLI   CFINFLT,0                                                        
         BE    PFHAVR9                                                          
         CLI   CFINFLT,C'Y'                                                     
         BNE   PFHAVR8C                                                         
         CLI   PCLTFIN,C'Y'                                                     
         BNE   PFSEQ               ONLY FINANCIAL CLTS                          
         B     PFHAVR9                                                          
*                                                                               
PFHAVR8C CLI   CFINFLT,C'N'        ONLY NON-FINANCIAL CLTS                      
         BNE   PFHAVR9                                                          
         CLI   PCLTFIN,0                                                        
         BNE   PFSEQ                                                            
*                                                                               
PFHAVR9  CLI   CSFHFLT,0                                                        
         BE    PFHAVRA                                                          
         CLI   CSFHFLT,C'Y'        ONLY SFH CLIENTS ?                           
         BNE   PFHAVR9C            NO                                           
         TM    PCLTSTAT,X'01'      SFH CLIENT ?                                 
         BNO   PFSEQ               NO - SKIP                                    
         B     PFHAVRA                                                          
*                                                                               
PFHAVR9C CLI   CSFHFLT,C'N'        ONLY NON-SFH CLTS ?                          
         BNE   PFHAVRA             NO                                           
         TM    PCLTSTAT,X'01'      SFH CLIENT ?                                 
         BO    PFSEQ               YES - SKIP                                   
*                                                                               
PFHAVRA  CLI   CFRZFLT,C'Y'        ONLY FROZEN CLTS ?                           
         BNE   PFHAVRB             NO                                           
         TM    PCLTSTAT,X'02'      FROZEN CLIENT ?                              
         BNO   PFSEQ               NO - SKIP                                    
*                                                                               
PFHAVRB  CLI   CFRZFLT,C'N'        NO FROZEN CLTS ?                             
         BNE   PFHAVRC             NO                                           
         TM    PCLTSTAT,X'02'      FROZEN CLIENT ?                              
         BO    PFSEQ               YES- SKIP                                    
*                                                                               
PFHAVRC  CLI   CACCOFF,0           ACC OFFICE FILTER                            
         BE    PFHAVRF                                                          
         CLC   PCLTAOFC,CACCOFF                                                 
         BNE   PFSEQ                                                            
*                                                                               
PFHAVRF  DS    0H                                                               
         OC    CPROFLT,CPROFLT                                                  
         BZ    PFHAVRG                                                          
         LA    RF,PCLTPROF                                                      
         LA    RE,CPROFLT                                                       
         LA    R9,32                                                            
PFFLTR   CLI   0(RE),0                                                          
         BE    PFFLTR1                                                          
         CLC   0(1,RF),0(RE)                                                    
         BNE   PFSEQ                                                            
PFFLTR1  LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R9,PFFLTR                                                        
         EJECT                                                                  
*                                                                               
PFHAVRG  CLI   CCOS2FLT,0          COST2 FACTOR FILTER                          
         BE    PFHAVRH                                                          
         CLI   CCOS2FLT,C'Y'                                                    
         BNE   PFHAVRGC                                                         
         TM    PCLTSTAT,X'04'      COST2 $?                                     
         BO    PFHAVRH                                                          
         TM    PCLTSTAT,X'08'      COST2 FACTOR?                                
         BO    PFHAVRH                                                          
         B     PFSEQ               COST2 IS NOT PRESEMT, GET NEXT REC           
*                                                                               
PFHAVRGC CLI   CCOS2FLT,C'N'       COST2 IS A NEGATIVE FILTER?                  
         BNE   PFHAVRH                                                          
         TM    PCLTSTAT,X'04'      COST2 $ (COS2=Y)                             
         BO    PFSEQ                                                            
         TM    PCLTSTAT,X'08'      COST2 FACTOR (COS2=9.999999)                 
         BO    PFSEQ                                                            
*                                                                               
*                                                                               
*                                                                               
PFHAVRH  DS    0X                  IF NEW DATA OPTS NEED TO BE ADDED            
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
* RECORD HAS PASSED ALL FILTERS SO DISPLAY IT                                   
*                                                                               
PFSEND   CLI   CDFRST,1                                                         
         BNE   PFSEND4                                                          
*                                                                               
         MVI   WESTAGY,0            TURN WESTERN AGENCY INDICATOR OFF           
         CLC   SVAGY(2),=C'WI'     "WESTERN" AGENCY ?                           
         BE    PFSEND2              YES                                         
         CLC   SVAGY(2),=C'WJ'     "WESTERN" AGENCY ?                           
         BE    PFSEND2              YES                                         
         CLC   SVAGY(2),=C'WT'     "WESTERN" AGENCY ?                           
         BE    PFSEND2              YES                                         
         CLC   SVAGY(2),=C'SJ'      OR DDS TEST SJ ?                            
         BNE   PFSEND2X             NO                                          
PFSEND2  DS    0H                                                               
         MVI   WESTAGY,1           TURN WESTERN AGENCY INDICATOR ON             
PFSEND2X DS    0H                                                               
*                                                                               
         LA    R2,SINHDRH                                                       
         MVC   FLDDATA+1(16),=C'CLIENT CODE/NAME'                               
         CLI   CFSCRSW,C'Y'                                                     
         BNE   *+14                                                             
         MVC   FLDDATA+28(06),=C'COST 2'                                        
         B     *+10                                                             
         MVC   FLDDATA+28(14),=C'CLIENT PROFILE'                                
*                                                                               
         MVI   FLDDATA+63,C'O'                                                  
         MVI   FLDDATA+66,C'A'                                                  
         MVI   FLDDATA+67,C'O'                                                  
         MVI   FLDDATA+72,C'G'                                                  
         MVI   FLDDATA+74,C'F'                                                  
         MVI   FLDDATA+76,C'S'                                                  
*                                                                               
*****    CLI   WESTAGY,1           WESTERN AGENCY ?                             
*****    BNE   *+8                 NO                                           
         MVI   FLDDATA+78,C'F'                                                  
*                                                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(16),=C'----------------'                               
*                                                                               
         CLI   CFSCRSW,C'Y'                                                     
         BNE   *+14                                                             
         MVC   FLDDATA+28(06),FLDDATA+1                                         
         B     *+10                                                             
         MVC   FLDDATA+28(14),FLDDATA+1                                         
*                                                                               
         MVI   FLDDATA+63,C'F'                                                  
         MVI   FLDDATA+66,C'C'                                                  
         MVI   FLDDATA+67,C'F'                                                  
         MVI   FLDDATA+72,C'R'                                                  
         MVI   FLDDATA+74,C'I'                                                  
         MVI   FLDDATA+76,C'F'                                                  
*                                                                               
*****    CLI   WESTAGY,1           WESTERN AGENCY ?                             
*****    BNE   *+8                 NO                                           
         MVI   FLDDATA+78,C'R'                                                  
*                                                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
*                                                                               
         CLI   CFSCRSW,C'Y'                                                     
         BE    *+22                                                             
         MVC   FLDDATA+28(4),=C'1-10'                                           
         MVC   FLDDATA+39(5),=C'11-20'                                          
         MVC   FLDDATA+50(5),=C'21-32'                                          
*                                                                               
         MVI   FLDDATA+63,C'F'                                                  
         MVI   FLDDATA+66,C'C'                                                  
         MVI   FLDDATA+67,C'F'                                                  
         MVI   FLDDATA+72,C'P'                                                  
         MVI   FLDDATA+74,C'N'                                                  
         MVI   FLDDATA+76,C'H'                                                  
*                                                                               
*****    CLI   WESTAGY,1           WESTERN AGENCY ?                             
*****    BNE   *+8                 NO                                           
         MVI   FLDDATA+78,C'Z'                                                  
*                                                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVI   CDFRST,0                                                         
*                                                                               
PFSEND4  DS    0H                                                               
         MVC   FLDDATA+1(3),PCLTKCLT                                            
         MVI   FLDDATA+4,C'/'                                                   
         MVC   FLDDATA+5(20),PCLTNAME                                           
*                                                                               
         CLI   CFSCRSW,C'Y'                                                     
         BE    *+22                                                             
         MVC   FLDDATA+28(10),PCLTPROF                                          
         MVC   FLDDATA+39(10),PCLTPROF+10                                       
         MVC   FLDDATA+50(12),PCLTPROF+20                                       
*                                                                               
         MVC   FLDDATA+63(1),PCLTOFF                                            
         L     R0,=V(HEXOUT)                                                    
         A     R0,RELO                                                          
         GOTO1 =V(OFFOUT),DMCB,PCLTOFF,(R0),(C'L',FLDDATA+64),RR=RELO           
         CLI   FLDDATA+65,C' '     BLANK IN POSITION 2 ?                        
         BNE   *+8                 NO                                           
         MVI   FLDDATA+64,C' '     YES - CLEAR POSITION 1                       
         MVC   FLDDATA+66(2),PCLTAOFC                                           
         OC    PCLTACCA,PCLTACCA      SEE IF I HAVE AN AGENCY                   
         BZ    PFSEND6                                                          
         MVI   FLDDATA+68,C'/'                                                  
         MVC   FLDDATA+69(2),PCLTACCA                                           
*                                                                               
PFSEND6  DS    0H                                                               
         MVC   FLDDATA+72(1),PCLTBLGP                                           
         MVC   FLDDATA+74(1),PCLTFIN                                            
         MVI   FLDDATA+76,C' '     CLEAR SFH FIELD                              
         TM    PCLTSTAT,X'01'      "SFH" CLIENT ?                               
         BZ    *+8                 NO                                           
         MVI   FLDDATA+76,C'Y'     YES                                          
         MVI   FLDDATA+78,C' '     CLEAR FRZ FIELD                              
         TM    PCLTSTAT,X'02'      "FROZEN" CLIENT ?                            
         BZ    *+8                 NO                                           
         MVI   FLDDATA+78,C'Y'     YES                                          
*                                                                               
*                                                                               
*                                                                               
         CLI   CFSCRSW,C'Y'                                                     
         BNE   PFSENDXX                                                         
         TM    PCLTSTAT,X'04'                                                   
         BZ    PFSEND7                                                          
         TM    PCLTSTAT,X'08'                                                   
         BZ    *+6                                                              
         DC    H'0'                EITHER X'04' OR X'08' (NOT BOTH)             
*                                                                               
         MVC   FLDDATA+28(3),=C'YES'                                            
         B     PFSENDXX                                                         
PFSEND7  TM    PCLTSTAT,X'08'                                                   
         BZ    PFSENDXX            IS NOT COST2                                 
         TM    PCLTSTAT,X'04'                                                   
         BZ    *+6                                                              
         DC    H'0'                EITHER X'04' OR X'08' (NOT BOTH)             
         L     R9,AREC                                                          
         LA    R9,33(R9)           POINT TO ELEM(S)                             
         MVI   ELCODE,X'45'                                                     
         BAS   RE,NEXTEL           SEARCH FOR COST2 FACTOR ELEM                 
         BE    *+6                                                              
         DC    H'0'                ELEM MUST EXIST                              
         USING PCLTCFEL,R9                                                      
         OC    PCLTCF,PCLTCF                                                    
         BZ    PFSEND8                                                          
*                                                                               
         EDIT  (P5,PCLTCF),(8,FLDDATA+28),6,ALIGN=LEFT,FILL=0,DROP=5            
*                                                                               
         DROP  R9                                                               
PFSEND8  MVI   ELCODE,X'45'                                                     
         BAS   RE,NEXTEL           SEARCH FOR COST2 FACTOR ELEM AGAIN           
         BNE   PFSENDXX                                                         
         DC    H'0'                THERE'S ONLY ONE COST2 ELEM!                 
*                                                                               
*                                                                               
*                                                                               
PFSENDXX FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         BCT   R4,PFSEQ                                                         
         MVC   PREVKEY,KEY                                                      
         B     MODEXIT                                                          
PFEND    XC    PREVKEY,PREVKEY                                                  
         B     MODEXIT                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R9)                                                         
         AR    R9,R0               FIRST ELEM IS ALWAYS X'20'                   
         CLC   ELCODE,0(R9)                                                     
         JE    NEXTELX             CC IS EQUAL                                  
         CLI   0(R9),0                                                          
         JNE   NEXTEL                                                           
         LTR   R9,R9               CC IS NOT EQUAL                              
NEXTELX  BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
GETALPH  SR    R5,R5                                                            
GETAL1   CLI   0(R4),C','                                                       
         BER   R9                                                               
         CLI   0(R4),0                                                          
         BER   R9                  END OF INPUT                                 
         CLI   0(R4),C' '                                                       
         BER   R9                                                               
         CLI   0(R4),C'-'          ACCEPT -                                     
         BE    GETAL4                                                           
         CLI   0(R4),C'A'                                                       
         BL    GETAERR                                                          
         CLI   0(R4),C'9'                                                       
         BH    GETAERR                                                          
GETAL4   LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     GETAL1                                                           
*                                                                               
*                                                                               
GETAERR  SR    R5,R5                                                            
         BR    R9                                                               
*                                                                               
*                                                                               
*                                                                               
DATAERR  DS    0H                  DATA= ERROR                                  
         LA    RE,9                                                             
         B     FLTERR                                                           
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)                                                  
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(22,RF),=C'- INVALID FILTER FIELD'                              
         LA    RF,23(RF)                                                        
         MVC   0(20,RF),WORK                                                    
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,SINIFLTH                                                      
         B     MODX                                                             
MODEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    MODX                                                             
         LA    R2,SINENDH                                                       
         MVI   PREVKEY+7,X'FF'                                                  
MODX     OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPACCTEST                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPGENEROL                                                      
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         LA    RE,TMPREC                                                        
         LA    RF,400*4                                                         
         XCEFL                                                                  
*                                                                               
         LA    R4,TMPREC                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDER                                                         
         DROP  R4,RE                                                            
*                                                                               
CKTRIDX  J     SETCCEQ             EQUAL                                        
*                                                                               
CKTRIDER J     SETCCNEQ            NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
EXIT_X   XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRAFFACC NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC LIMIT ACCESS            
*                                                                               
         MVI   BYTE3,0             FOR RETURN CODE                              
*                                                                               
         L     R9,AREC             POINT TO CLIENT RECORD                       
         LA    R9,33(R9)                                                        
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         BRAS  RE,NEXTEL                                                        
         BNE   TRACCX              NO CLIENT TRAFFIC OFFICE CODE ELEM           
         MVC   BYTE3,2(R6)         SAVE CLIENT TRAFFIC OFFICE CODE              
*                                                                               
TRACCX   J     EXIT_X                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FLT_COFC NTR1  BASE=*,LABEL=*      FILTER - CLIENT OFFICE CODE                  
*                                                                               
         XC    COFFFLT1,COFFFLT1   INIT OFFICE FILTER FIELDS                    
         XC    COFFFLT2,COFFFLT2                                                
         MVI   COFFCIND,0                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(21),=C'OFFICE=XX, -XX, XX-YY'                               
*                                                                               
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(7,=C'OFFICE=')                        
         OC    4(4,R1),4(R1)                                                    
         BZ    F_COFC_X                                                         
*                                                                               
         L     R4,4(R1)            POINT TO FILTER FIELD DATA                   
         ST    R4,AFLDDATA         SAVE ADDRESS OF FILTER FIELD DATA            
         LA    R4,7(R4)            BYPASS FILTER KEYWORD OVERHEAD               
         LR    RE,R4                                                            
         SR    RF,RF               C'-' COUNTER                                 
         SR    R1,R1               TOTAL CHARS COUNTER                          
*                                                                               
F_COFC24 CLI   0(RE),C','          BEGINNING OF NEXT FILTER KEYWORD?            
         BE    F_COFC32                                                         
         CLI   0(RE),0             END OF FILTER FIELD INPUT?                   
         BE    F_COFC32                                                         
         CLI   0(RE),C' '          END OF FILTER FIELD INPUT?                   
         BE    F_COFC32                                                         
         CLI   0(RE),C'-'          C'-' CHARACTER?                              
         BNE   *+8                                                              
         AHI   RF,1                C'-' COUNTER INCREMENT                       
         AHI   R1,1                TOTAL CHARS COUNTER INCREMENT                
         LA    RE,1(RE)            POINT TO NEXT CHAR IN INPUT                  
         B     F_COFC24                                                         
*                                                                               
F_COFC32 CHI   R1,0                NO FILTER VALUE IS ENTERED?                  
         BNH   F_COFCER                                                         
         CHI   RF,1                ONLY ONE C'-' IN FILTER INPUT?               
         BH    F_COFCER                                                         
         CHI   R1,5                FILTER INPUT > XX-YY?                        
         BH    F_COFCER                                                         
         CHI   RF,0                C'-' IS NOT IN FILTER INPUT?                 
         BNE   F_COFC34                                                         
         CHI   R1,2                                                             
         BH    F_COFCER            IF NO C'-', ONLY XX IS ALLOWED               
         OI    COFFCIND,COFC_XXQ   SET ONLY OFFICE CODE FILTERING BIT           
         SHI   R1,1                MINUS ONE FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   COFFFLT1(0),0(R4)   SET FILTERING VALUE                          
         B     F_COFC_X                                                         
*                                                                               
F_COFC34 CLI   0(R4),C'-'          FIRST CHAR IS C'-'?                          
         BNE   F_COFC36                                                         
         CHI   R1,3                                                             
         BH    F_COFCER            MAX INPUT ALLOWED IS -XX                     
         OI    COFFCIND,COFC_NEQ   SET NEGATIVE FILTERING BIT                   
         SHI   R1,(1+1)            MINUS ONE FOR C'-' AND ONE FOR EX            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   COFFFLT1(0),1(R4)   SET FILTERING VALUE                          
         B     F_COFC_X                                                         
*                                                                               
F_COFC36 CLI   1(R4),C'-'          2ND CHAR IS C'-'?                            
         BNE   F_COFC42                                                         
         CHI   R1,4                                                             
         BH    F_COFCER            MAX INPUT ALLOWED IS X-YY                    
         CHI   R1,2                                                             
         BNH   F_COFCER            CANNOT JUST BE X-                            
         MVC   COFFFLT1(1),0(R4)                                                
         SHI   R1,(2+1)            TWO FOR X- AND ONE FOR EX                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   COFFFLT2(0),2(R4)                                                
         OI    COFFCIND,COFC_RGQ   SET RANGE FILTERING BIT                      
         B     F_COFC_X                                                         
*                                                                               
F_COFC42 CLI   2(R4),C'-'          3RD CHAR IS C'-'?                            
         BNE   F_COFCER                                                         
         CHI   R1,3                                                             
         BNH   F_COFCER            CANNOT JUST BE XX-                           
         MVC   COFFFLT1,0(R4)                                                   
         SHI   R1,(3+1)            THREE FOR XX- AND ONE FOR EX                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   COFFFLT2(0),3(R4)                                                
         OI    COFFCIND,COFC_RGQ   SET RANGE FILTERING BIT                      
*                                                                               
F_COFC_X J     SETCCEQ                                                          
*                                                                               
F_COFCER J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TSTCOFCF NTR1  BASE=*,LABEL=*      TEST CLT OFFICE CODE FILTER                  
*                                                                               
         OC    COFFFLT1,COFFFLT1   CLT OFFICE CODE FILTER PRESENT?              
         BZ    T_COFC_X                                                         
*                                                                               
         OI    COFFFLT1+1,C' '     SPACE PAD 2ND CHAR                           
         OI    COFFFLT2+1,C' '     SPACE PAD 2ND CHAR                           
         TM    COFFCIND,COFC_NEQ   NEGATIVE FILTERING?                          
         BZ    T_COFC24                                                         
         BRAS  RE,TRNSLOFC         TRANSLATE OFFICE CODE                        
         CLC   OFCOFC2,COFFFLT1                                                 
         BNE   T_COFC_X                                                         
         B     T_COFCER                                                         
*                                                                               
T_COFC24 TM    COFFCIND,COFC_XXQ   OFFICE CODE FILTERING?                       
         BZ    T_COFC34                                                         
         BRAS  RE,TRNSLOFC         TRANSLATE OFFICE CODE                        
         CLC   OFCOFC2,COFFFLT1                                                 
         BNE   T_COFCER                                                         
         B     T_COFC_X                                                         
*                                                                               
T_COFC34 TM    COFFCIND,COFC_RGQ   RANGE FILTERING?                             
         BNZ   *+6                                                              
         DC    H'0'                INVALID CLT OFFICE MEDIA FILTER              
*                                                                               
         BRAS  RE,TRNSLOFC         TRANSLATE OFFICE CODE                        
         CLC   OFCOFC2,COFFFLT1                                                 
         BL    T_COFCER                                                         
         CLC   OFCOFC2,COFFFLT2                                                 
         BH    T_COFCER                                                         
*                                                                               
T_COFC_X J     SETCCEQ                                                          
*                                                                               
T_COFCER J     SETCCNEQ                                                         
*                                                                               
TRNSLOFC ST    RE,SV_REG_E         TRANSLATE OFFICE CODE                        
         XC    OFCBLK,OFCBLK       INIT OFFICER BLOCK                           
         MVI   OFCSYS,C'P'         PRINT SYSTEM                                 
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF                                                   
         GOTOR OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         CLI   0(R1),0                                                          
         JNE   *+8                                                              
         OI    OFCOFC2+1,C' '                                                   
         L     RE,SV_REG_E                                                      
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLTWRKD  DSECT                                                                  
RELO     DS    F                                                                
OFFICER  DS    A                   ADDRESS OF OFFICER                           
AFLDDATA DS    A                   SAVE ADDRESS OF FILTER FIELD DATA            
SV_REG_E DS    F                                                                
*                                                                               
CDFRST   DS    CL1                                                              
WESTAGY  DS    CL1                 1=WESTERN AGENCY                             
*                                                                               
COFFFLT1 DS    CL(L'OFCOFC2)       CLT OFFICE CODE FILTER START                 
COFFFLT2 DS    CL(L'OFCOFC2)       CLT OFFICE CODE FILTER END                   
COFFCIND DS    X                   CLT OFFICE CODE FILTER INDICATOR             
COFC_XXQ EQU   X'80'               CLT OFFICE CODE FILTERING                    
COFC_NEQ EQU   X'40'               CLT OFFICE CODE NEGATIVE FILTERING           
COFC_RGQ EQU   X'20'               CLT OFFICE CODE RANGE FILTERING              
*                                                                               
CGRPFLT  DS    CL1                 GROUP FILTER                                 
CFINFLT  DS    CL1                 FINANCIAL FILTER                             
CCOS2FLT DS    CL1                 COST2 FACTOR FILTER                          
CACCOFF  DS    CL2                 ACC OFFICE FILTER                            
CSFHFLT  DS    CL1                 SFH FILTER (Y,N)                             
CFRZFLT  DS    CL1                 FROZEN CLIENT ONLY INDICATOR                 
CPROFLT  DS    CL32                                                             
CFSCRSW  DS    XL1                 COST2 FACTOR DISPLAY SCREEN                  
ELCODE   DS    XL1                 ELEMENT CODE FOR NEXTEL                      
*                                                                               
OFCBLK   DS    XL(OFCLENQ)         OFFICER BLOCK                                
*                                                                               
TMPREC   DS    400F                                                             
*                                                                               
CLTWRKX  EQU   *                                                                
*                                                                               
LINLEN   EQU   88                                                               
INVERR   EQU   2                                                                
CLTNFND  EQU   40                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSINFOWRK                                                     
         EJECT                                                                  
*                                                                               
