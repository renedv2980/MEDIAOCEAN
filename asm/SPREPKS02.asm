*          DATA SET SPREPKS02  AT LEVEL 175 AS OF 05/02/19                      
***********************************************************************         
* SUPPORT FOR ENTRAVISION                                                       
***********************************************************************         
*PHASE SPKS02A                                                                  
*INCLUDE PRTREC                                                                 
SPKS02W  TITLE 'SPKS02 - SALESPERSON EXTRACTION'                                
         SPACE 1                                                                
SPKS02   CSECT                                                                  
         DS    6000C                                                            
         ORG   SPKS02                                                           
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPKS02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPKS02,RB,RC                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAIN00                                                           
*                                                                               
YES      CR    RB,RB               SET CC EQUAL                                 
         B     XIT                                                              
*                                                                               
NO       LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
* REGARDLESS OF DATES                                                           
***********************************************************************         
MAIN00   DS    0H                                                               
         LHI   R6,IO-SPKS02                                                     
         AR    R6,RB               ALWAYS KILLS ME IN DADDS                     
         ST    R6,AREC             IF I FORGET THIS                             
*                                                                               
         OPEN  (TOCONCAT,OUTPUT)                                                
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         MVC   SAVEREP,=C'K3'      FIRST REP TO PROCESS IS KATZ                 
*                                                                               
         DO  WHILE=(CLC,SAVEREP,GT,=C'  ')                                      
            MVC  SAVCOMPC,=X'FFFF'   WE DO NOT HAVE A COMPANY YET               
*                                                                               
            SELECT  CLC,SAVEREP,EQ                                              
               WHEN  (=C'K3')          A KATZ REP COMPANY?                      
                  LA  R4,SALEINFO                                               
               WHEN  (=C'IR')          A MFS REP COMPANY?                       
                  LA  R4,SALEINF2                                               
               WHEN  (=C'GR')          A REGIONAL REP COMPANY?                  
                  LA  R4,SALEINF3                                               
               WHEN  (=C'NM')          A NATIONAL PUBLIC MEDIA COMPANY?         
                  LA  R4,SALEINF4                                               
               WHEN  (=C'EN')          A ENTRAVISION COMPANY?                   
                  LA  R4,SALEINF5                                               
               OTHRWISE                                                         
                  ASMLEAVE ,                                                    
            ENDSEL                                                              
*                                                                               
            OPEN  ((R4),INPUT)                                                  
            LTR   RF,RF                                                         
            JNZ   *+2                                                           
            ZAP   INPCOUNT,=P'0'      NO SALESPEOPLE YET FOR THIS FILE          
            GET   (R4),IO4            SKIP 1ST LINE (COLUMN HEADINGS)           
*                                                                               
            BAS   RE,MYSUB                                                      
**********************************************************************          
* THIS SECTION OF CODE RUNS WHEN A EOF CONDITION HAPPENS                        
**********************************************************************          
NOMORINP    DS    0H                      EOF HOOK                              
*                                                                               
            LA    RE,IO                                                         
            XR    RF,RF                                                         
            IF  (ICM,RF,3,GSPLFLEN-GSPLRECD(RE),NZ)                             
              LA    RF,4(RF)            SET QSAM LENGTH                         
              STCM  RF,3,IOQSAM                                                 
              PUT   TOCONCAT,IOQSAM                                             
            ENDIF                                                               
            XC    SAVSPCOD,SAVSPCOD   SAVED OFF LAST SALESPERSON                
*                                                                               
            IF  (CLC,SAVCOMPC,NE,=X'FFFF')  ANY COMPANY TO SAVE OFF?            
              BAS   RE,MAKESUB       YES, GOT LAST SUBSIDIARY TO CREATE         
            ENDIF                                                               
*                                                                               
            SELECT  CLC,SAVEREP,EQ                                              
              WHEN  (=C'K3')          A KATZ REP COMPANY?                       
                IF  (CP,INPCOUNT,NH,=P'70000')  AS OF APR05/13,                 
                  BRAS  RE,CKINPFLS             KATZ HAD 83650                  
                ENDIF                                                           
*                                                                               
                CLOSE SALEINFO                                                  
                LTR   RF,RF                                                     
                JNZ   *+2                                                       
                MVC   SAVEREP,=C'IR'  AFTER KATZ WE WANT MFS                    
*                                                                               
              WHEN  (=C'IR')          A MFS REP COMPANY?                        
                IF  (CP,INPCOUNT,NH,=P'450')  AS OF APR05/13,                   
                  BRAS  RE,CKINPFLS           MFS HAD 596                       
                ENDIF                                                           
*                                                                               
                CLOSE SALEINF2                                                  
                LTR   RF,RF                                                     
                JNZ   *+2                                                       
                MVC   SAVEREP,=C'GR'  AFTER MFS WE WANT REGIONAL                
*                                                                               
              WHEN  (=C'GR')          A REGIONAL REP COMPANY?                   
*&&DO                                   REGIONAL HAS NO MORE SALESPPL           
                IF  (CP,INPCOUNT,NH,=P'000')   AS OF APR05/13,                  
                  BRAS  RE,CKINPFLS            RRP HAD 146                      
                ENDIF                                                           
*&&                                                                             
                CLOSE SALEINF3                                                  
                LTR   RF,RF                                                     
                JNZ   *+2                                                       
                MVC   SAVEREP,=C'NM'  AFTER REGIONAL WE WANT NPM                
                                                                                
              WHEN  (=C'NM')          A NATIONAL PUBLIC MEDIA COMPANY?          
                IF  (CP,INPCOUNT,NH,=P'5')    AS OF JUL19/15,                   
                  BRAS  RE,CKINPFLS           NPM HAD 8                         
                ENDIF                                                           
                                                                                
                CLOSE SALEINF4                                                  
                LTR   RF,RF                                                     
                JNZ   *+2                                                       
*                                                                               
                MVC   SAVEREP,=C'EN'  WE'RE DONE, NO MORE COMPANIES             
              WHEN  (=C'EN')          A ENTRAVISION COMPANY?                    
                IF  (CP,INPCOUNT,NH,=P'1')    AS OF MAR06/19,                   
                  BRAS  RE,CKINPFLS           NPM HAD 8                         
                ENDIF                                                           
                                                                                
                CLOSE SALEINF5                                                  
                LTR   RF,RF                                                     
                JNZ   *+2                                                       
*                                                                               
                MVC   SAVEREP,=C'  '  WE'RE DONE, NO MORE COMPANIES             
            ENDSEL                                                              
         ENDDO ,                                                                
***********************************                                             
* DONE WITH ALL THE INPUT FILES                                                 
***********************************                                             
         TM    MISCFLG1,MF1NOTFY   NOTIFY US OF A PROBLEM??                     
         BZ    MAINX                                                            
*                                                                               
         CLI   QOPT4,C'Y'          DO NOT SEND EMAILS?                          
         BE    MAINX               THAT'S RIGHT, DO NOT                         
         L     R3,ADCONLST                                                      
         USING SPADCONS,R3                                                      
         GOTOR VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
EMAIL00  GOTOR VSMTP,DMCB,('SMTPATCS',SMTPTO),(L'SUBJECT,SUBJECT),     +        
               (0,SMTPCC),(0,SMTPBCC)                                           
*                                                                               
         XC    P,P                                                              
         MVC   P(L'LINE1),LINE1                                                 
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
*                                                                               
         XC    P,P                                                              
         MVC   P(L'LINE2),LINE2                                                 
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
         DROP  R3                                                               
*                                                                               
EMAILX   B     MAINX                                                            
*                                                                               
JESMAIL  DC    CL8'JESMAIL '                                                    
SMTPTO   DC    CL24'EDIADMIN@MEDIAOCEAN.COM:'                                   
SMTPCC   DC    0CL69                                                            
         DC    CL45'WALTERHO@MEDIAOCEAN.COM,HWONG@MEDIAOCEAN.COM,'              
         DC    CL24'CCOLLINS@MEDIAOCEAN.COM:'                                   
SMTPBCC  DC    CL40'WACKYWALT@GMAIL.COM,HENEYWONG@GMAIL.COM:'                   
SUBJECT  DC    C'BAD SALESPERSON UPLOAD'                                        
LINE1    DC    C'CHECK OUTPUT IN AFREKS'                                        
LINE2    DC    C'LOOK FOR ***'                                                  
*                                                                               
MAINX    GOTO1 AENDREQ                                                          
         EJECT                                                                  
***********************************************************************         
* EMAIL PEOPLE TO CHECK THE RADIO "SALES" FILES AS THE COUNT IS LOW             
*    DDS.KATZ.SALES      96552 AS OF APR15/19                                   
*    DDS.IRKATZ.SALES     1610 AS OF APR15/19                                   
*    DDS.RSKATZ.SALES        0 AS OF APR15/19                                   
*    DDS.NMKATZ.SALES       30 AS OF APR15/19                                   
*    DDS.ENKATZ.SALES      ??? AS OF APR15/19                                   
***********************************************************************         
CKINPFLS NTR1                                                                   
         L     R3,ADCONLST                                                      
         USING SPADCONS,R3                                                      
*                                                                               
* OPEN ENDED CONDITION, DO NOT ENCLOSE WITHIN PARENTHESIS                       
         SELECT  CLC,SAVEREP,EQ                                                 
           WHEN  (=C'K3')                                                       
             MVC CKSBJCT(2),=C'  '      KATZ SALESPERSON COUNT IS LOW           
           WHEN  (=C'GR')                                                       
             MVC CKSBJCT(2),=C'RS'      REGIONAL SALES COUNT IS LOW             
           OTHRWISE                                                             
             MVC CKSBJCT(2),SAVEREP     OTHER FILES COUNT IS LOW                
         ENDSEL ,                                                               
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPAINI',CKJESMAIL)                                
         GOTOR VSMTP,DMCB,('SMTPATCS',CKSMTPTO),(L'CKSBJCT,CKSBJCT),   +        
               (0,CKSMTPCC),(0,CKSMTPBC)                                        
*                                                                               
         XC    P,P                                                              
         MVC   P(L'CKLINE1),CKLINE1                                             
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
*                                                                               
         XC    P,P                                                              
         MVC   P(L'CKLINE2),CKLINE2                                             
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
         DROP  R3                                                               
*                                                                               
CKIFX    J     YES                                                              
*                                                                               
CKJESMAIL DC    CL8'JESMAIL '                                                   
CKSMTPTO DC    CL24'INT_EBIZ@MEDIAOCEAN.COM:'                                   
CKSMTPCC DC    0C                                                               
         DC    CL45'WALTERHO@MEDIAOCEAN.COM,HWONG@MEDIAOCEAN.COM,'              
         DC    CL24'CCOLLINS@MEDIAOCEAN.COM:'                                   
CKSMTPBC DC    CL40'WACKYWALT@GMAIL.COM,HENEYWONG@GMAIL.COM:'                   
CKSBJCT  DC    C'??KATZ SALESPERSON COUNT IS LOW'                               
CKLINE1  DC    C'CHECK =3.4 FOR'                                                
CKLINE2  DC    C'   DDS.*KATZ.SALES'                                            
         EJECT                                                                  
***********************************************************************         
* MYSUB SUBROUTINE                                                              
***********************************************************************         
MYSUB    ST    RE,ADDSAVE                                                       
         L     R2,ACOMFACS                                                      
         USING COMFACSD,R2                                                      
*****    MVC   ASCANNER,CSCANNER-COMFACSD(R2)                                   
         MVC   APARSNIP,CPARSNIP-COMFACSD(R2)                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NGENDIR NGENFIL X',DMWORK,0                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    OFFLSTEL,OFFLSTEL   SET UP OFFICE LIST ELEM FOR SUB REC          
OFFLSTD  USING GSPLOFFL,OFFLSTEL                                                
         MVI   OFFLSTD.GSPLOFFL,X'02'              ELEMENT CODE                 
         MVI   OFFLSTD.GSPLOFLN,GSPLOFFN-GSPLOFFL                               
*****************************************                                       
*    R3 IS ALWAYS POINTING TO SBSTAB                                            
*    R5 IS ALWAYS POINTING TO SLSINFO                                           
*    R6 POINTS TO IO WHICH HAS THE RECORD BEING BUILT                           
*****************************************                                       
MYSUB100 DS    0H                                                               
         BAS   RE,GTSLSINF         GET SALES INFO INTO IO2:SLSINFO              
         BNE   MYSUB100                                                         
*                                                                               
         LA    R5,IO2                                                           
         USING SLSINFO,R5                                                       
*                                                                               
         CLC   SAVCOMPC,=X'FFFF'   DID WE HAVE A COMPANY ALREADY?               
         BE    MYSUB110            NO                                           
         CLC   SAVCOMPC,SLSCOMPC   DID WE GET A NEW COMPANY CODE?               
         BE    MYSUB110                                                         
*                                                                               
         LA    RE,IO               YES, PUT OUT LAST SALESPERSON FOR            
         XR    RF,RF                                                            
         ICM   RF,3,GSPLFLEN-GSPLRECD(RE)                                       
         LA    RF,4(RF)            SET QSAM LENGTH                              
         STCM  RF,3,IOQSAM                                                      
         PUT   TOCONCAT,IOQSAM                                                  
*                                                                               
         BAS   RE,MAKESUB          YES, CREATE SUB (X'00007100') REC            
*                                                                               
         XC    SAVSPCOD,SAVSPCOD   NEW COMPANY, NEW SALESPERSON                 
         XC    OFFLSTEL,OFFLSTEL   SET UP OFFICE LIST ELEM                      
         MVI   OFFLSTD.GSPLOFFL,X'02'              ELEMENT CODE                 
         MVI   OFFLSTD.GSPLOFLN,GSPLOFFN-GSPLOFFL                               
*                                                                               
MYSUB110 MVC   SAVCOMPN,SLSCOMPN   SAVE OFF COMPANY INFO FOR COMPARE            
         MVC   SAVCOMPC,SLSCOMPC                                                
         MVC   SAVCOMPH,SLSHUBAD                                                
*                                                                               
         LA    R3,OFFLSTD.GSPLOFFN                                              
         XR    R1,R1                                                            
         ICM   R1,1,OFFLSTD.GSPLOFNM                                            
         BZ    MYSUB130                                                         
*                                                                               
MYSUB120 CLC   0(L'GSPLOFFN,R3),SLSDDSOF  ALREADY HAVE THIS OFFICE?             
         BE    MYSUB140               YES                                       
         LA    R3,L'GSPLOFFN(R3)      NEXT OFFICE IN LIST                       
         BCT   R1,MYSUB120            UNTIL WE CHECK ALL OFFICE WE HAVE         
                                                                                
MYSUB130 MVC   0(L'GSPLOFFN,R3),SLSDDSOF                                        
         XR    R1,R1                                                            
         IC    R1,OFFLSTD.GSPLOFNM                                              
         LA    R1,1(R1)                                                         
         STC   R1,OFFLSTD.GSPLOFNM                                              
*                                                                               
         LA    R3,L'GSPLOFFN(R3)   FIGURE OUT THE NEW ELEMENT LENGTH            
         LA    R1,OFFLSTD.GSPLOFFL                                              
         SR    R3,R1                                                            
         STC   R3,OFFLSTD.GSPLOFLN                                              
***********************************                                             
* LOOKUP THE POWER CODE FOR THE REP COMPANY                                     
***********************************                                             
MYSUB140 LA    R3,SBSTAB           START FROM THE BEGINNING                     
         USING SBSTABD,R3                                                       
MYSUB143 CLI   0(R3),X'FF'         DID WE HIT THE END?                          
         BE    MYSUBERR            YES, UNKNOWN REP COMPANY                     
*                                                                               
         CLC   SLSHUBAD,SBHUBADR   SAME HUB ADDRESS?                            
         BNE   MYSUB146                                                         
         CLC   SLSCOMPC,SBREPNUM   IS IT THE SAME COMPANY CODE (BINARY)         
         BE    MYSUB150                                                         
MYSUB146 LA    R3,SBREPLNQ(R3)                                                  
         B     MYSUB143                                                         
*                                                                               
MYSUBERR MVC   SAVCOMPC,=X'FFFF'   SO WE DO NOT CREATE A SUBSIDIARY             
*                                                                               
         MVC   P(15),=C'***************'                                        
         GOTO1 REPORT                                                           
         MVC   P(15),=C'* BAD COMPANY *'                                        
         GOTO1 REPORT                                                           
         MVC   P(15),=C'***************'                                        
         GOTO1 REPORT                                                           
         MVC   P(7),=C'BAD -->'                                                 
         MVC   P+8(133),IO3                                                     
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         OI    MISCFLG1,MF1NOTFY   NOTIFY THAT THERE WAS A PROBLEM              
         B     MYSUB100            NEXT SALESPERSON RECORD                      
*                                                                               
MYSUB150 MVC   SVREPPWC,SBREPPWC                                                
         DROP  R3                                                               
*                                                                               
         OC    SAVSPCOD,SAVSPCOD                                                
         BZ    MYSUB160                                                         
         CLC   SAVSPCOD,SLSSPCOD   DID WE GET A NEW SALESPERSON CODE?           
         BE    MYSUB180            NO, THEN IT WOULD BE ANOTHER OFFICE          
*                                                                               
         LA    RE,IO                                                            
         XR    RF,RF               YES, PUT OUT WHAT WE CREATED IN IO           
         ICM   RF,3,GSPLFLEN-GSPLRECD(RE)                                       
         LA    RF,4(RF)            SET QSAM LENGTH                              
         STCM  RF,3,IOQSAM                                                      
         PUT   TOCONCAT,IOQSAM                                                  
*                                                                               
***  WE'RE CLEARING THE IOAREA TO CREATE SALESPERSON OR POINTPERSON REC         
MYSUB160 MVC   SAVSPCOD,SLSSPCOD                                                
*                                                                               
         LA    RE,IO               CLEAR THE IO AREA                            
         L     RF,=F'4000'                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         TM    SLSFLAG,SLSPP       IS IT POINT PERSON?                          
         BO    POINTP               - YUP IT IS                                 
         EJECT                                                                  
******************************                                                  
*  SALES PERSON RECORD HERE  *                                                  
******************************                                                  
SALESP   XC    KEY,KEY                                                          
         USING GSPLRECD,R6                                                      
         LA    R6,IO                                                            
         MVC   GSPLKTYP(2),=X'7101'   SALESPERSON RECORD                        
         MVC   GSPLKSRP,SVREPPWC                                                
         MVC   GSPLKSAL,SLSSPCOD   BINARY SALESPERSON CODE                      
*                                                                               
*  ELEMENT INITIATION                                                           
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING GSPLSPCD,R7                                                      
         MVI   GSPLSPCD,X'01'                                                   
         MVI   GSPLSPLN,GSPLSALL   ELEMENT LENGTH                               
         MVC   GSPLSPNM(10),SLSFRSTN                                            
         MVC   GSPLSPNM+10(10),SLSLASTN                                         
         MVC   GSPLSPOF,SLSDDSOF                                                
         MVC   GSPLSPPC,SVREPPWC                                                
         B     ELEMADD                                                          
******************************                                                  
*  POINT PERSON RECORD HERE  *                                                  
******************************                                                  
POINTP   XC    KEY,KEY                                                          
         USING GSPLRECD,R6                                                      
         LA    R6,IO                                                            
         MVC   GSPLKTYP(2),=X'7102'   POINTPERSON RECORD                        
         MVC   GSPLKPRP,SVREPPWC                                                
         MVC   GSPLKPP,SLSSPCOD    BINARY POINTPERSON CODE                      
*                                                                               
*  ELEMENT INITIATION                                                           
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING GSPLPPCD,R7                                                      
         MVI   GSPLPPCD,X'01'                                                   
         MVI   GSPLPPLN,GSPLPPL    ELEMENT LENGTH                               
         MVC   GSPLPPNM(10),SLSFRSTN                                            
         MVC   GSPLPPNM+10(10),SLSLASTN                                         
         MVC   GSPLPPOF,SLSDDSOF                                                
*                                                                               
ELEMADD  MVI   GSPLFLEN+1,GSPLFRST    LENGTH BEFORE ANY ELEMS                   
*                                                                               
         LA    R2,GSPLFRST(R6)                                                  
***  ELEM START IS X'002A' INTO THE RECORD                                      
***  RECORD LEN IS X'0020' INTO THE RECORD                                      
***  MAX LENGTH OF GENFIL RECORDS ARE 2000 BYTES (X'07D0')                      
         GOTO1 RECUP,DMCB,(X'FE',IO),ELEM,(R2),=X'002A002007D0'                 
         LA    R2,GSPLPPL(R2)                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING GSPLNFCD,R7                                                      
         MVI   GSPLNFCD,GSPLNFCQ   X'02'                                        
         MVI   GSPLNFLN,GSPLNFL    LENGTH OF ELEMENT                            
         MVC   GSPLNFCM,SLSCOMPN   COMPANY NAME                                 
         MVC   GSPLNFCC,SLSCOMPC   COMPANY CODE                                 
         MVC   GSPLNFLS,SLSLASTN   LAST NAME                                    
         MVC   GSPLNFFR,SLSFRSTN   FIRST NAME                                   
         GOTO1 RECUP,DMCB,(X'FE',IO),ELEM,(R2),=X'002A002007D0'                 
*                                                                               
MYSUB180 LA    R7,IO                                                            
         XR    R2,R2                                                            
         ICM   R2,3,GSPLFLEN-GSPLRECD(R7)                                       
         AR    R2,R7               R2 = A(EOR)                                  
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING GSPLOFCD,R7                                                      
         MVI   GSPLOFCD,GSPLOFCQ   X'03'                                        
         MVI   GSPLOLEN,GSPLOFL    LENGTH OF ELEMENT                            
         MVC   GSPLOFDD,SLSDDSOF   DDS OFFICE CODE                              
         MVC   GSPLOFON,SLSOFFNM   OFFICE NAME                                  
         MVC   GSPLOFOC,SLSOFFCD   OFFICE CODE                                  
         MVC   GSPLOFMC,SLSNSICD   NSI MARKET CODE                              
*                                                                               
         GOTO1 RECUP,DMCB,(X'FE',IO),ELEM,(R2),=X'002A002007D0'                 
         B     MYSUB100                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT THE SALES INFORMATION FROM RECORD READ FROM THE            
* SALESPERSON FILE                                                              
***********************************************************************         
GTSLSINF NTR1                                                                   
         SELECT  CLC,SAVEREP,EQ                                                 
           WHEN  (=C'K3')          A KATZ REP COMPANY?                          
             LA  R4,SALEINFO                                                    
           WHEN  (=C'IR')          A MFS REP COMPANY?                           
             LA  R4,SALEINF2                                                    
           WHEN  (=C'GR')          A REGIONAL REP COMPANY?                      
             LA  R4,SALEINF3                                                    
           WHEN  (=C'NM')          A NATIONAL MEDIA COMPANY?                    
             LA  R4,SALEINF4                                                    
           OTHRWISE                OTHERWISE IT IS ENTRAVISION                  
             LA  R4,SALEINF5                                                    
         ENDSEL                                                                 
*                                                                               
         GET   (R4),IO3                                                         
         AP    INPCOUNT,=P'1'      INCREMENT # OF SALESPEOPLE                   
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   GTSLS045                                                         
         MVC   P(133),IO3                                                       
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
GTSLS045 XC    IO3HEAD,IO3HEAD     EXTRACT WHAT WE READ FROM FILE               
         LA    RE,IO3+133-1                                                     
         LA    RF,133                                                           
GTSLS050 CLI   0(RE),C' '                                                       
         BH    GTSLS055                                                         
         BCTR  RE,0                                                             
         BCT   RF,GTSLS050                                                      
         DC    H'0'                                                             
GTSLS055 STC   RF,IO3HEAD+5                                                     
*                                                                               
*****    GOTO1 ASCANNER,DMCB,IO3HEAD,(15,BLOCK)   15 INPUTS                     
         GOTO1 APARSNIP,DMCB,(0,IO3HEAD),(15,BLOCK),(X'88',0)                   
*   WE'RE USING IO2 TO STORE ALL THE INFORMATION TEMPORARILY                    
         LA    RE,IO2              CLEAR THE IO AREA                            
         LA    RF,1000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R5,IO2                                                           
         USING SLSINFO,R5                                                       
*                                                                               
         LA    R4,BLOCK            WE ONLY WANT THE FIELD COMPONENTS            
         USING PSND,R4                 FROM PARSNIP                             
         CLI   PSNERR,0            THIS COMPONENT IN ERROR?                     
         BE    GTSLS065            NO                                           
GTSLS060 MVI   LINE,1                                                           
         MVC   P(15),=C'***************'                                        
         GOTO1 REPORT                                                           
         MVC   P(15),=C'**   ERROR   **'                                        
         GOTO1 REPORT                                                           
         MVC   P(15),=C'***************'                                        
         GOTO1 REPORT                                                           
         MVC   P(7),=C'ERR -->'                                                 
         MVC   P+8(133),IO3          THIS LINE HAS AN ERROR                     
         GOTO1 REPORT                                                           
         OI    MISCFLG1,MF1NOTFY   WE NEED TO BE NOTIFIED OF A PROBLEM          
         B     GTSLSNO                                                          
*                                                                               
GTSLS065 XR    RE,RE               POINTING TO COMPANY NAME                     
         IC    RE,PSNLEN                                                        
         BCTR  RE,0                                                             
         L     R1,PSNCOMP          A(COMPONENT)                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SLSHUBAD(0),0(R1)   MOVE THE COMPANY HUB ADDRESS OVER            
         OC    SLSHUBAD,SPACES                                                  
*                                                                               
         L     R4,PSNFLD                                                        
         CLI   PSNERR,0            THIS COMPONENT IN ERROR?                     
         BNE   GTSLS060            YES                                          
         XR    RE,RE               POINTING TO COMPANY NAME                     
         IC    RE,PSNLEN                                                        
         BCTR  RE,0                                                             
         L     R1,PSNCOMP          A(COMPONENT)                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SLSCOMPN(0),0(R1)      MOVE THE COMPANY NAME OVER                
*                                                                               
         L     R4,PSNFLD                                                        
         CLI   PSNERR,0            THIS COMPONENT IN ERROR?                     
         BNE   GTSLS060            YES                                          
         XR    RE,RE               POINTING TO COMPANY CODE                     
         ICM   RE,15,PSNNUM                                                     
         STCM  RE,3,SLSCOMPC                                                    
*                                                                               
         OI    SLSFLAG,SLSSP       ASSUME IT'S SALESPERSON                      
         L     R4,PSNFLD                                                        
         CLI   PSNERR,0            THIS COMPONENT IN ERROR?                     
         BNE   GTSLS060            YES                                          
         XR    RE,RE               POINTING TO UNWIRED REP NAME                 
         ICM   RE,1,PSNLEN         ANYTHING HERE?                               
         BZ    GTSLS070             - NOPE, IT IS SALESPERSON                   
         NI    SLSFLAG,X'FF'-SLSSP   TURN OFF SALESPERSON FLAG                  
         OI    SLSFLAG,SLSPP       WE HAVE A POINTPERSON                        
*                                                                               
GTSLS070 L     R4,PSNFLD           **NOT** A DUPLICATE LINE  !!!                
         L     R4,PSNFLD           DELIBERATE SKIP OF FIELD                     
*                                                                               
         CLI   PSNERR,0            THIS COMPONENT IN ERROR?                     
         BNE   GTSLS060            YES                                          
         XR    RE,RE               POINTING TO OFFICE NAME                      
         IC    RE,PSNLEN                                                        
         BCTR  RE,0                                                             
         L     R1,PSNCOMP                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SLSOFFNM(0),0(R1)      MOVE THE OFFICE NAME OVER                 
*                                                                               
         L     R4,PSNFLD                                                        
         CLI   PSNERR,0            THIS COMPONENT IN ERROR?                     
         BNE   GTSLS060            YES                                          
         XR    RE,RE               POINT TO OFFICE CODE                         
         ICM   RE,15,PSNNUM                                                     
         STCM  RE,3,SLSOFFCD       MOVE THE BINARY OFFICE CODE                  
*                                                                               
         L     R4,PSNFLD                                                        
         CLI   PSNERR,0            THIS COMPONENT IN ERROR?                     
         BNE   GTSLS060            YES                                          
         XR    RE,RE               POINT TO THE NSI MARKET CODE                 
         ICM   RE,15,PSNNUM                                                     
****                                                                            
         CH    RE,=H'124'          ATLANTA'S NSI MKT CODE WAS CHANGED           
         BNE   *+8                   FROM 168 TO 124, BUT DDS STILL             
         LH    RE,=H'168'            RECOGNIZES AT AS 168, SO CONVERT           
****                                                                            
         STCM  RE,7,SLSNSICD                                                    
*                                                                               
         L     R4,PSNFLD                                                        
         CLI   PSNERR,0            THIS COMPONENT IN ERROR?                     
         BNE   GTSLS060            YES                                          
         XR    RE,RE               POINT TO THE LAST NAME                       
         IC    RE,PSNLEN                                                        
         BCTR  RE,0                                                             
         L     R1,PSNCOMP                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SLSLASTN(0),0(R1)      MOVE THE LAST NAME OVER                   
         OC    SLSLASTN,SPACES                                                  
*                                                                               
         L     R4,PSNFLD                                                        
         CLI   PSNERR,0            THIS COMPONENT IN ERROR?                     
         BNE   GTSLS060            YES                                          
         XR    RE,RE               POINT TO THE FIRST NAME                      
         IC    RE,PSNLEN                                                        
         BCTR  RE,0                                                             
         L     R1,PSNCOMP                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SLSFRSTN(0),0(R1)      MOVE THE FIRST NAME OVER                  
         OC    SLSFRSTN,SPACES                                                  
*                                                                               
         L     R4,PSNFLD                                                        
         CLI   PSNERR,0            THIS COMPONENT IN ERROR?                     
         BNE   GTSLS060            YES                                          
         XR    RE,RE               POINT TO THE SALESPERSON CODE                
         ICM   RE,15,PSNNUM                                                     
         STCM  RE,7,SLSSPCOD       SAVE OFF BINARY SALESPERSON CODE             
***********************************                                             
* LOOKUP THE DDS OFFICE CODE FOR LIST TO BUILD                                  
***********************************                                             
         LR    R3,RB                                                            
         AHI   R3,MKTTAB-SPKS02    START FROM THE BEGINNING                     
         USING MKTTABD,R3                                                       
GTSLS075 CLI   0(R3),X'FF'         DID WE HIT THE END?                          
         BNE   GTSLS080                                                         
*****                                                                           
         MVC   P(41),=C'SKIPPED THIS RECORD: INVALID NSI MARKET #'              
         EDIT  (B3,SLSNSICD),(4,P+42),ALIGN=LEFT,FILL=0                         
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         MVC   P(7),=C'INV -->'                                                 
         MVC   P+8(133),IO3                                                     
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         B     GTSLSNO             RETURN NEQ CONDITION CODE                    
*****                                                                           
GTSLS080 CLC   SLSNSICD,MKTNSICD   SAME NSI MARKET (BINARY)?                    
         BE    GTSLS085                                                         
         LA    R3,MKTNQ(R3)                                                     
         B     GTSLS075                                                         
*                                                                               
GTSLS085 MVC   SLSDDSOF,MKTCTYCD       SAVE 2-CHAR DDS OFFICE CODE              
*                                                                               
GTSLSYES B     YES                                                              
*                                                                               
GTSLSNO  B     NO                                                               
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* CREATES THE SUBSIDIARY REP COMPANY RECORD                                     
***********************************************************************         
MAKESUB  NTR1                                                                   
*                                                                               
         LA    RE,IO               CLEAR THE IO AREA                            
         L     RF,=F'4000'                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R6,IO                                                            
         MVI   GSPLKTYP,GSPLRECQ   X'00007100' RECORD                           
         MVC   GSPLKRP1,SVREPPWC                                                
         MVC   GSPLKRP2,SVREPPWC                                                
         MVC   GSPLMRP,SAVEREP     MASTER REP BASED ON THE FILE                 
*****                                                                           
         CLC   SVREPPWC,=C'TM'       KBBKR - OUR BIT BUCKET REP?                
         BNE   *+10                                                             
         MVC   GSPLMRP,=C'TM'        DIFF MASTER REP FOR BIT BUCKET REP         
         CLC   SVREPPWC,=C'WJ'       IBBIR - OUR BIT BUCKET REP?                
         BNE   *+10                                                             
         MVC   GSPLMRP,=C'WJ'        DIFF MASTER REP FOR BIT BUCKET REP         
*                                                                               
         CLC   SVREPPWC,=C'SD'     REPS ON MFSQA  HUB  SHOULD HAVE              
         BNE   *+10                   A MASTER REP OF  SF                       
         MVC   GSPLMRP,=C'SF'                                                   
         CLC   SVREPPWC,=C'SF'                                                  
         BNE   *+10                                                             
         MVC   GSPLMRP,=C'SF'                                                   
*                                                                               
         CLC   SVREPPWC,=C'SJ'     REPS ON KRGQA  HUB  SHOULD HAVE              
         BNE   *+10                   A MASTER REP OF  SG                       
         MVC   GSPLMRP,=C'SG'                                                   
         CLC   SVREPPWC,=C'SG'                                                  
         BNE   *+10                                                             
         MVC   GSPLMRP,=C'SG'                                                   
*****                                                                           
         MVI   GSPLFLEN+1,GSPLFRST      LENGTH BEFORE ANY ELEMS                 
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING GSPLSBCD,R7                                                      
         MVI   GSPLSBCD,X'01'                                                   
         MVI   GSPLSBLN,GSPLSUBL                                                
         MVC   GSPLSBMR,SAVCOMPN                                                
         DROP  R7                                                               
*                                                                               
         LA    R2,GSPLFRST(R6)                                                  
         GOTO1 RECUP,DMCB,(X'FE',IO),ELEM,(R2),=X'002A002007D0'                 
         LA    R2,GSPLSUBL(R2)                                                  
*                                                                               
         GOTO1 RECUP,DMCB,(X'FE',IO),OFFLSTEL,(R2),=X'002A002007D0'             
         XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         XC    ELEM,ELEM           HUB ADDRESS ELEM                             
         LA    R7,ELEM                                                          
         USING GSPLHUBL,R7                                                      
         MVI   GSPLHUBL,X'05'                                                   
         MVI   GSPLHBLN,GSPLHBLQ                                                
         MVC   GSPLHBAD,SAVCOMPH                                                
         DROP  R7                                                               
*                                                                               
         GOTO1 RECUP,DMCB,(X'FE',IO),ELEM,(R2),=X'002A002007D0'                 
*                                                                               
         BRAS  RE,DUMPREC                                                       
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,GSPLFLEN                                                    
         LA    RF,4(RF)            SET QSAM LENGTH                              
         STCM  RF,3,IOQSAM                                                      
         PUT   TOCONCAT,IOQSAM                                                  
*                                                                               
         LA    RE,IO               CLEAR THE IO AREA                            
         L     RF,=F'4000'                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
*****    GOTO1 DATAMGR,DMCB,(0,=C'ADDREC'),(0,=C'GENFIL'),KEY,IO,DMWORK         
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*****************************************************************               
DUMPREC  NTR1                                                                   
         LA    R2,IO                                                            
         SR    R5,R5                                                            
         ICM   R5,3,GSPLFLEN-GSPLRECD(R2)                                       
         GOTO1 PRNTBL,DMCB,=CL20'*DUMP*',(R2),C'DUMP',(R5),=C'2D'               
         B     XIT                                                              
         LTORG                                                                  
***********************************************************************         
* NOTE: POWER CODES FOR EASTMAN & DEDICATED ARE SWAPPED, UNFORTUNATELY          
*       WE CAN'T CHANGE IT NOW AS THE SALESPERSON IS ENCODED WITH THE           
*       POWER CODE AND SALESPERSON SEQ #                                        
*                                                                               
* NOTE2: COPY OF THIS TABLE EXISTS IN SPLNK16, UPDATE THERE AS WELL             
*                                                                               
***********************************************************************         
SBSTAB   DS    0CL14                                                            
* KATZ                                                                          
         DC    CL10'KRG',AL2(00),C'K3'    MASTER REP - ADVANTAGE                
         DC    CL10'KRG',AL2(52),C'CR'    CHR - CHRISTAL                        
         DC    CL10'KRG',AL2(53),C'J0'    KRD - EASTMAN                         
         DC    CL10'KRG',AL2(55),C'KF'    KH  - UNIVISION RADIO SALES           
         DC    CL10'KRG',AL2(56),C'KU'    KR  - KATZ RADIO                      
         DC    CL10'KRG',AL2(58),C'NU'    CCR - CCM+E MMS                       
         DC    CL10'KRG',AL2(63),C'EA'    DED - KRG DEDICATED                   
         DC    CL10'KRG',AL2(64),C'6S'    KNR - NET RADIO SALES                 
         DC    CL10'KRG',AL2(65),C'RT'    KRG - KATZ RADIO GROUP                
         DC    CL10'KRG',AL2(67),C'SM'    CRD - CLEAR CHANNEL                   
         DC    CL10'KRG',AL2(68),C'UO'    K36 - KATZ360 NETWORK                 
         DC    CL10'KRG',AL2(69),C'QD'    WON - WESTWOOD ONE NATIONAL           
         DC    CL10'KRG',AL2(70),C'KI'    KNT - KATZNET                         
***      DC    CL10'KRG',AL2(54),C'K6'    NOT - KZ RAD GRP (DIM)                
***      DC    CL10'KRG',AL2(59),C'WC'    WSM - WEST SIDE                       
**REUSED DC    CL10'KRG',AL2(57),C'QD'    SPS - SPRTS SPEC                      
*                                                                               
* MFS - USING SOME OF ITG POWER CODES (SEE DDTSTIDTAB)                          
         DC    CL10'IR ',AL2(000),C'IR'   MASTER REP - SPECIALIZED              
         DC    CL10'IR ',AL2(056),C'IB'   GMP - GMPT                            
         DC    CL10'IR ',AL2(063),C'D4'   TACHER GMP                            
         DC    CL10'IR ',AL2(066),C'IF'   RRG - REGIONAL REPS GMP               
         DC    CL10'IR ',AL2(072),C'MG'   MG  + MCGAVREN GUILD RADIO            
         DC    CL10'IR ',AL2(077),C'GB'   SBI + SBS/INTEREP                     
         DC    CL10'IR ',AL2(088),C'DE'   NRR - REGIONAL REPS NON-REP           
         DC    CL10'IR ',AL2(101),C'NM'   LCF + INTEREP LOCAL FOCUS             
         DC    CL10'IR ',AL2(102),C'NR'   NLF + NON-REP LOCAL FOCUS             
         DC    CL10'IR ',AL2(109),C'PD'   MGM + MCGAVERN GUILD MEDIA            
         DC    CL10'IR ',AL2(110),C'PM'   LFR + LOCAL FOCUS RADIO               
*** GEN MEDIA PARTNERS SAID THESE REP COMPANIES BELOW ARE NO LONGER             
*** LEAVING THEM HERE IN CASE OF ISSUES, MARKING REUSED ONES, I.E. 56           
         DC    CL10'IR ',AL2(062),C'  '   CML - CUMULUS RADIO SALES             
**REUSED DC    CL10'IR ',AL2(056),C'IB'   ABR - ABC RADIO SALES                 
**REUSED DC    CL10'IR ',AL2(063),C'D4'   DAR + D & R RADIO <DEFUNCT>           
**REUSED DC    CL10'IR ',AL2(100),C'MP'   CIT - ABC-CITADEL                     
*                                                                               
* REGIONAL - ALSO USING ITG POWER CODES                                         
         DC    CL10'ROR',AL2(00),C'GR'    MASTER REP - REGIONAL                 
         DC    CL10'ROR',AL2(01),C'GV'    RRP - REGIONAL REPS                   
* NPM - ALSO USING ITG POWER CODES                                              
         DC    CL10'NPM',AL2(00),C'NM'    MASTER - NAT'L PUBLIC MEDIA           
         DC    CL10'NPM',AL2(01),C'RU'    NPM - NAT'L PUBLIC MEDIA              
* ENTRAVISION -  ALSO USING ITG POWER CODES                                     
         DC    CL10'ENT',AL2(00),C'EN'    MASTER - LOTUS ENTRAVISION            
         DC    CL10'ENT',AL2(01),C'MP'    ENTRAVISION SOLUTIONS                 
* TEST REPS                                                                     
         DC    CL10'KRGQA',AL2(0),C'SG'     MASTER REP-KRGQA ADVANTAGE          
         DC    CL10'KRGQA',AL2(52),C'SJ'    KQ0 - KQACHR                        
         DC    CL10'KRGTP',AL2(01),C'T1'    KT1 - KTPKR                         
         DC    CL10'KBBKR',AL2(02),C'TM'    BBR - KBBKR                         
         DC    CL10'MEG  ',AL2(03),C'HD'    MEG - MEG                           
         DC    CL10'KBBJR',AL2(04),C'*B'    BB2 - KBBJR                         
         DC    CL10'IBBIR',AL2(05),C'WJ'    IBR - IBBIR                         
         DC    CL10'IBBRR',AL2(06),C'H9'    IB2 - IBBRR                         
         DC    CL10'MFSQA',AL2(109),C'SD'   MQA - MQAGM                         
         DC    CL10'MFSQA',AL2(00),C'SF'    MQA - MASTER REP                    
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
SALEINFO DCB   DDNAME=SALEINFO,DSORG=PS,RECFM=FB,LRECL=133,            X        
               BLKSIZE=27930,MACRF=GM,EODAD=NOMORINP                            
SALEINF2 DCB   DDNAME=SALEINF2,DSORG=PS,RECFM=FB,LRECL=133,            X        
               BLKSIZE=27930,MACRF=GM,EODAD=NOMORINP                            
SALEINF3 DCB   DDNAME=SALEINF3,DSORG=PS,RECFM=FB,LRECL=133,            X        
               BLKSIZE=27930,MACRF=GM,EODAD=NOMORINP                            
SALEINF4 DCB   DDNAME=SALEINF4,DSORG=PS,RECFM=FB,LRECL=133,            X        
               BLKSIZE=27930,MACRF=GM,EODAD=NOMORINP                            
SALEINF5 DCB   DDNAME=SALEINF5,DSORG=PS,RECFM=FB,LRECL=133,            X        
               BLKSIZE=27930,MACRF=GM,EODAD=NOMORINP                            
TOCONCAT DCB   DDNAME=TOCONCAT,DSORG=PS,RECFM=VB,MACRF=PM,             X        
               LRECL=2048,BLKSIZE=8120,BUFNO=2                                  
***********************************************************************         
ADDSAVE  DS    A                                                                
***** ASCANNER DS    A                                                          
APARSNIP DS    A                                                                
JDTTODAY DS    XL4                 EBCDIC DATE FOR TODAY                        
*                                                                               
MISCFLG1 DS    XL1                 MISC FLAGS                                   
MF1NOTFY EQU   X'80'               - NOTIFY US THAT THERE WAS A PROBLEM         
*                                                                               
INPCOUNT DS    PL5                 SALESPERSON COUNTER PER FILE                 
ELCODE   DS    XL1                                                              
SAVCOMPC DS    XL2                                                              
SAVCOMPN DS    CL20                                                             
SAVCOMPH DS    CL10                SBS HUB ADDRESS                              
SAVSPCOD DS    XL3                 SAVED SALESPERSON CODE                       
SVREPPWC DS    CL2                 SAVED REP POWER CODE                         
SAVEREP  DS    CL2                                                              
*                                                                               
SAVEAGMD DS    CL1                                                              
SAVEKEY2 DS    CL32                                                             
BLOCK    DS    15CL32              STANDARD BLOCK OF 480                        
ELEMNAME DC    CL8'**ELEM**'                                                    
ELEM     DS    XL255                                                            
OFFLSTEL DS    XL100               OFFICE LIST ELEMENT                          
*                                                                               
IO2NAME  DC    CL8'**I/O2**'                                                    
IO2      DS    1000C                                                            
IO3HEAD  DS    XL8                 NOT QSAM AS DATASET IS FB                    
IO3      DS    150C                RECORDS ARE 133 BYTES                        
IO4HEAD  DS    XL8                 NOT QSAM AS DATASET IS FB                    
IO4      DS    150C                RECORDS ARE 133 BYTES                        
IONAME   DC    CL7'**I/O**'                                                     
IOQSAM   DS    XL4                                                              
IO       DS    4000C                                                            
       ++INCLUDE SPDARNSI                                                       
*                                                                               
SLSINFO  DSECT                                                                  
SLSHUBAD DS    CL10                SBS HUB ADDRESS                              
SLSCOMPN DS    CL20                SBS COMPANY NAME                             
SLSCOMPC DS    CL2                 SBS COMPANY CODE                             
SLSSPCOD DS    CL3                 SALESPERSON CODE                             
SLSLASTN DS    CL20                LAST NAME                                    
SLSFRSTN DS    CL20                FIRST NAME                                   
SLSOFFNM DS    CL20                SBS OFFICE NAME                              
SLSOFFCD DS    CL2                 SBS OFFICE CODE                              
SLSNSICD DS    CL3                 NSI MARKET CODE                              
SLSDDSOF DS    CL2                 DDS OFFICE CODE                              
SLSFLAG  DS    CL1                 SLS FLAG                                     
SLSPP    EQU   X'80'                - POINTPERSON                               
SLSSP    EQU   X'40'                - SALESPERSON                               
*                                                                               
SBSTABD  DSECT                                                                  
SBHUBADR DS    CL10              SBS HUBADDR                                    
SBREPNUM DS    AL2               KATZ COMPANY NUMBER                            
SBREPPWC DS    CL2               REP POWER CODE                                 
SBREPLNQ EQU   *-SBSTABD                                                        
*                                                                               
MKTTABD  DSECT                                                                  
MKTNSICD DS    AL3                 NSI MARKET CODE                              
MKTCTYCD DS    CL2                 MARKET NAME 2 LETTER ABBREV.                 
MKTNAME  DS    CL20                MARKET NAME 20 CHARACTER                     
MKTNQ    EQU   *-MKTNSICD                                                       
*                                                                               
       ++INCLUDE GEGENSPSAL                                                     
*PREFIX=CT$                                                                     
       ++INCLUDE CTGENSTAD                                                      
*PREFIX=                                                                        
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDACTIVD                                                       
****** ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDPARSNIPD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DDSMTPD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'175SPREPKS02 05/02/19'                                      
         END                                                                    
