*          DATA SET PPREPBS02  AT LEVEL 010 AS OF 05/01/02                      
*PHASE PPBS02A,+0                                                               
         TITLE 'PPBS02 - PRINTPAK INSERT/BILL TAPE FOR BACKER'                  
*                                                                               
************  CHANGE LOG  ************                                          
*                                                                               
*  SMYE  12/20/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
PPBS02   CSECT                                                                  
         NMOD1 0,PPBS02                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPBSWRKD,R8                                                      
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,PROCBUY                                                     
         BE    PROCESS                                                          
         CLI   MODE,LBUYPRD                                                     
         BE    PRDL                                                             
         CLI   MODE,LBUYEST                                                     
         BE    ESTL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
INITIAL  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   OUTCNT,=P'0'                                                     
         ZAP   INSCNT,=P'0'                                                     
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
         LA    R1,OUTFILE                                                       
*&&DO                                                                           
         OPENR (1)                                                              
*&&                                                                             
*&&OS                                                                           
         OPEN  (OUTFILE,(OUTPUT))                                               
*&&                                                                             
         B     EXIT                                                             
         EJECT                                                                  
PROCESS  DS    0H                                                               
         LA    R7,OUTREC                                                        
         XC    0(250,R7),0(R7)                                                  
         USING DDINS,R7                                                         
         MVC   DDIAGY(3),PBUYKAGY     AGY/MED                                   
         MVC   DDICLT,PCLTKCLT                                                  
         MVC   DDIPRD,PPRDKPRD                                                  
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDIEST,DUB                                                       
         UNPK  WORK(11),PBUYKPUB(6)                                             
         MVC   DDIVEN(8),WORK         FIRST 8 DIGITS                            
         LA    R2,DDIVEN+8                                                      
         CLC   WORK+8(2),=C'00'                                                 
         BE    PROC2               NO ZONE                                      
         MVI   DDIVEN+8,C','                                                    
         MVC   DDIVEN+9(2),WORK+8      ZONE                                     
         LA    R2,DDIVEN+11                                                     
PROC2    CLI   PBUYKPUB+5,0                                                     
         BE    PROC2B              NO EDITION                                   
         XC    WORK(25),WORK                                                    
         MVC   WORK(5),=X'1000000000'                                           
         MVC   WORK+5(1),PBUYKEDT                                               
         GOTO1 PUBEDIT,DMCB,(C'0',WORK),(0,WORK+10)                             
         MVI   0(R2),C','                                                       
         MVC   1(3,R2),WORK+19         EDITION                                  
*                                                                               
PROC2B   OC    DDIVEN+8(7),SPACES                                               
PROC3    DS    0H                                                               
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         UNPK  DDIGRS,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDIGRS+09,X'F0'                                                  
         L     R0,AGYCOM                                                        
         CVD   R0,DUB                                                           
         UNPK  DDIACOM,DUB                                                      
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDIACOM+9,X'F0'                                                  
         L     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         UNPK  DDICD,DUB                                                        
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDICD+09,X'F0'                                                   
         MVC   DDIADNO,PBDJOB                                                   
*                                                                               
PUTDATES DS    0H                                                               
         MVC   WORK(3),PBUYKDAT    INS DATE                                     
         MVC   WORK+3(3),PBDSDATE  ON-SALE                                      
         MVC   WORK+6(3),PBDCDATE  CLOSING DATE                                 
         MVC   WORK+9(3),PBDBDATE      BILLABLE                                 
         LA    R3,WORK                                                          
         LA    R4,DDIINSDT                                                      
         LA    R5,4                FOR BCT                                      
*PUTD2    GOTO1 DTCNV,DMCB,(1,0(R3)),(0,0(R4))                                  
PUTD2    GOTO1 DATCON,DMCB,(3,0(R3)),(0,0(R4))                                  
         LA    R3,3(R3)                                                         
         LA    R4,6(R4)                                                         
         BCT   R5,PUTD2                                                         
         CLI   QMEDIA,C'N'                                                      
         BE    PUTNEWS                                                          
         IF    QMEDIA,EQ,C'O',AND,PBDSPACE,EQ,X'FF',PUTOUTD                     
*                                                                               
         GOTO1 OUTER,DMCB,PBUYREC,DDISPCE,WORK                                  
         OC    DDISPCE,SPACES                                                   
         B     PUTREG              GO PUT REGIONS                               
*                                                                               
PUTNEWS  DS    0H                  SPECIAL FOR NEWSPAPERS                       
         MVC   DDIUIND,PBDUIND     UNITS INDICATOR                              
         CLI   PBDUIND,X'89'       LOWER CASE I - 2 DECIMALS                    
         BNE   *+8                                                              
         MVI   DDIUIND,C'2'                                                     
         UNPK  DDILINES,PBDUNITS                                                
         CP    PBDUNITS,=P'0'                                                   
         BL    *+8                                                              
         OI    DDILINES+4,X'F0'                                                 
*                                                                               
PUTPREM  DS    0H                                                               
         MVI   DDICOLOR,C' '                                                    
         CLI   PBDCL,0                                                          
         BE    PUTREG                                                           
         ZIC   R0,PBDCL                                                         
         CVD   R0,DUB                                                           
         UNPK  DDICOLOR,DUB                                                     
         OI    DDICOLOR,X'F0'                                                   
         B     PUTREG                                                           
*                                                                               
PUTOUTD  DS    0H                  SPECIAL OUTDOOR FIELDS                       
         MVC   DDISHOW,=C'SPC'     SPECIAL                                      
         CLC   PBDSHOW(3),=C'SPC'  SPECIAL                                      
         BE    PUTO2                                                            
         UNPK  DDISHOW,PBDSHOW                                                  
         OI    DDISHOW+2,X'F0'                                                  
*                                                                               
PUTO2    UNPK  DDIREGLR,PBDREG                                                  
         OI    DDIREGLR+3,X'F0'                                                 
         UNPK  DDIILLUM,PBDILLUM                                                
         OI    DDIILLUM+3,X'F0'                                                 
         EJECT                                                                  
PUTREG   DS    0H                                                               
PUTSHR   DS    0H                                                               
         MVC   DDISHR,=C'100'                                                   
         CLC   PBUYKPRD,=C'ZZZ'                                                 
         BNE   PUTCOM                                                           
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'21'                                                     
PUTS2    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST FIND X'21'                              
         USING PPDUMD04,R2                                                      
         CLC   PPRDKPRD,PPRCODE                                                 
         BNE   PUTS2                                                            
         SR    R4,R4                                                            
         ZIC   R5,PPRCOST                                                       
         M     R4,=F'100'                                                       
         LR    R4,R5                                                            
         SR    R5,R5                                                            
         SRDA  R4,31                                                            
         ZIC   R6,PBDWTSUM                                                      
         DR    R4,R6                                                            
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         CVD   R5,DUB                                                           
         UNPK  DDISHR,DUB                                                       
         OI    DDISHR+2,X'F0'                                                   
         EJECT                                                                  
PUTCOM   DS    0H                                                               
         MVC   DDICOM,SPACES                                                    
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'66'                                                     
PUTC2    BAS   RE,NEXTEL                                                        
         BNE   PROCX                                                            
         CLI   1(R2),2                                                          
         BNH   PUTC2                                                            
         ZIC   R3,1(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DDICOM(0),2(R2)                                                  
*                                                                               
PROCX    DS    0H                                                               
         BAS   RE,WRITE                                                         
         AP    INSCNT,=P'1'                                                     
         B     EXIT                                                             
         EJECT                                                                  
TOTALS   DS    0H                                                               
         LA    R4,TITLES                                                        
         LA    R5,INSCNT                                                        
         LA    R6,1                                                             
TOT2     MVC   P+7(17),0(R4)                                                    
         EDIT  (P4,0(R5)),(9,P+26),0,COMMAS=YES                                 
         GOTO1 REPORT                                                           
         LA    R4,17(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R6,TOT2                                                          
         GOTO1 REPORT              SKIP A LINE                                  
         MVC   P+7(13),=C'TOTAL RECORDS'                                        
         EDIT  OUTCNT,(9,P+26),0,COMMAS=YES                                     
         MVI   P+35,C'*'                                                        
         GOTO1 REPORT                                                           
         LA    R2,OUTFILE                                                       
*&&DO                                                                           
         CLOSER (2)                                                             
*&&                                                                             
*&&OS                                                                           
         CLOSE ((2),)                                                           
*&&                                                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
         DC    F'0'                                                             
WRITE    ST    RE,WRITE-4                                                       
         LA    R1,OUTFILE                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         AP    OUTCNT,=P'1'                                                     
         L     RE,WRITE-4                                                       
         BR    RE                                                               
*                                                                               
TITLES   DS    0C                                                               
         DC    CL17'INSERTION DETAIL'                                           
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
OUTFILE  DTFMT BLKSIZE=1750,DEVADDR=SYS008,FILABL=STD,IOAREA1=OUTBUFF, X        
               RECFORM=FIXBLK,RECSIZE=175,REWIND=UNLOAD,               X        
               TYPEFLE=OUTPUT,WORKA=YES                                         
*&&                                                                             
*&&OS                                                                           
OUTFILE  DCB   DDNAME=OUTFILE,         DOS SYS008                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00175,                                            X        
               BLKSIZE=01750,          DOS BLKSIZE=01750               X        
               MACRF=PM                                                         
*&&                                                                             
*                                                                               
*&&DO                                                                           
OUTBUFF  DS    1750C                                                            
*&&                                                                             
         EJECT                                                                  
PPBSWRKD DSECT                                                                  
OUTCNT   DS    PL4'0'                                                           
INSCNT   DS    PL4'0'                                                           
*                                                                               
ELCODE   DS    CL1                                                              
ZEROS    DS    CL30                                                             
         DS    F                                                                
OUTREC   DS    CL250                                                            
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
DDINS    DSECT                ***** PRINT INSERTION DETAIL RECORD *****         
DDIAGY   DS    CL2                 AGENCY                                       
DDIMED   DS    CL1                 MEDIA                                        
DDICLT   DS    CL3                 CLIENT                                       
DDIPRD   DS    CL3                 PRODUCT                                      
DDIEST   DS    CL3      NUMERIC    ESTIMATE                                     
DDIVEN   DS    CL15                VENDOR NUMBER NNNNNNNN,NN,AAA                
DDIADNO  DS    CL6                 ADCODE                                       
DDIINSDT DS    CL6                 INSERTION DATE                               
DDIOSDT  DS    CL6                 ON SALE DATE                                 
DDICLDT  DS    CL6                 CLOSING DATE                                 
DDIBLDT  DS    CL6                 BILLABLE DATE                                
*                                                                               
DDIGRS   DS    CL10                GROSS                                        
DDIACOM  DS    CL10                AGY COMM                                     
DDICD    DS    CL10                CASH DISC                                    
*                                                                               
DDISPCE  DS    CL17                SPACE FOR M,T,S,O                            
         ORG   DDISPCE                                                          
*                                  NEWSPAPER FIELDS                             
DDILINES DS    CL5                 UNITS                                        
DDICOLOR DS    CL1                 COLOR                                        
DDIUIND  DS    CL1                 UNITS INDICATOR L=LINES,I=INCHES             
*                                  2=INCHES WITH 2 DECIMALS                     
         ORG   DDISPCE                                                          
*                                  OUTDOOR FIELDS                               
DDISHOW  DS    CL3                 SHOWING  (SPC=SPECIAL)                       
DDIREGLR DS    CL4                 REGULAR                                      
DDIILLUM DS    CL4                 ILLUM                                        
         ORG                                                                    
*                                                                               
DDISHR   DS    CL3                 COST PERCENTAGE                              
         DS    CL21                SPARE                                        
DDICOM   DS    CL47                FIRST BUY COMMENT                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PPREPBS02 05/01/02'                                      
         END                                                                    
