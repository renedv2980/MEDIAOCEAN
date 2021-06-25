*          DATA SET PPREPIT02  AT LEVEL 024 AS OF 05/01/02                      
*PHASE PPIT02C,+0                  **** NOTE "C" PHASE                          
*                                                                               
************  CHANGE LOG  ************                                          
*                                                                               
*  SMYE  12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
         TITLE 'PPIT02 - PRINTPAK INSERTION INTERFACE TAPE'                     
*                                                                               
*        QOPT1  Y= BREAKOUT ZZZ INSERTIONS                                      
*                                                                               
*        QOPT5  P=PRINT OUTPUT RECS                                             
*                                                                               
PPIT02   CSECT                                                                  
         NMOD1 0,PPIT02                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPITWRKD,R8                                                      
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,FBUYREQ                                                     
         BE    FIRSTB                                                           
         CLI   MODE,PROCBUY                                                     
         BE    PROCESS                                                          
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
INITIAL  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   OUTCNT,=P'0'                                                     
         ZAP   TGROSS,=P'0'                                                     
         ZAP   TAGYCOM,=P'0'                                                    
         ZAP   TCD,=P'0'                                                        
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
         OPEN  (OUTFILE,(OUTPUT))                                               
         B     EXIT                                                             
*                                                                               
FIRSTB   DS    0H                                                               
         MVI   FCRDACTV,C'N'         SET TO READ INACTIVE POINTERS              
         CLI   QOPT1,C'Y'            SEE IF BREAKING OUT ZZZ BUYS               
         BE    FIRSTB5                                                          
*                                    IF NOT ONLY READ ACTIVE POINTERS           
         MVI   FCRDACTV,C'Y'         SET TO READ ACTIVE POINTERS                
*                                                                               
FIRSTB5  CLC   QDIV,SPACES                                                      
         BE    FIRSTBX                                                          
         MVI   FCGTDIV,C'Y'                                                     
         CLC   QREGION,SPACES                                                   
         BE    FIRSTBX                                                          
         MVI   FCGTREG,C'Y'                                                     
         CLC   QDIST,SPACES                                                     
         BE    FIRSTBX                                                          
         MVI   FCGTDIST,C'Y'                                                    
FIRSTBX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
PROCESS  DS    0H                                                               
         LA    R7,OUTREC                                                        
         MVC   0(132,R7),SPACES                                                 
         MVC   132(132,R7),SPACES                                               
         MVC   264(46,R7),SPACES                                                
         USING DDINS,R7                                                         
         MVI   DDISYS,C'P'                                                      
         MVC   DDICOD,=C'IN'                                                    
*                                                                               
         MVC   DDIAGY(3),PBUYKAGY     AGY/MED                                   
         MVC   DDICLT,PCLTKCLT                                                  
         MVC   DDIPRD,PPRDKPRD                                                  
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDIEST,DUB                                                       
         UNPK  WORK(11),PBUYKPUB(6)                                             
         MVC   DDIVEN(10),WORK                                                  
         MVC   DDIVENED,SPACES                                                  
PROC2    CLI   PBUYKPUB+5,0                                                     
         BE    PROC3               NO EDITION                                   
         XC    WORK(25),WORK                                                    
         MVC   WORK(5),=X'1000000000'                                           
         MVC   WORK+5(1),PBUYKEDT                                               
         GOTO1 PUBEDIT,DMCB,(C'0',WORK),(0,WORK+10)                             
         MVC   DDIVENED,WORK+19                                                 
         OC    DDIVENED,SPACES         EDITION                                  
*                                                                               
PROC3    DS    0H                                                               
         MVC   DDISTATE,PUBSTATE                                                
         MVC   DDICITY,PUBCITY                                                  
         OC    DDISTATE(18),SPACES                                              
         MVC   DDIVENNM,PUBNAME                                                 
         MVC   DDIZONNM,PUBZNAME                                                
         OC    DDIVENNM,SPACES                                                  
         OC    DDIZONNM,SPACES                                                  
*                                                                               
PUTCIRC  DS    0H                                                               
         MVC   DDICIRC,ZEROS                                                    
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'30'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PUTREG                                                           
         USING PPDUMD06,R2                                                      
         ZAP   DUB,PUBCIR1                                                      
         UNPK  DDICIRC,DUB                                                      
         OI    DDICIRC+8,X'F0'                                                  
         DROP  R2                                                               
*                                                                               
PUTREG   DS    0H             LEAVE AS SPACES IF NOT BY DIV/REG/DST             
         CLI   PDIVREC,0                                                        
         BE    *+10                                                             
         MVC   DDIDIV,PDIVKDIV                                                  
         CLI   PREGREC,0                                                        
         BE    *+10                                                             
         MVC   DDIREG,PREGKREG                                                  
         CLI   PDSTREC,0                                                        
         BE    *+10                                                             
         MVC   DDIDIST,PDSTKDST                                                 
*                                                                               
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
*                                                                               
         AP    TGROSS,DUB                                                       
*                                                                               
         UNPK  DDIGRS,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDIGRS+11,X'F0'                                                  
         L     R0,AGYCOM                                                        
         CVD   R0,DUB                                                           
*                                                                               
         AP    TAGYCOM,DUB                                                      
*                                                                               
         UNPK  DDIACOM,DUB                                                      
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDIACOM+11,X'F0'                                                 
         L     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
*                                                                               
         AP    TCD,DUB                                                          
*                                                                               
         UNPK  DDICD,DUB                                                        
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDICD+11,X'F0'                                                   
*                                                                               
         L     R0,PBDPLCOS            PLANNED COST                              
         CVD   R0,DUB                                                           
         UNPK  DDIPCOST,DUB                                                     
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDIPCOST+11,X'F0'                                                
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+1,PBDTAX                                                    
         L     R0,FULL              TAX PCT                                     
         CVD   R0,DUB                                                           
         UNPK  DDITAX,DUB                                                       
         OI    DDITAX+5,X'F0'                                                   
*                                                                               
         MVC   DDIADNO,PBDJOB                                                   
         OC    DDIADNO,SPACES                                                   
*                                                                               
PUTDATES DS    0H                                                               
*        GOTO1 DTCNV,DMCB,(1,PBUYKDAT),(0,DDIINSDT)                             
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(0,DDIINSDT)                            
         ZIC   R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDIINSLN,DUB                                                     
         OC    PBDIDAT2,PBDIDAT2                                                
         BZ    PUTD1                                                            
*        GOTO1 DTCNV,DMCB,(1,PBDIDAT2),(0,DDIINSD2)                             
         GOTO1 DATCON,DMCB,(3,PBDIDAT2),(0,DDIINSD2)                            
         MVC   DDIINSME,PBDEMIND                                                
         OI    DDIINSME,C' '                                                    
*                                                                               
PUTD1    MVC   WORK(3),PBDBDATE     BILLABLE                                    
         MVC   WORK+3(3),PBDPDATE    PAYABLE                                    
         MVC   WORK+6(3),PBDCDATE  CLOSING DATE                                 
         MVC   WORK+9(3),PBDMDATE     MATERIALS CLOSING                         
         MVC   WORK+12(3),PBDSDATE    ON-SALE                                   
         LA    R3,WORK                                                          
         LA    R4,DDIBLDT                                                       
         LA    R5,5                FOR BCT                                      
*PUTD2    GOTO1 DTCNV,DMCB,(1,0(R3)),(0,0(R4))                                  
PUTD2    GOTO1 DATCON,DMCB,(3,0(R3)),(0,0(R4))                                  
         CLC   0(6,R4),=C'000000'                                               
         BNE   *+10                                                             
         MVC   0(6,R4),SPACES                                                   
*                                                                               
         LA    R3,3(R3)                                                         
         LA    R4,6(R4)                                                         
         BCT   R5,PUTD2                                                         
         CLI   QMEDIA,C'N'                                                      
         BE    PUTNEWS                                                          
         IF    QMEDIA,EQ,C'O',AND,PBDSPACE,EQ,X'FF',PUTOUTD                     
*                                                                               
         GOTO1 OUTER,DMCB,PBUYREC,DDISPCE,WORK                                  
         OC    DDISPCE,SPACES                                                   
         B     PUTSHR              GO PUT REGIONS                               
*                                                                               
PUTNEWS  DS    0H                  SPECIAL FOR NEWSPAPERS                       
         MVC   DDIUIND,PBDUIND                                                  
         CLI   PBDUIND,X'89'       LOWER CASE I - INCHES TO 2 DECIMALS          
         BNE   *+8                                                              
         MVI   DDIUIND,C'2'        CHANGE TO 2                                  
*                                                                               
         OI    DDIUIND,C' '                                                     
*                                                                               
         L     R0,UNITS                                                         
         CVD   R0,DUB                                                           
         UNPK  DDIUNITS,DUB                                                     
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDIUNITS+4,X'F0'                                                 
         MVC   DDINSPCE,PBDSPACE                                                
         OC    DDINSPCE,SPACES                                                  
*                                                                               
PUTPREM  DS    0H                                                               
         MVI   DDICOLOR,C' '                                                    
         CLI   PBDCL,0                                                          
         BE    PUTSHR                                                           
         ZIC   R0,PBDCL                                                         
         CVD   R0,DUB                                                           
         UNPK  DDICOLOR,DUB                                                     
         OI    DDICOLOR,X'F0'                                                   
         B     PUTSHR                                                           
*                                                                               
PUTOUTD  DS    0H                  SPECIAL OUTDOOR FIELDS                       
         MVC   DDISHOW,=C'99999'   SPECIAL                                      
         CLC   PBDSHOW(3),=C'SPC'  SPECIAL                                      
         BE    PUTO2                                                            
         UNPK  DDISHOW,PBDSHOW                                                  
         OI    DDISHOW+4,X'F0'                                                  
*                                                                               
PUTO2    UNPK  DDIREGLR,PBDREG                                                  
         OI    DDIREGLR+4,X'F0'                                                 
         UNPK  DDIILLUM,PBDILLUM                                                
         OI    DDIILLUM+4,X'F0'                                                 
         EJECT                                                                  
PUTSHR   DS    0H                                                               
         MVC   DDISHRNM(6),=C'001001'                                           
         CLC   PBUYKPRD,=C'ZZZ'                                                 
         BNE   PUTCOM                                                           
         CLC   QPRODUCT,=C'ZZZ'      SEE IF ZZZ REQUEST                         
         BE    PUTCOM                                                           
         CLI   QOPT1,C'Y'            SEE IF BREAKING OUT ZZZ BUYS               
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
         ZIC   R0,PPRCOST                                                       
         CVD   R0,DUB                                                           
         UNPK  DDISHRNM,DUB                                                     
         OI    DDISHRNM+2,X'F0'                                                 
         ZIC   R0,PBDWTSUM            WEIGHT SUM                                
         CVD   R0,DUB                                                           
         UNPK  DDISHRDN,DUB                                                     
         OI    DDISHRDN+2,X'F0'                                                 
         DROP  R2                                                               
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
         B     EXIT                                                             
         EJECT                                                                  
TOTALS   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,TITLES                                                        
         LA    R5,TGROSS                                                        
         LA    R6,3                                                             
TOT2     MVC   P+7(17),0(R4)                                                    
         EDIT  (P8,0(R5)),(11,P+26),2,COMMAS=YES,MINUS=YES                      
         GOTO1 REPORT                                                           
         LA    R4,17(R4)                                                        
         LA    R5,8(R5)                                                         
         BCT   R6,TOT2                                                          
         GOTO1 REPORT              SKIP A LINE                                  
         MVC   P+7(13),=C'TOTAL RECORDS'                                        
         EDIT  OUTCNT,(9,P+26),0,COMMAS=YES                                     
         MVI   P+35,C'*'                                                        
         GOTO1 REPORT                                                           
         CLOSE (OUTFILE,)                                                       
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
         CLI   QOPT5,C'P'     PRINTING OUTPUT                                   
         BNE   WRIT2                                                            
         MVC   P(7),=C'001-120'                                                 
         MVC   P+8(120),OUTREC                                                  
         GOTO1 REPORT                                                           
         MVC   P(7),=C'121-240'                                                 
         MVC   P+8(120),OUTREC+120                                              
         GOTO1 REPORT                                                           
         MVC   P(7),=C'241-300'                                                 
         MVC   P+8(60),OUTREC+240                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
WRIT2    DS    0H                                                               
         LA    R1,OUTFILE                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         AP    OUTCNT,=P'1'                                                     
         L     RE,WRITE-4                                                       
         BR    RE                                                               
*                                                                               
TITLES   DS    0C                                                               
         DC    CL17'GROSS ORDERED'                                              
         DC    CL17'AGENCY COMMISSION'                                          
         DC    CL17'CASH DISCOUNT'                                              
         LTORG                                                                  
         EJECT                                                                  
OUTFILE  DCB   DDNAME=OUTFILE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=300,                                              X        
               BLKSIZE=300,                                            X        
               MACRF=PM                                                         
*                                                                               
         EJECT                                                                  
PPITWRKD DSECT                                                                  
OUTCNT   DS    PL4'0'                                                           
TGROSS   DS    PL8'0'                                                           
TAGYCOM  DS    PL8'0'                                                           
TCD      DS    PL8'0'                                                           
*                                                                               
ELCODE   DS    CL1                                                              
ZEROS    DS    CL30                                                             
         DS    F                                                                
OUTREC   DS    CL300                                                            
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPINSINTF                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024PPREPIT02 05/01/02'                                      
         END                                                                    
