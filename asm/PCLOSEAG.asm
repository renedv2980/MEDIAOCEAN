*          DATA SET PCLOSEAG   AT LEVEL 047 AS OF 12/23/02                      
*PHASE PCLOSEAA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PUBEDIT                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE GETINS                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'PCLOSEAG --- DELETE AG RECORDS '                                
*                                                                               
*                                                                               
*        X'03'   DIVISIONS           (NO-OPPED FOR THIS RUN)                    
*        X'04'   REGIONS             (NO-OPPED FOR THIS RUN)                    
*        X'05'   DISTRICTS           (NO-OPPED FOR THIS RUN)                    
*        X'07'   ESTIMATES           (NO-OPPED FOR THIS RUN)                    
*        X'09'   ESTIMATE BUCKETS    (NO-OPPED FOR THIS RUN)                    
*        X'10'   CONTRACTS           (NO-OPPED FOR THIS RUN)                    
*        X'11'   REPS                (NO-OPPED FOR THIS RUN)                    
*        X'15'   JOB RECORDS         (NO-OPPED FOR THIS RUN)                    
*        X'17'   PUBLISTS            (NO-OPPED FOR THIS RUN)                    
*        X'20'   BUYS                                                           
*        X'40'   STANDARD COMMENT    (NO-OPPED FOR THIS RUN)                    
*                                                                               
*                                                                               
*        SINCE THE JOB RECORD CODE IS X'15' AND THE BUY IS X'20'                
*        JOBS ARE ENCOUNTERED BEFORE THE BUYS ARE READ                          
*        SO A PRELIMINARY RUN MUST BE DONE WHICH WILL LIST                      
*        THE JOBS TO BE ADDED TO A HARDCODED TABLE (ADRECTBL)                   
*        FOR THE "LIVE" RUN.                                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        FOR THIS RUN THE OUTPUT AGENCY IS NOT CHANGED                          
*      CLIENT CODE OR CLIENT CODE/PRODUCT CODES ARE CHANGED                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
PCLOSEAG CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PCLOSEA,=V(REGSAVE)                                            
         SPACE 2                                                                
         LA    R6,PCLOSEAG+4095                                                 
         LA    R6,1(R6)                                                         
         USING PCLOSEAG+4096,R6    R6 AS 2ND BASE REGISTER                      
*                                                                               
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*                                                                               
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*                                                                               
*                                                                               
*                                                                               
START1   DS    0H                                                               
*                                                                               
         BAS   RE,CARDS                                                         
         CLC   =C'/*',CARD                                                      
         BE    START12                                                          
         CLC   =C'DUMP=',CARD                                                   
         BNE   START2                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
         B     START1                                                           
*                                                                               
START2   DS    0H                                                               
*                                                                               
         CLC   =C'PRINT',CARD                                                   
         BNE   START3B                                                          
         MVI   PRTSW,C'Y'                                                       
         B     START1                                                           
*                                                                               
START3   DS    0H                                                               
*                                                                               
*                                                                               
START3B  DS    0H                                                               
*                                                                               
         MVC   P+1(80),CARD                                                     
         BAS   RE,PRNT                                                          
*                                                                               
         B     START1                                                           
*                                                                               
START10  DS    0H                                                               
*                                                                               
START12  DS    0H                                                               
*                                                                               
GET      DS    0H                                                               
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         CLC   RECID,REC+3                                                      
         BE    GET10                                                            
         MVC   RECID,REC+3                                                      
*        BAS   RE,HEXREC                                                        
*                                                                               
GET10    DS    0H                                                               
*                                                                               
         CLI   EOFSW,C'Y'                                                       
         BE    EOJ                                                              
**********************************************************************          
*                                                                               
* ADD CHECK FOR AGENCY BEING                                                    
*                                                                               
         CLC   REC(2),=C'AG'                                                    
         BH    PUT                                                              
******** BH    EOF                 CAN STOP READING                             
*                                                                               
         CLC   REC(3),=C'AGI'      INTERACTIVE                                  
         BE    GET20                                                            
         CLC   REC(3),=C'AGM'      MAGAZINES                                    
         BE    GET20                                                            
         CLC   REC(3),=C'AGN'      NEWSPAPERS                                   
         BE    GET20                                                            
         CLC   REC(3),=C'AGO'      OUTDOOR                                      
         BE    GET20                                                            
         CLC   REC(3),=C'AGS'      SUPPLEMENT                                   
         BE    GET20                                                            
         CLC   REC(3),=C'AGT'      TRADE                                        
         BE    GET20                                                            
         B     PUT                                                              
*                                                                               
GET20    DS    0H                                                               
*                                                                               
         CLI   REC+3,X'20'                                                      
         BE    BUY                                                              
*                                                                               
*                                                                               
         B     PUT                OUTPUT OTHER RECORD TYPES                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BUY      DS    0H                                                               
*                                                                               
         LA    R2,REC                                                           
         USING PBUYREC,R2                                                       
*                                                                               
         CLC   PBUYKDAT(3),=X'660701'                                           
         BL    PUT                                                              
         AP    BUYCNT,=P'1'        BUYS JUL01/02 AND LATER                      
         DROP  R2                                                               
*                                                                               
         LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
         MVI   ELCODE,X'25'        PAY ELEMENT                                  
*                                                                               
         BAS   RE,NEXTEL           FOUND ?                                      
         BNE   BUY10               NO                                           
         OC    2(3,R2),2(R2)       CHECK FOR DATE                               
         BZ    BUY10                                                            
*        BAS   RE,DMPREC                                                        
*        MVC   P+1(22),=C'*** PAID INSERTION ***'                               
*        BAS   RE,PRNT                                                          
         AP    KEEPCNT,=P'1'       KEPT BUY                                     
         B     PUT                                                              
*                                                                               
BUY10    DS    0H                                                               
         LA    R2,REC+33           POINT TO BUY RECORD'S FIRST ELEM             
         MVI   ELCODE,X'26'        BILL ELEMENT                                 
*                                                                               
         BAS   RE,NEXTEL           FOUND ?                                      
         BNE   BUY20               NO                                           
         OC    5(3,R2),5(R2)       CHECK FOR DATE                               
         BZ    BUY20                                                            
*        BAS   RE,DMPREC                                                        
*        MVC   P+1(22),=C'*** BILLED INSERTION ***'                             
*        BAS   RE,PRNT                                                          
         AP    KEEPCNT,=P'1'        KEPT BUY                                    
         B     PUT                                                              
*                                                                               
BUY20    DS    0H                                                               
*                                                                               
***  ACCUMULATE GROSS, PAID AND BILLED DOLLARS FOR COPIED BUYS  ***             
*                                                                               
         GOTO1 =V(GETINS),DMCB,REC,PVALUES,REC+7,(C'X',0)                       
         LA    R2,REC                                                           
         USING PBUYREC,R2                                                       
*                                                                               
         TM    PBUYCNTL,X'80'                                                   
         BNZ   BUY30                                                            
*        BAS   RE,DMPREC                                                        
         BAS   RE,PRTREC                                                        
         AP    ACTCNT,=P'1'      ACTIVE BUY  - NO BILL/PAY                      
         B     BUY40                                                            
*                                                                               
BUY30    DS    0H                                                               
         AP    DELCNT,=P'1'      DELETED BUY - NO BILL/PAY                      
         XC    GROSS(12),GROSS                                                  
         DROP  R2                                                               
*                                                                               
BUY40    DS    0H                                                               
         L     RE,GROSS                                                         
         CVD   RE,DUB                                                           
         AP    BUYGRS,DUB                                                       
         L     RE,AGYCOM                                                        
         CVD   RE,DUB                                                           
         AP    BUYAGY,DUB                                                       
         L     RE,CSHDSC                                                        
         CVD   RE,DUB                                                           
         AP    BUYDSC,DUB                                                       
         L     RE,PGROSS                                                        
         CVD   RE,DUB                                                           
         AP    BUYPGRS,DUB                                                      
         L     RE,PAGYCOM                                                       
         CVD   RE,DUB                                                           
         AP    BUYPAGY,DUB                                                      
         L     RE,PCSHDSC                                                       
         CVD   RE,DUB                                                           
         AP    BUYPDSC,DUB                                                      
         L     RE,BGROSS                                                        
         CVD   RE,DUB                                                           
         AP    BUYBGRS,DUB                                                      
         L     RE,BAGYCOM                                                       
         CVD   RE,DUB                                                           
         AP    BUYBAGY,DUB                                                      
         L     RE,BCSCHDSC                                                      
         CVD   RE,DUB                                                           
         AP    BUYBDSC,DUB                                                      
*                                                                               
         B     DELBUY                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*    ADD CODE HERE IF SWITCHING OTHER THAN AGENCY AS IN AGSW BELOW              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
AGSW     MVC   REC(2),=C'MK'       MNH                                          
         B     OUT90                                                            
*                                                                               
CLSW     MVC   REC+4(3),NEWCODE    CLIENT ONLY                                  
         B     OUT90                                                            
*                                                                               
CLPRSW   MVC   REC+4(6),NEWCODE    CLIENT AND PRODUCT                           
         B     OUT90                                                            
*                                                                               
DELBUY   DS    0H                  CLOSE OUT                                    
*                                                                               
         AP    CLOCNT,=P'1'        CLOSED OUT COUNTER                           
         OI    REC+27,X'C0'                                                     
*        BAS   RE,DMPREC                                                        
         BAS   RE,PRTREC                                                        
         B     OUT90                                                            
*                                                                               
OUT90    CLI   PRTSW,C'Y'                                                       
         BNE   PUT                                                              
         MVI   DMPSW,C'Y'                                                       
         B     PUT                                                              
*                                                                               
*                                                                               
PUT      DS    0H                                                               
*                                                                               
PUTXX    BAS   RE,PUTREC                                                        
         B     GET                                                              
*                                                                               
*                                                                               
EOF      CLOSE (IN,)                                                            
         CLOSE (OUT,)                                                           
*                                                                               
         BAS   RE,PRNT                                                          
         LA    R3,COUNTS                                                        
         LA    R4,25                                                            
         LA    R5,COUNTSX                                                       
*                                                                               
EOF2     MVC   P+1(20),5(R3)                                                    
         OI    4(R3),X'0F'                                                      
         UNPK  P+22(7),0(5,R3)                                                  
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF2                                                       
*                                                                               
         BAS   RE,PRNT                                                          
         LA    R3,BUYDOLS                                                       
         LA    R4,34                                                            
         LA    R5,BUYDOLSX                                                      
*                                                                               
EOF4     MVC   P+1(25),9(R3)                                                    
         EDIT  (P9,0(R3)),(15,P+27),2,COMMAS=YES,CR=YES                         
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF4                                                       
*                                                                               
         MVI   EOFSW,C'Y'                                                       
         XIT1             RETURNS TO GETREC CALL                                
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SKIP     MVC   PCOM,=C'BC01'                                                    
         ZAP   LNCNT,=P'0'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT3    MVC   PCOM,=C'BL03'                                                    
         AP    LNCNT,=P'3'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT2    MVC   PCOM,=C'BL02'                                                    
         AP    LNCNT,=P'2'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT     MVC   PCOM,=C'BL01'                                                    
         AP    LNCNT,=P'1'                                                      
*                                                                               
PRNTR    NTR1                                                                   
*                                                                               
         GOTO1 =V(PRINT),DMCB,P,PCOM                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         LA    R2,4(R2)                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMPREC   NTR1                                                                   
*                                                                               
         LA    R5,REC                                                           
         MVC   HALF,REC+25                                                      
         SR    R2,R2                                                            
         LH    R2,HALF                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'REC',(R5),C'DUMP',(R2),=C'1D'                 
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMPKEY   NTR1                     DUMP KEY                                      
*                                                                               
         LA    R5,REC                                                           
         LA    R3,KLEN                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'KEY',(R5),C'DUMP',(R3),=C'1D'                 
*                                                                               
*                                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMPELEM  NTR1                          DUMP ELEM                                
*                                                                               
         LA    R4,ELEN                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'ELEM',(R2),C'DUMP',(R4),=C'1H'                
*                                                                               
         B     XIT                                                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRTREC   NTR1                                                                   
*                                                                               
TEST     CLI   REC+3,X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R7,P                                                             
         USING STARD,R7                                                         
*                                                                               
         MVC   STARMED,PBUYKMED                                                 
         MVC   STARCLT,PBUYKCLT                                                 
         MVC   STARPRD,PBUYKPRD                                                 
         GOTO1 =V(DATCON),DMCB,(3,PBUYKDAT),(10,STARDAT)                        
         EDIT  PBUYKEST,STAREST                                                 
         EDIT  PBUYKLIN,STARLIN                                                 
*                                                                               
         MVI   STARDEL,C'D'                                                     
         TM    PBUYCNTL,X'80'                                                   
         BNZ   *+8                                                              
         MVI   STARDEL,C'L'                                                     
*                                                                               
         XC    CHAR15,CHAR15                                                    
         MVC   CHAR6,PBUYKPUB                                                   
         GOTO1 =V(PUBEDIT),DMCB,(X'08',CHAR6),(C'S',CHAR15)                     
*                                                                               
         MVC   STARPUN,CHAR8      PUB NUMBER                                    
*                                                                               
         CLI   CHAR2,X'EF'         CHECK FOR NUMBER                             
         JH    PUBNUM01                                                         
         CLI   CHAR2,X'C1'                                                      
         JL    PUBNUM00                                                         
         CLI   CHAR2,X'C6'                                                      
         JH    PUBNUM00                                                         
         J     PUBNUM01                                                         
PUBNUM00 DS    0H                                                               
         MVC   STARPUE,CHAR2                                                    
         J     PUBEND                                                           
*                                                                               
PUBNUM01 DS    0H                                                               
         MVC   STARPUZ,CHAR2      PUB ZONE                                      
         MVC   STARPUE,CHAR3      PUB EDITION                                   
*                                                                               
PUBEND   DS    0H                                                               
***  ACCUMULATE                                                                 
         GOTO1 =V(GETINS),DMCB,REC,PVALUES,REC+7,(C'X',0)                       
         L     RE,GROSS                                                         
         CVD   RE,DUB                                                           
         ZAP   F1,DUB                                                           
         EDIT  (P9,F1),STARGRS,2,FLOAT=-                                        
         L     RE,AGYCOM                                                        
         CVD   RE,DUB                                                           
         ZAP   F2,DUB                                                           
         EDIT  (P9,F2),STARAGC,2,FLOAT=-                                        
         L     RE,CSHDSC                                                        
         CVD   RE,DUB                                                           
         ZAP   F3,DUB                                                           
         EDIT  (P9,F3),STARCDS,2,FLOAT=-                                        
*                                                                               
         BAS   RE,PRNT                                                          
*                                                                               
         DROP  R7                                                               
*                                                                               
         B     XIT                                                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
HEXREC   NTR1                                                                   
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,REC-4,P+1,40                                     
         BAS   RE,PRNT                                                          
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
GETREC   NTR1                                                                   
         GET   IN,REC-4                                                         
         AP    INCNT,=P'1'                                                      
*                                                                               
         CLI   REC+3,X'E0'         DIRECTORY ONLY ?                             
         BE    GETRNXTA            YES - BYPASS "EOR MARKING"                   
         CLI   REC+3,X'E1'         DIRECTORY ONLY ?                             
         BE    GETRNXTA            YES - BYPASS "EOR MARKING"                   
         CLI   REC+3,X'E2'         DIRECTORY ONLY ?                             
         BE    GETRNXTA            YES - BYPASS "EOR MARKING"                   
         CLI   REC+3,X'E3'         DIRECTORY ONLY ?                             
         BE    GETRNXTA            YES - BYPASS "EOR MARKING"                   
         CLI   REC+3,X'E4'         DIRECTORY ONLY ?                             
         BE    GETRNXTA            YES - BYPASS "EOR MARKING"                   
         CLI   REC+3,X'E5'         DIRECTORY ONLY ?                             
         BE    GETRNXTA            YES - BYPASS "EOR MARKING"                   
         CLI   REC+3,X'5A'         DIRECTORY ONLY ?                             
         BE    GETRNXTA            YES - BYPASS "EOR MARKING"                   
         CLI   REC+3,X'3A'         DIRECTORY ONLY ?                             
         BL    GETRNXT             NO                                           
         CLI   REC+3,X'3F'         DIRECTORY ONLY ?                             
         BNH   GETRNXTA            YES - BYPASS "EOR MARKING"                   
*                                                                               
GETRNXT  MVC   HALF,REC-4                                                       
         LH    R2,HALF                                                          
         LA    R3,REC-4(R2)                                                     
         MVC   0(2,R3),=X'0000'    EOR                                          
         B     XIT                                                              
GETRNXTA DS    0H                                                               
         AP    DIRIN,=P'1'                                                      
         B     XIT                                                              
*  *                                                                            
*                                                                               
PUTREC   NTR1                                                                   
*                                                                               
*                                                                               
         AP    DIROUT,=P'1'                                                     
         CLI   REC+3,X'E0'         DIRECTORY ONLY ?                             
         BE    PUTREC3             YES                                          
         CLI   REC+3,X'E1'         DIRECTORY ONLY ?                             
         BE    PUTREC3             YES                                          
         CLI   REC+3,X'E2'                                                      
         BE    PUTREC3                                                          
         CLI   REC+3,X'E3'                                                      
         BE    PUTREC3                                                          
         CLI   REC+3,X'E4'                                                      
         BE    PUTREC3                                                          
         CLI   REC+3,X'E5'                                                      
         BE    PUTREC3                                                          
         CLI   REC+3,X'5A'         DIRECTORY ONLY ?                             
         BE    PUTREC3             YES                                          
         CLI   REC+3,X'3A'         DIRECTORY ONLY ?                             
         BL    PUTREC2             NO                                           
         CLI   REC+3,X'3F'         DIRECTORY ONLY ?                             
         BNH   PUTREC3             YES                                          
*                                                                               
         CLI   DMPSW,C'Y'                                                       
         BNE   PUTREC2                                                          
         MVI   DMPSW,C'N'                                                       
         SP    DMPCNT,=P'1'                                                     
         BNP   PUTREC2                                                          
*        BAS   RE,DMPKEY                                                        
*        BAS   RE,DMPREC                                                        
*NOP*    BAS   RE,SKIP                                                          
PUTREC2  DS    0H                                                               
         SP    DIROUT,=P'1'                                                     
         MVC   HALF,REC+25                                                      
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
*                                                                               
PUTREC3  DS    0H                                                               
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
OUT      DCB   DDNAME=OUT,             DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=PM                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
UPSI     DS    XL1                                                              
DMDM     DS    CL6                                                              
         DS    0F                                                               
X        DS    CL100                                                            
WORK     DS    CL256                                                            
KLEN     EQU   25                                                               
ELEN     EQU   23                                                               
RECID    DC    X'00'                                                            
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
DMPCNT   DC    PL5'100000'                                                      
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
LASTAGM  DS    CL3                                                              
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
LKPARS   DS    6F                                                               
ADPARS   DS    6F                                                               
COMPARS  DS    6F                                                               
ESTPARS  DS    6F                                                               
TODAY    DS    CL8                                                              
PRTSW    DS    CL1                                                              
EOFSW    DS    CL1                                                              
CHAR6    DS    CL6                                                              
*                                                                               
CHAR15   DS    0CL13                                                            
CHAR8    DS    CL8                                                              
         DS    C                                                                
CHAR2    DS    CL2                                                              
         DS    C                                                                
CHAR3    DS    CL1                                                              
*                                                                               
F1       DS    PL9                                                              
F2       DS    PL9                                                              
F3       DS    PL9                                                              
         DS    0D                                                               
MYDUB    DS    PL8                                                              
NEWCODE  DS    CL6    "REPLACEMENT" CLT/PRD CODE FOR NEW RECORD                 
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
DIRIN    DC    PL5'0',CL20'DIR ONLY IN'                                         
DIROUT   DC    PL5'0',CL20'DIR ONLY OUT'                                        
*CLTCNT   DC    PL5'0',CL20'CLIENTS'                                            
*PRDCNT   DC    PL5'0',CL20'PRODUCTS'                                           
*DIVCNT   DC    PL5'0',CL20'DIVISIONS'                                          
*REGCNT   DC    PL5'0',CL20'REGIONS'                                            
*DSTCNT   DC    PL5'0',CL20'DISTRICTS'                                          
*PUBLCNT  DC    PL5'0',CL20'PUB LISTS'                                          
*REPCNT   DC    PL5'0',CL20'REPS'                                               
*ESTCNT   DC    PL5'0',CL20'ESTIMATES'                                          
*ESTBCNT  DC    PL5'0',CL20'ESTIMATE BUCKETS'                                   
*JOBCNT   DC    PL5'0',CL20'AD RECORDS'                                         
*ADCNT    DC    PL5'0',CL20'AD CODES IN BUYS'                                   
*ECCNT    DC    PL5'0',CL20'COMMENTS IN ESTS'                                   
*CCCNT    DC    PL5'0',CL20'COMMS IN CONTRACTS'                                 
*COCNT    DC    PL5'0',CL20'COMMENTS IN BUYS'                                   
BUYCNT    DC    PL5'0',CL20'BUYS - 7/1/02 +'                                    
DELCNT    DC    PL5'0',CL20'DEL BUYS-NO BILL/PAY'                               
ACTCNT    DC    PL5'0',CL20'ACT BUYS-NO BILL/PAY'                               
KEEPCNT   DC    PL5'0',CL20'KEPT BUYS - BILL/PAY'                               
CLOCNT    DC    PL5'0',CL20'CLOSED BUYS'                                        
*CONCNT   DC    PL5'0',CL20'CONTRACTS'                                          
*COMCNT   DC    PL5'0',CL20'STANDARD COMMENTS'                                  
*                                                                               
* REST OF COUNTERS SHOULD ALL BE ZERO IN THIS COPYPRT RUN                       
*                                                                               
* OTHER COUNTERS ADDED HERE WILL AUTOMATICALLY PRINT AT EOJ                     
*                                                                               
COUNTSX  EQU   *-1                                                              
*                                                                               
BUYDOLS  DS    0C                                                               
*                                                                               
BUYGRS   DC    PL9'0',CL25'GROSS ORDERED'                                       
BUYAGY   DC    PL9'0',CL25'AGENCY COMMISSION'                                   
BUYDSC   DC    PL9'0',CL25'CASH DISCOUNT'                                       
BUYPGRS  DC    PL9'0',CL25'GROSS PAID'                                          
BUYPAGY  DC    PL9'0',CL25'AGENCY COMMISSION PAID'                              
BUYPDSC  DC    PL9'0',CL25'CASH DISCOUNT PAID'                                  
BUYBGRS  DC    PL9'0',CL25'GROSS BILLED'                                        
BUYBAGY  DC    PL9'0',CL25'AGENCY COMMISSION BILLED'                            
BUYBDSC  DC    PL9'0',CL25'CASH DISCOUNT BILLED'                                
***************************************************************                 
****************************************************************                
* OTHER ACCUMULATORS ADDED HERE WILL AUTOMATICALLY PRINT AT EOJ                 
*                                                                               
BUYDOLSX EQU   *-1                                                              
*                                                                               
*                                                                               
P        DC    CL133' '                                                         
*                                                                               
***************************************************************                 
***************************************************************                 
PVALUES  DS    0F                                                               
*                                                                               
* ORDERED DATA                                                                  
*                                                                               
GROSS    DS    F                   GROSS ORDERED                                
AGYCOM   DS    F                   AGENCY COMMISSION                            
CSHDSC   DS    F                   CASH DISCOUNT                                
PYABLE   DS    F                   GROSS-AGYCOMM-CASHDSC                        
BLABLE   DS    F                   GROSS-CASH DSC                               
PREMIUM  DS    F                   (INCLUDED IN ABOVE FIELDS)                   
UNITS    DS    F                   NUMBER OF LINES BOUGHT                       
*                                                                               
***** NOTE ORDERED TAX UNDER PAID DATA                                          
* PAID DATA                                                                     
*                                                                               
PGROSS   DS    F                   GROSS PAID                                   
PAGYCOM  DS    F                   AGY COMM PAID                                
PCSHDSC  DS    F                   CASH DISCOUNT PAID                           
PAID     DS    F                   ACTUAL PAID AMOUNT                           
*                                                                               
TAX      DS    F                   ORDERED TAX - WAS PAYABLE DATE               
*                          (INCLUDED IN ORDERED GROSS,PYABLE,BLABLE)            
*                                  NET X PBDTAX (4 DECIMALS)                    
*                                                                               
* BILLED DATA                                                                   
*                                                                               
BGROSS   DS    F                   GROSS BILLED                                 
BAGYCOM  DS    F                   AGY COMM BILLED                              
BCSCHDSC DS    F                   CASH DISCOUNT BILLED                         
BILLED   DS    F                   ACTUAL BILLED AMOUNT                         
BLBLDT   DS    CL3                 BILLABLE DATE -YMD                           
*                                                                               
PVALUESX DS    0C                                                               
*                                                                               
***************************************************************                 
***************************************************************                 
*                                                                               
***************************************************************                 
***************************************************************                 
*                                                                               
*                                                                               
*          DATA SET PVALUES    AT LEVEL 005 AS OF 06/30/86                      
*                        *** OUTPUT PARAMETER BLOCK FOR GETINS ****             
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
         DS    D                                                                
*                                                                               
*                                                                               
**********RINT OFF                                                              
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
                                                                                
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PCLTREC                                                        
*                                                                               
*                                                                               
         PRINT ON                                                               
*                                                                               
STARD    DSECT                                                                  
STARALL  DS    0CL88                                                            
         DS    C                                                                
STARMED  DS    CL1                                                              
         DS    C                                                                
STARCLT  DS    CL3                                                              
         DS    C                                                                
STARPRD  DS    CL3                                                              
         DS    CL3                                                              
STAREST  DS    CL3                                                              
         DS    CL2                                                              
STARPUN  DS    CL8                                                              
STARPUZ  DS    CL2                                                              
STARPUE  DS    CL3                                                              
         DS    CL3                                                              
STARDAT  DS    CL8                                                              
         DS    CL2                                                              
STARLIN  DS    CL2                                                              
         DS    CL2                                                              
STARDEL  DS    CL1                                                              
         DS    CL2                                                              
STARGRS  DS    CL13                                                             
         DS    C                                                                
STARAGC  DS    CL13                                                             
         DS    C                                                                
STARCDS  DS    CL13                                                             
STARLENQ EQU   *-STARD                                                          
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047PCLOSEAG  12/23/02'                                      
         END                                                                    
