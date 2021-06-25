*          DATA SET COPYH9CLT  AT LEVEL 048 AS OF 02/15/02                      
*PHASE COPYH9CA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE GETINS                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'COPYH9CLT'                                                      
*                                                                               
*     THIS PROGRAM WILL COPY THE FOLLOWING RECORDS FOR BD (BDNY)                
*     FROM ONE CLIENT CODE (OR CLT/PRD CODES) TO ANOTHER (SEE CLPRDTBL)         
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
*        X'20'   BUYS                (NO-OPPED FOR THIS RUN)                    
*        X'40'   STANDARD COMMENT                                               
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
COPYH9   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,COPYH9,=V(REGSAVE)                                             
         SPACE 2                                                                
         LA    R6,COPYH9+4095                                                   
         LA    R6,1(R6)                                                         
         USING COPYH9+4096,R6    R6 AS 2ND BASE REGISTER                        
*                                                                               
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*                                                                               
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*                                                                               
* SET ADCODE AND STDCOM AND ESTIMATE NUMBER TABLES BINSRCH PARS                 
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
*        LA    R7,CLTTAB           POINT TO TABLE                               
GET      DS    0H                                                               
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
GET10    DS    0H                                                               
*                                                                               
         CLI   REC,C'Z'            SPECIAL DDS RECORD                           
         BE    EOF                 PAST ALL REAL DATA                           
**********************************************************************          
*                                                                               
* ADD CHECK FOR AGENCY BEING COPIED HERE                                        
*                                                                               
         CLC   REC(2),=C'SJ'                                                    
         BH    EOF                 CAN STOP READING                             
*                                                                               
         CLC   REC(3),=C'SJI'      INTERACTIVE                                  
         BE    GET20                                                            
         CLC   REC(3),=C'SJM'      MAGAZINES                                    
         BE    GET20                                                            
         CLC   REC(3),=C'SJN'      NEWSPAPERS                                   
         BE    GET20                                                            
         CLC   REC(3),=C'SJO'      OUTDOOR                                      
         BE    GET20                                                            
*        CLC   REC(3),=C'AFS'      SUPPLEMENT                                   
*        BE    GET20                                                            
*        CLC   REC(3),=C'AFT'      TRADE                                        
*        BE    GET20                                                            
         B     GET                                                              
         B     PUT                                                              
*                                                                               
GET20    DS    0H                                                               
*                                                                               
         CLC   REC+4(6),=C'AB AB '                                              
         BNE   GET                                                              
*        BNE   PUT                                                              
*                                                                               
         CLI   REC+3,X'06'         PRODUCT                                      
         BE    PRD                                                              
*                                                                               
         CLI   REC+3,X'07'         ESTIMATES                                    
         BE    EST                                                              
*                                                                               
*        CLI   REC+3,X'08'         BILL HEADER                                  
*        BE    BIL                                                              
*                                                                               
         CLI   REC+3,X'15'         JOB RECORDS                                  
         BE    JOB                                                              
*                                                                               
*                                                                               
         CLI   REC+3,X'20'         BUY                                          
         BE    BUY                                                              
*                                                                               
         B     GET                SKIP OTHER RECORD TYPES                       
         B     PUT                SKIP OTHER RECORD TYPES                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BUY      DS    0H                                                               
*                                                                               
         AP    BUYCNT,=P'1'                                                     
         B     CLTSW                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRD      DS    0H                                                               
*                                                                               
*                                                                               
         AP    PRDCNT,=P'1'                                                     
         B     CLTSW                                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
JOB      DS    0H                                                               
         AP    JOBCNT,=P'1'                                                     
         B     CLTSW                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EST      DS    0H                                                               
*                                                                               
         AP    ESTCNT,=P'1'                                                     
         B     CLTSW                                                            
*                                                                               
*                                                                               
*                                                                               
BIL      DS    0H                                                               
*                                                                               
         AP    BILCNT,=P'1'                                                     
         B     CLTSW                                                            
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
CLTSW    DS    0H                                                               
         LA    R8,CLTTAB1                                                       
CLTSW01  CLI   0(R8),X'FF'                                                      
         BE    CLTSW02                                                          
         MVC   REC+4(3),0(R8)    CLIENT                                         
         MVC   REC+7(3),0(R8)    PRODUCT                                        
         BAS   RE,PUTREC                                                        
         AHI   R8,3                                                             
         B     CLTSW01                                                          
*LTSW02  DS    0H                                                               
         MVC   REC+4(6),=C'AB AB '                                              
         B     OUT90                                                            
CLTSW02  DS    0H                                                               
         B     GET                                                              
*                                                                               
CLTTAB1  DS    0H                                                               
         DC    CL3'AB1'                                                         
         DC    CL3'AB2'                                                         
         DC    CL3'AB3'                                                         
         DC    CL3'AB4'                                                         
         DC    CL3'AB5'                                                         
         DC    CL3'AB6'                                                         
         DC    CL3'AB7'                                                         
         DC    CL3'AB8'                                                         
         DC    CL3'AB9'                                                         
         DC    X'FF'                                                            
*                                                                               
CLPRSW   MVC   REC+4(6),NEWCODE    CLIENT AND PRODUCT                           
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
GETREC   NTR1                                                                   
         GET   IN,REC-4                                                         
         AP    INCNT,=P'1'                                                      
*                                                                               
         CLI   REC+3,X'E0'         DIRECTORY ONLY ?                             
         BE    GETRNXTA            YES - BYPASS "EOR MARKING"                   
         CLI   REC+3,X'E1'         DIRECTORY ONLY ?                             
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
GETRNXTA B     XIT                                                              
*  *                                                                            
*                                                                               
PUTREC   NTR1                                                                   
*                                                                               
         CLI   DMPSW,C'Y'                                                       
         BNE   PUTREC2                                                          
         MVI   DMPSW,C'N'                                                       
         SP    DMPCNT,=P'1'                                                     
         BNP   PUTREC2                                                          
*        BAS   RE,DMPKEY                                                        
         BAS   RE,DMPREC                                                        
*NOP*    BAS   RE,SKIP                                                          
PUTREC2  DS    0H                                                               
         MVC   HALF,REC+25                                                      
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
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
         DS    0D                                                               
MYDUB    DS    PL8                                                              
NEWCODE  DS    CL6    "REPLACEMENT" CLT/PRD CODE FOR NEW RECORD                 
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
*CLTCNT   DC    PL5'0',CL20'CLIENTS'                                            
PRDCNT   DC    PL5'0',CL20'PRODUCTS'                                            
*DIVCNT   DC    PL5'0',CL20'DIVISIONS'                                          
*REGCNT   DC    PL5'0',CL20'REGIONS'                                            
*DSTCNT   DC    PL5'0',CL20'DISTRICTS'                                          
*PUBLCNT  DC    PL5'0',CL20'PUB LISTS'                                          
*REPCNT   DC    PL5'0',CL20'REPS'                                               
ESTCNT   DC    PL5'0',CL20'ESTIMATES'                                           
BILCNT   DC    PL5'0',CL20'BILL HEADERS '                                       
*ESTBCNT  DC    PL5'0',CL20'ESTIMATE BUCKETS'                                   
JOBCNT   DC    PL5'0',CL20'AD RECORDS'                                          
*ADCNT    DC    PL5'0',CL20'AD CODES IN BUYS'                                   
*ECCNT    DC    PL5'0',CL20'COMMENTS IN ESTS'                                   
*CCCNT    DC    PL5'0',CL20'COMMS IN CONTRACTS'                                 
*COCNT    DC    PL5'0',CL20'COMMENTS IN BUYS'                                   
BUYCNT   DC    PL5'0',CL20'BUYS'                                                
*CONCNT   DC    PL5'0',CL20'CONTRACTS'                                          
*COMCNT   DC    PL5'0',CL20'STANDARD COMMENTS'                                  
*                                                                               
* REST OF COUNTERS SHOULD ALL BE ZERO IN THIS COPYPRT RUN                       
*                                                                               
* OTHER COUNTERS ADDED HERE WILL AUTOMATICALLY PRINT AT EOJ                     
*                                                                               
COUNTSX  EQU   *-1                                                              
*                                                                               
*                                                                               
P        DC    CL133' '                                                         
CLTTAB   DS    3000C                                                            
*                                                                               
***************************************************************                 
*   PAID/BILLED ESTIMATES DON'T COPY TABLE                                      
*                                                                               
*                                                                               
***************************************************************                 
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
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048COPYH9CLT 02/15/02'                                      
         END                                                                    
