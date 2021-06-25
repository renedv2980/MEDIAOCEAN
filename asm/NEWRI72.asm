*          DATA SET NEWRI72    AT LEVEL 055 AS OF 05/01/02                      
*PHASE T32072A,+0                                                               
*INCLUDE CLUNPK                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE TIMEOUT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T32072 - CABLE UPLOAD RECAP REPORT'                             
T32072   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**UPLD**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          ANETWS2=WORKING STORAGE                      
         USING WORKD,R7                                                         
         L     R6,ANETWS4                                                       
         USING NDDEMBLK,R6                                                      
*                                                                               
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE                                                                  
EDITMOD  NTR1                                                                   
*                                                                               
         L     R4,NBAIO                                                         
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,T320FFD+10                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMREAD'),=CL8'CTFILE',(R4),(R4)             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,CTIDATA                                                       
CT10     CLI   0(RE),X'02'                                                      
         BE    CT20                                                             
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BNE   CT10                                                             
         DC    H'0'                                                             
CT20     MVC   WAGYID,2(RE)                                                     
         MVC   WAGYNM,USERNAME                                                  
*                                                                               
* EDIT THE DATE FIELD                                                           
*                                                                               
         LA    R2,SPLDATH                                                       
         CLI   5(R2),0                                                          
         BE    ED050                                                            
         CLC   8(5,R2),=CL5'TODAY'                                              
         BE    ED050                                                            
         NETGO NVSTRDAT,DMCB                                                    
         GOTO1 DATCON,DMCB,(0,NBSELSTR),(3,WTODAY)                              
         GOTO1 DATCON,DMCB,(3,WTODAY),(8,WDATE)                                 
         B     ED100                                                            
ED050    GOTO1 DATCON,DMCB,(5,DUB),(3,WTODAY)                                   
         GOTO1 DATCON,DMCB,(3,WTODAY),(8,WDATE)                                 
         DROP  R4                                                               
*                                                                               
* EDIT THE CLIENT FIELD                                                         
*                                                                               
ED100    LA    R2,SPLCLIH                                                       
         CLI   5(R2),0                                                          
         BE    ED150                                                            
         NETGO NVCLI,DMCB                                                       
         B     EDEX                                                             
ED150    CLI   TWAWHEN,2           IS REQUEST SOON                              
         BNE   EDEX                                                             
*                                                                               
         MVI   ERROR,MISSING                                                    
         B     EDERR                                                            
*                                                                               
EDEX     B     XIT                                                              
*                                                                               
EDERR    GOTO1 ERREX,DMCB                                                       
         SPACE 1                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
REPMOD   NTR1                                                                   
*  SEE IF SAME REQUEST AS PREVIOUS                                              
         L     R5,BOOKVAL                                                       
         USING TWAHLD,R5                                                        
         CLC   TWAAGYID,NBACTAM                                                 
         BNE   RP050                                                            
         CLC   TWACLI,NBACTCLI                                                  
         BNE   RP050                                                            
         CLC   TWADATE,WTODAY                                                   
         BE    RPX                                                              
*  SAVE CURRENT REQUEST INFO                                                    
RP050    GOTO1 =V(PRNTBL),DMCB,=C'TWA',TWAAGYID,C'DUMP',6,=C'1D'                
         GOTO1 =V(PRNTBL),DMCB,=C'AGY',NBACTAM,C'DUMP',1,=C'1D'                 
         GOTO1 =V(PRNTBL),DMCB,=C'CLI',NBACTCLI,C'DUMP',2,=C'1D'                
         GOTO1 =V(PRNTBL),DMCB,=C'DTE',WTODAY,C'DUMP',3,=C'1D'                  
         B     RPX                                                              
         MVC   TWAAGYID,NBACTAM                                                 
         MVC   TWACLI,NBACTCLI                                                  
         MVC   TWADATE,WTODAY                                                   
         GOTO1 =V(PRNTBL),DMCB,=C'TWA',TWAAGYID,C'DUMP',6,=C'1D'                
         DROP  R5                                                               
*                                                                               
         LA    R5,PRTTAB                                                        
         MVI   0(R5),X'FF'                                                      
         MVC   NBDTADSP,=H'27'                                                  
         LA    R2,P                                                             
*                                                                               
* - READ UNIT RECORDS FILL IN TABLE                                             
*                                                                               
         NETGO NVSETUNT,DMCB                                                    
         LA    R4,KEY                                                           
         USING NURECD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM(1),NBACTAM                                                 
         MVC   NUKCLT(2),NBACTCLI                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR  ',KEY,KEY,0                   
         B     RP200                                                            
RP150    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'UNTDIR  ',KEY,KEY,0                   
RP200    CLC   KEY(2),KEYSAVE                                                   
         BNE   RP300                                                            
         OC    NBACTCLI,NBACTCLI   WAS CLIENT REQUESTED                         
         BZ    *+14                                                             
         CLC   KEY+2(2),KEYSAVE+2                                               
         BNE   RP300                                                            
         LA    R4,KEY                                                           
         TM    NUKPROG+3,X'F0'                                                  
         BNO   RP150                                                            
         TM    NUKPROG+4,X'F0'                                                  
         BNO   RP150                                                            
         LA    R3,KEY+21                                                        
         L     R4,AIO1                                                          
         LA    R5,DMWORK                                                        
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'UNTFILE ',(R3),(R4),(R5)          
*                                                                               
*  IF RECORD WAS CREATED VIA CABLE UPLOAD AND WAS ADDED                         
*  TODAY.  ADD THE PERTINENT INFO TO THE PRINT TABLE.                           
*  THE PRINT TABLE IS LOCATED IN PRTTAB.                                        
*                                                                               
         L     R3,AIO1                                                          
         USING NUSDRD,R3                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   RP150                                                            
         TM    NUSDST3,X'04'      IS UNIT A CABLE UPLOAD                        
         BZ    RP150                                                            
         DROP  R3                                                               
*                                                                               
         L     R3,AIO1                                                          
         USING NUACTD,R3                                                        
         MVI   ELCODE,X'99'                                                     
         BAS   RE,GETEL                                                         
         BNE   RP150                                                            
         CLC   NUACTADT,WTODAY     WAS RECORD CREATED TODAY                     
         BNE   RP150                                                            
         DROP  R3                                                               
*                                                                               
         MVC   WORK(2),NUKCLT                                                   
         MVC   WORK+2(1),NUKEST                                                 
         MVC   WORK+3(4),NUKNET                                                 
         MVC   WORK+7(1),NUPACK                                                 
*                                                                               
         SR    R3,R3                                                            
         LA    R5,PRTTAB                                                        
*                                                                               
RP230    CLI   0(R5),X'FF'                                                      
         BE    RP260                                                            
         CLC   0(8,R5),WORK        IS ENTRY ALREADY IN TABLE                    
         BE    RP150               YES READ NEXT RECORD                         
         LA    R3,1(R3)                                                         
         LA    R5,8(R5)                                                         
         B     RP230                                                            
*                                                                               
RP260    C     R3,=F'1000'         MAX STORAGE SIZE OF PRTTAB                   
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   0(8,R5),WORK                                                     
         MVI   8(R5),X'FF'                                                      
         B     RP150                                                            
*                                                                               
*  PRINT OUT THE DATA STORED IN PRTTAB                                          
*                                                                               
RP300    LA    R5,PRTTAB                                                        
*                                                                               
RP350    CLI   0(R5),X'FF'                                                      
         BE    RPX                                                              
         MVC   WORK(2),0(R5)                                                    
         GOTO1 =V(CLUNPK),DMCB,WORK,P+6                                         
         EDIT  (B1,2(R5)),(3,P+27),ALIGN=LEFT     EST NUMBER TO PRINT           
         MVC   P+47(4),3(R5)       NETWORK TO PRINT                             
         EDIT  (B1,7(R5)),(3,P+64),ALIGN=LEFT     PKG NUMBER TO PRINT           
         BAS   RE,SPOOLIT                                                       
*                                                                               
         LA    R5,8(R5)                                                         
         B     RP350                                                            
RPX      B     XIT                                                              
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
SPOOLIT  NTR1                                                                   
         MVI   SPACING,3                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
         GETEL (R3),NBDTADSP,ELCODE                                             
*                                                                               
         EJECT                                                                  
MYSPECS  DS    0F                                                               
         SSPEC H1,1,C'MEDIA     CABLE T.V.'                                     
         SSPEC H1,51,C'CABLE RECAP REPORT'                                      
         SSPEC H2,51,C'------------------'                                      
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         DC    X'00'                                                            
         SPACE                                                                  
*                                                                               
HOOK     NTR1                                                                   
         MVC   H2(4),=CL4'DATE'                                                 
         MVC   H2+10(8),WDATE                                                   
         MVC   H8+5(6),=C'CLIENT'                                               
         MVC   H9+5(6),=C'------'                                               
         MVC   H8+25(8),=C'ESTIMATE'                                            
         MVC   H9+25(8),=C'--------'                                            
         MVC   H8+45(7),=C'NETWORK'                                             
         MVC   H9+45(7),=C'-------'                                             
         MVC   H8+62(7),=C'PACKAGE'                                             
         MVC   H9+62(7),=C'-------'                                             
*        XC    DMCB(16),DMCB                                                    
         LA    R4,RELO                                                          
         S     R4,RELO                                                          
*        GOTO1 =V(TICTOC),DMCB,C'SGET',0,RR=R4                                  
*        MVC   FULL,0(R1)                                                       
*        AP    FULL,=P'80000'                                                   
*        CP    FULL,=P'240000'                                                  
*        BNH   DOTIME                                                           
*        SP    FULL,=P'240000'                                                  
* DOTIME LA    R4,RELO                                                          
*        S     R4,RELO                                                          
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         GOTO1 =V(TIMEOUT),DMCB,(X'03',DUB),(X'81',WORK),RR=R4                  
         CLI   WORK+6,X'F0'        DROP LEADING ZERO                            
         BNE   *+8                                                              
         MVI   WORK+6,X'40'                                                     
         MVC   H4+117(2),WORK+6                                                 
         MVI   H4+119,C'.'         SET DECIMAL                                  
         MVC   H4+120(4),WORK+8                                                 
         B     XIT                                                              
RELO     DC    A(*)                                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         DC    CL8'**PRTTAB*'                                                   
PRTTAB   DC    8000X'00'                                                        
*                                                                               
*                                                                               
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
WAGYID   DS    CL10                AGY ID (PASS ONLY 1ST 4 CHAR)                
WAGYNM   DS    CL33                AGY NAME                                     
WMEDIA   DS    CL1                 MEDIA                                        
WTODAY   DS    CL3                 DATE                                         
WDATE    DS    CL8                 DATE (FOR HEADLINE)                          
WCLT     DS    CL3                 CLIENT                                       
WCLTNM   DS    CL40                CLIENT NAME                                  
WNET     DS    CL4                 NETWORK                                      
WEST     DS    CL3                 ESTIMATE                                     
WESTNM   DS    CL40                ESTIMATE NAME                                
WESTRT   DS    CL8                 EST START DATE                               
WEEND    DS    CL8                 EST END DATE                                 
WPAK     DS    CL3                 PACKAGE                                      
WPAKNM   DS    CL40                PACKAGE NAME                                 
WVPHIMP  DS    CL1                 VPH/IMP BASE                                 
WDEMOS   DS    CL120               DEMOS(NO NAD,NO HOMES,NO MODIFIER)           
WMALLOC  DS    CL3                 MASTER ALLOCATION                            
WPRODNAM DS    CL20                MASTER ALLOCATION PRODUCT NAME               
WCOMMENT DS    CL1000              COMMENTS                                     
WLENE    EQU   *-WAGYID                                                         
PAKRGS   DS    CL1                 PACKAGE RANGE START                          
PAKRGE   DS    CL1                 PACKAGE RANGE END                            
*                                                                               
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIEED                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         EJECT                                                                  
       ++INCLUDE DDFAXINFOD                                                     
TWAHLD   DSECT                                                                  
TWAAGYID DS    CL1                  SAVE AGENCY                                 
TWACLI   DS    CL2                  SAVE CLIENT                                 
TWADATE  DS    CL3                  SAVE DATE                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055NEWRI72   05/01/02'                                      
         END                                                                    
