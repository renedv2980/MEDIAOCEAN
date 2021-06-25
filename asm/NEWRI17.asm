*          DATA SET NEWRI17    AT LEVEL 016 AS OF 05/01/02                      
*PHASE T32017A                                                                  
         TITLE 'T32017 - CASHFLOW REPORT '                                      
T32017   CSECT                                                                  
         SPACE 3                                                                
*                                                                               
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**CASH**                                                       
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING T32017+4096,R6                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
         L     R7,ANETWS1                                                       
         ST    R7,NBACLI           ANETWS1 = CLIENT REC                         
         A     R7,=F'250'          CLIENT REC NOW = 1250                        
         ST    R7,ANETWS2          ANETWS2+250 = WORKING STORAGE                
         USING WORKD,R7                                                         
*              EDITING REQUEST                                                  
         SPACE 3                                                                
         CLI   MODE,PRINTREP                                                    
         BE    GETS                                                             
         CLI   MODE,VALKEY                                                      
         BNE   XIT                                                              
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         SPACE 1                                                                
*                                  CLIENT VALIDATION                            
         LA    R2,SPLCLIH                                                       
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
         SPACE 1                                                                
*                                  PRODUCT VALIDATION                           
         LA    R2,SPLPROH                                                       
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
         SPACE 1                                                                
*                                  ESTIMATE VALIDATION                          
         LA    R2,SPLESTH                                                       
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         OI    SPLESTNH+6,X'80'                                                 
         SPACE 1                                                                
*                                  NETWORK VALIDATION                           
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB,SAVMKT                                             
         SPACE 1                                                                
*                                  DAYPART VALIDATION                           
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN                                               
         OI    SPLDPTNH+6,X'80'                                                 
         MVC   DPFILT,NBSELDP                                                   
         SPACE 1                                                                
*                                  PACKAGE VALIDATION                           
         LA    R2,SPLPAKH                                                       
         NETGO NVPAKLOK,DMCB,SPLPAKN                                            
         OI    SPLPAKNH+6,X'80'                                                 
         SPACE 1                                                                
*                                  START DATE                                   
         LA    R2,SPLRSTRH                                                      
         NETGO NVSTRDAT,DMCB                                                    
         SPACE 1                                                                
*                                  END DATE                                     
         LA    R2,SPLRENDH                                                      
         NETGO NVENDDAT,DMCB                                                    
         SPACE 1                                                                
         LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BAS   RE,VALIOPT                                                       
         SPACE 1                                                                
         LA    R2,SPLCLIH                                                       
         B     XMOD                                                             
         SPACE 1                                                                
EDERR    GOTO1 ERREX,DMCB                                                       
         SPACE 1                                                                
XMOD     XIT1  REGS=(R2)                                                        
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 3                                                                
VALIOPT  NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),0                                    
         LA    R3,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    OPTERR                                                           
         SPACE 1                                                                
VALIOPT2 CLC   12(5,R3),=C'INDEX'                                               
         BNE   VALIOPT4                                                         
         MVC   INDEXOPT,22(R3)                                                  
         B     VALIOPTX                                                         
         SPACE 1                                                                
VALIOPT4 CLC   12(6,R3),=C'ACTUAL'                                              
         BNE   VALIOPT6                                                         
         MVC   ACTOPT,22(R3)                                                    
         B     VALIOPTX                                                         
         SPACE 1                                                                
VALIOPT6 CLC   12(4,R3),=C'WEEK'                                                
         BNE   VALIOPT8                                                         
         CLC   22(3,R3),=C'SEP'                                                 
         BNE   VALIOPT8                                                         
         MVI   WEEKSEP,C'Y'                                                     
         B     VALIOPTX                                                         
         SPACE 1                                                                
VALIOPT8 B     OPTERR                                                           
         SPACE 1                                                                
VALIOPTX LA    R3,32(R3)                                                        
         BCT   R4,VALIOPT2                                                      
         B     XIT                                                              
         SPACE 1                                                                
OPTERR   MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         EJECT                                                                  
*              CONTROL OF REPORT                                                
         SPACE 3                                                                
GETS     DS    0H                                                               
         MVI   NBSELUOP,C'A'       USE ACTUAL SCHEDULE                          
         OI    NBSPLOPT,X'C0'      HANDLE SPLIT                                 
         MVI   NBDATA,C'U'         GET UNITS                                    
         SPACE 1                                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   GETUNIT2                                                         
         BAS   RE,UNIT                                                          
         B     GETUNIT                                                          
         SPACE 1                                                                
GETUNIT2 CLI   NBMODE,NBREQLST                                                  
         BNE   GETUNIT                                                          
         BAS   RE,REPORT                                                        
         B     XIT                                                              
*                                                                               
PROCERR  DC    H'0'                                                             
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SREC                                         
         B     XIT                                                              
         EJECT                                                                  
*              POST UNITS                                                       
         SPACE 3                                                                
UNIT     NTR1                                                                   
         XC    SREC,SREC           CLEAR                                        
* - FILL IN UNIT DATA                                                           
         MVC   SCLT,NBACTCLI                                                    
         BAS   RE,GETPRD           SETS SPROD (CL3)                             
         MVC   SEST,NBACTEST                                                    
         MVC   SNET,NBACTNET                                                    
         MVC   SDPT,NBACTDP                                                     
         MVC   SPACK,NBPACK                                                     
         MVC   SDATNUM,NBACTDAT                                                 
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'10'        GET BILLING ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   UNT30                                                            
UNT50    BAS   RE,PUTSORT                                                       
         B     XIT                                                              
         SPACE                                                                  
*                                                                               
*  - GET 3 BYTE PROD CODE FROM CLIENT REC IN ANETWS1                            
GETPRD   NTR1                                                                   
         L     R1,ANETWS1                                                       
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220                                                           
GTP10    CLC   NBSPLPRN,3(R2)                                                   
         BE    GTP20                                                            
         LA    R1,4(R1)                                                         
         BCT   R2,GTP10                                                         
         MVC   SPROD,=C'***'                                                    
         B     GTPX                                                             
GTP20    MVC   SPROD,0(R2)                                                      
*                                                                               
GTPX     B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*              ROUTINES TO CONTROL REPORT                                       
         SPACE 3                                                                
REPORT   NTR1                                                                   
REPX     B     XIT                                                              
*              HEADLINE HOOK - BOXES AND HEADLINES                              
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R4,ABOX             SET UP BOXES IF OFF LINE                     
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    HOOK1                                                            
         MVI   H12+131,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXROWS+08,C'T'                                                  
         MVI   BOXROWS+12,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+8,C'C'                                                   
         MVI   BOXCOLS+130,C'R'                                                 
         B     XIT                                                              
         SPACE 1                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 1                                                                
         EJECT                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEDSPECS SSPEC H1,1,C'CASH FLOW REPORT'                                         
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,50,PERIOD                                                     
         SSPEC H5,99,PAGE                                                       
         SSPEC H10,02,C'DAYPART DATE  NET    PROGRAM NAME'                      
         SSPEC H10,38,C'DAY    TIME     LEN   COST'                             
         SSPEC H10,068,C'-----------ESTIMATED-----------'                       
         SSPEC H10,100,C'------------ACTUAL-------------'                       
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,30,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE                                                  
WORKD    DSECT                                                                  
         SPACE 3                                                                
         DS    0D                                                               
SREC     DS    CL200              SORT RECORD                                   
         ORG   SREC                                                             
SKEY     DS    CL30               KEY FIELDS                                    
         ORG   SKEY                                                             
SCLT     DS    CL3                                                              
SPROD    DS    CL3                                                              
SEST     DS    XL1                                                              
SNET     DS    CL4                                                              
SDPT     DS    CL1                                                              
SPACK    DS    XL1                                                              
SDATNUM  DS    XL2                 UNIT DATE                                    
         DS    CL15                SPARE                                        
SKEYEND  EQU   *                                                                
*                                 DATA FIELDS                                   
STYPE    DS    CL1                A=BILLING DATA/B=CHECK DATE                   
SACTDAT  DS    XL2                BILL OR CHECK DATE                            
SINVNO   DS    CL6                INVOICE NUMBER                                
SBILLED  DS    XL4                BILLED (FROM BILLING RECORD)                  
SPAID    DS    XL4                PAID   (FROM CLEARANCE RECORD)                
SCHECKNO DS    CL6                CHECK NUMBER (CLEARANCE RECORD)               
SDISDAYS DS    PL4                DAYS TO DISBURSE                              
SDALYB   DS    PL8                DAILY BALANCE                                 
         DS    CL135              SPARE                                         
*                                                                               
WORKEND  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIF7D                                                       
*                                                                               
*                                                                               
*                                                                               
*                                  SPGENPRD                                     
*                                  DDBIGBOX                                     
*                                  NEGENINCLS                                   
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NEGENINCLS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016NEWRI17   05/01/02'                                      
         END                                                                    
