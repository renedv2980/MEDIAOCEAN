*          DATA SET REREP2302D AT LEVEL 005 AS OF 05/01/02                      
*PHASE RE2302D,*                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'MODULE TO CHANGE EFF DATE FOR INV RECS'                         
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2302 (RE2302) --- REP FILE MARKER                   *             
*                                                                 *             
*        THIS MODULE CHECKS THE FOLLOWING:                        *             
*              1. ANY EFF. START OR END DATE PRIOR TO 1980        *             
*              2. CHANGES THE EFF START DATE TO 1990              *             
*                                                                 *             
* --------------------------------------------------------------- *             
*******************************************************************             
*                                                                               
RE2302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE2302,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         XC    SAVEKEY,SAVEKEY                                                  
         XC    HDRKEY,HDRKEY                                                    
         XC    COUNT,COUNT                                                      
         XC    TOTAL,TOTAL                                                      
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         MVI   KEY,X'12'                                                        
         MVC   RINVKREP,=C'BL'                                                  
*                                                                               
PC10     DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,DHIGH                                                         
         B     PC20                                                             
*                                                                               
PCSEQ    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,DSEQ                                                          
*                                                                               
PC20     DS    0H                                                               
         LA    R6,KEY                                                           
*                                                                               
         CLI   KEY,X'12'                                                        
         BNE   PCX                                                              
         CLC   RINVKREP,=C'BL'                                                  
         BNE   PCSEQ                                                            
         CLI   RINVKSRC,0          ONLY HEADERS                                 
         BNE   PCSEQ                                                            
*                                                                               
PC25     DS    0H                                                               
         BAS   RE,DGETREC                                                       
*                                                                               
         LA    R6,IO                                                            
*                                                                               
         CLI   RINVKSTD,X'50'      START DATE YEAR = 1980                       
         BNE   PCSEQ                                                            
*&&DO                                                                           
         CLC   RINVPEFF(2),=XL2'A021' START DATE < 1980                         
         BNL   PCSEQ                                                            
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BNZ   PCSEQ                                                            
*&&                                                                             
PC100    DS    0H                                                               
         MVC   P(2),RINVKREP                                                    
         MVC   P+4(5),RINVKSTA                                                  
         MVC   P+11(4),RINVKINV                                                 
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,P+17)                                
*                                                                               
         GOTO1 REPORT                                                           
*&&DO                                                                           
         MVC   STDBIN,RINVKSTD     START DATE                                   
         MVI   STDBIN,X'5A'        DEFAULT TO 1990                              
         GOTO1 DATCON,DMCB,(3,STDBIN),(2,STDCOMP)                               
*                                                                               
         MVC   HDRKEY,KEY          SAVE AWAY KEY TO DELETE                      
         BAS   RE,ADDINV           ADD NEW INV                                  
         BAS   RE,DELINV           DELETE OLD INV                               
*&&                                                                             
PC200    L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
         B     PCSEQ                                                            
*                                                                               
PCX      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(13),=C'COUNT ======>'                                          
*                                                                               
         EDIT  COUNT,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(13),=C'TOTAL ======>'                                          
*                                                                               
         EDIT  TOTAL,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         GOTO1 REPORT                                                           
         B     EXIT                                                             
********************************************************************            
*        ADD INV FOR 1990                                                       
********************************************************************            
ADDINV   NTR1                                                                   
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
*                                                                               
ADDIHI   BAS   RE,DHIGH                                                         
         B     ADDI10                                                           
*                                                                               
ADDISEQ  BAS   RE,DSEQ                                                          
*                                                                               
ADDI10   DS    0H                                                               
         LA    R6,KEY                                                           
*                                                                               
         CLC   KEY(24),HDRKEY      SAME REP/STA/INV#/EFF                        
         BNE   ADDI200             DONE                                         
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         BAS   RE,DGETREC                                                       
*                                                                               
         MVC   RINVKSTD,STDBIN                                                  
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,DHIGH                                                         
*                                                                               
         CLC   KEY(27),KEYSAVE     DOES NEW INV FOR 1990 EXIST?                 
         BE    ADDI100                                                          
*                                                                               
         LA    R6,IO                                                            
         MVC   RINVKSTD,STDBIN                                                  
*                                                                               
         CLI   RINVKSRC,0          HEADER?                                      
         BNE   *+10                                                             
         MVC   RINVPEFF(2),STDCOMP                                              
*                                                                               
         DROP  R6                                                               
         MVI   ELCODE,X'EF'        GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   ADDI50                                                           
         USING RINVAEL,R6                                                       
*                                                                               
         OI    RINVAFLG,PRIOR80    THIS INV FROM EFF < 1980                     
         DROP  R6                                                               
*                                                                               
ADDI50   DS    0H                                                               
         LA    R6,IO                                                            
         USING REINVREC,R6                                                      
         BAS   RE,DADDREC                                                       
*                                                                               
         MVC   P+36(11),=CL11'<==== ADDED'                                      
         MVC   P(2),RINVKREP                                                    
         MVC   P+4(5),RINVKSTA                                                  
         MVC   P+11(4),RINVKINV                                                 
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,P+17)                                
         GOTO1 HEXOUT,DMCB,RINVKSRC,P+27,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,RINVKBK,P+31,2,=C'TOG'                               
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),SAVEKEY                                                  
*                                                                               
         BAS   RE,DHIGH                                                         
         BAS   RE,DGETREC                                                       
*                                                                               
         L     RF,TOTAL                                                         
         LA    RF,1(RF)                                                         
         ST    RF,TOTAL                                                         
*                                                                               
         B     ADDISEQ                                                          
*                                                                               
ADDI100  DS    0H                                                               
         MVC   P+36(12),=CL12'<==== EXISTS'                                     
         MVC   P(2),RINVKREP                                                    
         MVC   P+4(5),RINVKSTA                                                  
         MVC   P+11(4),RINVKINV                                                 
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,P+17)                                
         GOTO1 HEXOUT,DMCB,RINVKSRC,P+27,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,RINVKBK,P+31,2,=C'TOG'                               
*                                                                               
         GOTO1 REPORT                                                           
         B     ADDISEQ                                                          
*                                                                               
ADDI200  DS    0H                  DONE ADDING ALL NEW INV                      
         MVC   P(17),=C'ADDED ALL NEW INV'                                      
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY             RESTORE SEQUENCE                             
         MVC   KEY(27),HDRKEY                                                   
         BAS   RE,DHIGH                                                         
*                                                                               
ADDINVX  DS    0H                                                               
         B     XIT                                                              
*                                                                               
********************************************************************            
*        DELETE INV < 1990                                                      
********************************************************************            
DELINV   NTR1                                                                   
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
*                                                                               
DELIHI   BAS   RE,DHIGH                                                         
         B     DELI10                                                           
*                                                                               
DELISEQ  BAS   RE,DSEQ                                                          
*                                                                               
DELI10   DS    0H                                                               
         LA    R6,KEY                                                           
*                                                                               
         CLC   KEY(24),HDRKEY      SAME REP/STA/INV#/EFF                        
         BNE   DELI200             DONE                                         
*                                                                               
         BAS   RE,DGETREC                                                       
*                                                                               
         MVI   KEY+27,X'80'        MARK BOTH KEY & REC DELETED                  
         MVI   IO+29,X'80'                                                      
*                                                                               
         BAS   RE,DDMWRT                                                        
         BAS   RE,DPUTREC                                                       
*                                                                               
         MVC   P+35(13),=CL13'<==== DELETED'                                    
         MVC   P(2),RINVKREP                                                    
         MVC   P+4(5),RINVKSTA                                                  
         MVC   P+11(4),RINVKINV                                                 
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,P+17)                                
         GOTO1 HEXOUT,DMCB,RINVKSRC,P+27,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,RINVKBK,P+31,2,=C'TOG'                               
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,TOTAL                                                         
         LA    RF,1(RF)                                                         
         ST    RF,TOTAL                                                         
*                                                                               
         B     DELISEQ                                                          
*                                                                               
DELI200  DS    0H                  DONE ADDING ALL NEW INV                      
         MVC   P(19),=C'DELETED ALL OLD INV'                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         XC    KEY,KEY             RESTORE SEQUENCE                             
         MVC   KEY(27),HDRKEY                                                   
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE     SHOULD BE OLD INV MARKED DELETED             
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   KEY+27,X'80'        SHOULD BE MARKED DELETED                     
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
DELINVX  DS    0H                                                               
         B     XIT                                                              
*                                                                               
DHIGH    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
DSEQ     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,DMRSEQ),REPDIR,KEY,KEY,0                         
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
DGETREC  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,GETREC),REPFILE,KEY+28,IO,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
DADDREC  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,ADDREC),REPFILE,KEY+28,IO,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
DPUTREC  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,PUTREC),REPFILE,KEY+28,IO,DMWORK                 
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
DDMWRT   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(0,DMWRT),REPDIR,KEY,KEY,0                          
         TM    DMCB+8,X'FD'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
*                                                                               
EXIT     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
         DC    CL4'DATA'                                                        
COUNT    DS    F                                                                
TOTAL    DS    F                                                                
SAVEKEY  DS    CL27                                                             
HDRKEY   DS    CL27                RECS TO DELETE AFTER DATE CHANGE             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(RE2302,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
RELO     DS    A                                                                
FOOTSW   DS    CL1                 Y=PRINTING FOOTLINES                         
*                                                                               
ELCODE   DS    XL1                                                              
*                                                                               
STDBIN   DS    XL3                                                              
STDCOMP  DS    XL2                                                              
         DC    CL8'XXXXXXXX'                                                    
IO       DS    XL2000                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
*  INCLUDE REGENALL1                                                            
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REGENMKG                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREP2302D05/01/02'                                      
         END                                                                    
