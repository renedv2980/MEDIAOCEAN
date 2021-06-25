*          DATA SET REREP7702S AT LEVEL 049 AS OF 05/01/02                      
*PHASE RE7702C,+0                                                               
***********************************************************************         
*                                                                               
* 05MAY92 (SKU) DATE OF BIRTH.  NOTE: USES OWN I/O FOR LISTING                  
*                                                                               
* 09JUN92 (SKU) IF IRNY PRINT IN HEADER 'REPRESENTATIVE INTEREP'                
*                                                                               
* 31MAR95 (BU ) ADD 'LEAVE DATE' TO PRINTOUT                                    
*                                                                               
* 13DEC95 (SKU) IF KRG PRINT IN HEADER 'REPRESENTATIVE KRG                      
*                                                                   *           
* JAN28/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
***********************************************************************         
         TITLE 'POINT PERSON LISTING PROGRAM'                                   
RE7702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE7702,RR=R5                                                 
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*              CHECK MODE SETTINGS                                              
         CLI   MODE,REQFRST                                                     
         BNE   PPX                                                              
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         BAS   RE,ISINTREP         IF IRNY PRINT INTEREP                        
         BNZ   PPMAIN10                                                         
         MVC   REPNAME(7),=C'INTEREP'                                           
         B     PPSTART                                                          
*                                                                               
PPMAIN10 DS    0H                                                               
         BAS   RE,ISKRG            IF KRG PRINT KRG                             
         BNZ   PPGETREP                                                         
         MVC   REPNAME(7),=C'KRG    '                                           
         B     PPSTART                                                          
*                                                                               
PPGETREP DS    0H                                                               
*****    LA    R6,IOAREA                                                        
*****    USING RREPREC,R6                                                       
         XC    IOAREA(L'KEY),IOAREA                                             
         MVI   RREPKEY,X'01'                                                    
         MVC   RREPKREP,QREP                                                    
         MVC   KEY,IOAREA                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         CLC   KEY(24),KEYSAVE                                                  
         BNE   PPSTART                                                          
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK            
         MVC   REPNAME,RREPNAME                                                 
*****    DROP  R6                                                               
*                                                                               
PPSTART  DS    0H                                                               
         MVC   HEAD4+16(33),REPNAME                                             
         LA    R6,IOAREA                                                        
         USING RPTPREC,R6                                                       
         XC    IOAREA(L'KEY),IOAREA                                             
         MVI   RPTPKTYP,RPTPKTYQ                                                
         MVC   RPTPKREP,QREP                                                    
         MVC   KEY,IOAREA                                                       
         MVC   KEYSAVE,KEY                                                      
         MVC   STRTKEY,KEY         *************************                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
PPNEXT   CLC   KEY(24),STRTKEY     ***********************                      
         BNE   PPX                                                              
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK            
*                                                                               
         CLC   QOFFICE,SPACES      OFFICE FILTER??                              
         BE    PPLINE                                                           
         CLC   QOFFICE,RPTPOFF                                                  
         BNE   PPSEQ                                                            
*                                                                               
PPLINE   LA    R2,P                                                             
         USING PTPLINE,R2                                                       
         MVC   PCODE,RPTPKREC                                                   
         MVC   PNAME,RPTPNAME                                                   
         MVC   PPHONE,RPTPFONE                                                  
         MVC   PREP,RPTPREP                                                     
         MVC   POFF,RPTPOFF                                                     
         OC    RPTPLDAT,RPTPLDAT   ANY LEAVE DATE?                              
         BZ    PPLI0020            NO                                           
         GOTO1 DATCON,DMCB,(2,RPTPLDAT),(5,PLEVDATE)                            
PPLI0020 EQU   *                                                                
         MVC   PSAL,RPTPSPER                                                    
         BAS   RE,GETNAME          NAME OVERLAYS PP RECORD                      
*                                     AT THIS POINT, IT'S GONE!                 
*                                                                               
         GOTO1 REPORT                                                           
         CLC   MAXLINES,LINE                                                    
         BNE   PPSEQ                                                            
         MVC   HEAD4+16(33),REPNAME                                             
*                                                                               
PPSEQ    MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY,0                 
         B     PPNEXT                                                           
*                                                                               
PPX      XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* GET SUB REP SALESPERSON NAME                                                  
***********************************************************************         
GETNAME  NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
         LA    R5,IOAREA                                                        
         USING RSALREC,R5                                                       
         XC    IOAREA(L'KEY),IOAREA                                             
         MVI   RSALKTYP,6                                                       
         MVC   RSALKREP,PREP                                                    
         MVC   RSALKSAL,PSAL                                                    
         MVC   KEY,IOAREA                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GNAMEX                                                           
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK            
*                                                                               
         MVC   PSALN,RSALNAME                                                   
*                                                                               
GNAMEX   DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY,0                     
         XIT1                                                                   
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE IF INTEREP                                                
***********************************************************************         
ISINTREP NTR1                                                                   
         LA    R2,REPLIST                                                       
         LA    R3,L'REPLIST                                                     
*                                                                               
ISINTR10 CLC   QREP,0(R2)                                                       
         BE    YES                                                              
         LA    R2,2(R2)                                                         
         BCT   R3,ISINTR10                                                      
         B     NO                                                               
*                                                                               
NO       LA    R1,1                                                             
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XIT1                                                                   
*                                                                               
REPLIST  DS    0CL15                                                            
         DC    C'TO',C'I1',C'HN',C'DI',C'GP',C'MG',C'I8',C'I9'                  
         DC    C'SJ',C'I2'                                                      
         DC    C'RM',C'D4',C'IF',C'S1',C'CM'  ******* SM *******                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE IF KRG                                                    
***********************************************************************         
ISKRG    NTR1                                                                   
         LA    R2,KRGLIST                                                       
         LA    R3,L'KRGLIST                                                     
*                                                                               
ISKRG10  CLC   QREP,0(R2)                                                       
         BE    YES                                                              
         LA    R2,2(R2)                                                         
         BCT   R3,ISKRG10                                                       
         B     NO                                                               
*                                                                               
KRGLIST  DS    0CL6                                                             
         DC    C'BF',C'EA',C'CR',C'KF',C'KU',C'K4'                              
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
         DS    F                                                                
REPNAME  DS    CL33                                                             
SAVEKEY  DS    CL(L'KEY)                                                        
STRTKEY  DS    CL(L'KEY)           *************************                    
IOAREA   DS    CL1000                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PTPLINE  DSECT                                                                  
         DS    CL1                                                              
PCODE    DS    CL3                                                              
         DS    CL4                                                              
PNAME    DS    CL20                                                             
         DS    CL4                                                              
PPHONE   DS    CL20                                                             
         DS    CL4                                                              
PREP     DS    CL2                                                              
         DS    CL4                                                              
POFF     DS    CL2                                                              
         DS    CL4                                                              
PSAL     DS    CL3                                                              
         DS    CL2                                                              
PSALN    DS    CL20                                                             
         DS    CL2                                                              
PLEVDATE DS    CL10                                                             
         ORG   PLEVDATE            ********************************             
PKEY     DS    CL32                ********************************             
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049REREP7702S05/01/02'                                      
         END                                                                    
