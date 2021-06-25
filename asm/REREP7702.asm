*          DATA SET REREP7702  AT LEVEL 073 AS OF 04/03/03                      
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
* APR02/03 (BU ) --- FAX AND EMAIL DATA DISPLAYED                   *           
*                                                                   *           
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
         MVI   PEDI,C'Y'           SET 'FOR EDI USE = YES'                      
         TM    RPTPFLG,X'20'       BLOCK EDI SET?                               
         BNO   PPLI0020            NO                                           
         MVI   PEDI,C'N'           SET 'FOR EDI USE = NO '                      
PPLI0020 EQU   *                                                                
         MVC   PPHONE,RPTPFONE                                                  
         MVC   PREP,RPTPREP                                                     
         MVC   POFF,RPTPOFF                                                     
         OC    RPTPLDAT,RPTPLDAT   ANY LEAVE DATE?                              
         BZ    PPLI0040            NO                                           
         GOTO1 DATCON,DMCB,(2,RPTPLDAT),(5,PLEVDATE)                            
PPLI0040 EQU   *                                                                
*                                                                               
***      B     PPLI0180                                                         
*                                                                               
         LA    R3,RPTPELEM         SET A(DESCRIPTOR ELEMENT)                    
PPLI0060 EQU   *                                                                
         CLI   0(R3),X'20'         EMAIL ELEMENT?                               
         BE    PPLI0080            YES                                          
         CLI   0(R3),0             END OF RECORD?                               
         BE    PPLI0120            YES - NO ELEMENT                             
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     PPLI0060            GO BACK FOR NEXT                             
PPLI0080 EQU   *                                                                
         USING RPTPEMEM,R3                                                      
         ZIC   RF,1(R3)            SET LENGTH OF ELEMENT                        
         SH    RF,=H'3'            SUB FOR EX + L(CONTROL)                      
         EX    RF,PPLI0100                                                      
***      MVC   PSECOND+PLEVDATE-PTPLINE(10),=C'EMAIL ADDR'                      
         B     PPLI0120                                                         
PPLI0100 MVC   PSECOND+PLEVDATE-PTPLINE(0),RPTPEMAL                             
*PLI0100 MVC   PSECOND+20(0),RPTPEMAL                                           
*                                                                               
         DROP  R3                                                               
*                                                                               
PPLI0120 EQU   *                                                                
         MVI   PFAXFLG,C'N'        SET 'FAX PREFERENCE = NO '                   
         LA    R3,RPTPELEM         SET A(DESCRIPTOR ELEMENT)                    
PPLI0140 EQU   *                                                                
         CLI   0(R3),X'21'         FAX   ELEMENT?                               
         BE    PPLI0160            YES                                          
         CLI   0(R3),0             END OF RECORD?                               
         BE    PPLI0180            YES - NO ELEMENT                             
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     PPLI0140            GO BACK FOR NEXT                             
PPLI0160 EQU   *                                                                
         USING RPTPFXEM,R3                                                      
         MVC   PFAX#(12),RPTPFXFX                                               
         MVI   PFAXFLG,C'Y'        SET 'FAX PREFERENCE = YES'                   
         TM    RPTPFXFG,X'20'      FAX PREFERENCE SET?                          
         BO    PPLI0180            YES                                          
         MVI   PFAXFLG,C'N'        SET 'FAX PREFERENCE = NO '                   
*                                                                               
         DROP  R3                                                               
*                                                                               
PPLI0180 EQU   *                                                                
         MVC   PSAL,RPTPSPER                                                    
         BAS   RE,GETNAME          NAME OVERLAYS PP RECORD                      
*                                     AT THIS POINT, IT'S GONE!                 
*                                                                               
         BAS   RE,DOREPORT         PRINT THE LINE                               
*                                                                               
PPSEQ    MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY,0                 
         B     PPNEXT                                                           
*                                                                               
PPX      XMOD1 1                                                                
*                                                                               
DOREPORT NTR1                                                                   
         GOTO1 REPORT                                                           
         CLC   MAXLINES,LINE                                                    
         BNE   DREP0020                                                         
         MVC   HEAD4+16(33),REPNAME                                             
DREP0020 EQU   *                                                                
         XIT1                                                                   
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
         DROP  R2,R5                                                            
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
         GETEL R6,34,ELCODE                                                     
*                                                                               
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
         DS    F                                                                
REPNAME  DS    CL33                                                             
ELCODE   DS    XL1                                                              
SAVEKEY  DS    CL(L'KEY)                                                        
STRTKEY  DS    CL(L'KEY)           *************************                    
IOAREA   DS    CL1000                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PTPLINE  DSECT                                                                  
         DS    CL1                                                              
PCODE    DS    CL3                                                              
         DS    CL2                                                              
PNAME    DS    CL20                                                             
         DS    CL2                                                              
PEDI     DS    CL1                                                              
         DS    CL3                                                              
PPHONE   DS    CL20                                                             
         DS    CL1                                                              
PREP     DS    CL2                                                              
         DS    CL2                                                              
POFF     DS    CL2                                                              
         DS    CL3                                                              
PSAL     DS    CL3                                                              
         DS    CL3                                                              
PSALN    DS    CL16                                                             
         DS    CL1                                                              
PLEVDATE DS    CL10                                                             
         DS    CL12                                                             
PFAX#    DS    CL16                                                             
         DS    CL5                                                              
PFAXFLG  DS    CL1                                                              
         ORG   PLEVDATE            ********************************             
PKEY     DS    CL32                ********************************             
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073REREP7702 04/03/03'                                      
         END                                                                    
