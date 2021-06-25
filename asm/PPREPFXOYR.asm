*          DATA SET PPREPFXOYR AT LEVEL 001 AS OF 04/15/99                      
*                                                                               
*     (04/99 - CONVERTS YNR (YN) & ARMYP (AR) ACC OFFICES)                      
*                                                                               
*        QOPT5   N= DON'T MARK FILE (EVEN IF WRITE=YES)                         
*                                                                               
*                                                                               
*PHASE PP0202Y,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM TO FIX OFFICE CODES'                    
         PRINT NOGEN                                                            
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,30                                                      
*                                                                               
         CLI   QOPT5,C'N'                                                       
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      AGENCY,MEDIA                                 
         MVI   KEY+3,X'02'         CLTREC                                       
AGYC2    GOTO1 HIGH                                                             
         B     AGYC4                                                            
*                                                                               
AGYC3    DS    0H                                                               
         GOTO1 SEQ                                                              
AGYC4    DS    0H                                                               
         CLC   KEY(3),QAGENCY     MATCH AGENCY/MEDIA/CLTREC TYPE                
         BNE   EXIT                                                             
         CLI   KEY+3,X'02'                                                      
         BNE   EXIT                                                             
*                                                                               
AGYC6    DS    0H                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
*         FIRST CHECK FOR CLIENTS CHANGING BOTH PCLTOFF AND PCLTAOFC            
*                                                                               
AGYC8    LA    R1,CLTTAB                                                        
         LA    R0,CLTTABN          NUMBER OF TABLE ENTRIES                      
*                                                                               
LOOP01   CLC   PCLTKCLT,0(R1)      CLIENT ON TABLE ?                            
         BE    PRT10               YES - CONVERT CLIENT AND ACC OFFICES         
         LA    R1,6(R1)            BUMP TO NEXT CLIENT ENTRY                    
         BCT   R0,LOOP01                                                        
*                                                                               
*         IF CLIENT NOT FOUND FOR CONVERSION CONVERT ONLY ACC OFFICE            
*                                                                               
AGYC20   LA    R1,OFFTAB                                                        
         LA    R0,OFFTABN          NUMBER OF TABLE ENTRIES                      
*                                                                               
         MVC   OFCTEST,PCLTAOFC                                                 
         CLI   PCLTAOFC,C' '       ACC OFFICE PRESENT ?                         
         BH    LOOP10              YES - USE IT FOR SEARCH                      
         MVC   OFCTEST,PCLTOFF     NO - USE MEDIA OFFICE                        
*                                                                               
LOOP10   CLC   OFCTEST,2(R1)                                                    
         BL    ERR                                                              
         BE    PRT                                                              
         LA    R1,3(R1)                                                         
         BCT   R0,LOOP10                                                        
*                                                                               
ERR      MVC   P(16),=C'COULD NOT CONVERT'                                      
         MVC   P+17(4),=C'AGY='                                                 
         MVC   P+21(3),PCLTKAGY                                                 
         MVC   P+28(4),=C'CLT='                                                 
         MVC   P+32(3),PCLTKCLT                                                 
         MVC   P+38(8),=C'OLD OFC='                                             
         MVC   P+46(1),PCLTOFF                                                  
         MVC   P+50(13),=C'OLD ACCO/AGY='                                       
         MVC   P+64(2),PCLTAOFC                                                 
         MVI   P+66,C'/'                                                        
         MVC   P+67(2),PCLTACCA                                                 
*                                                                               
         BAS   RE,RPRT                                                          
         B     AGYC3                                                            
*                                                                               
PRT      DS    0H                  ACC OFFICE ONLY CONVERTED                    
         MVC   P+1(3),PCLTKAGY     "OLD" DATA                                   
         MVC   P+6(3),PCLTKCLT                                                  
         MVC   P+13(1),PCLTOFF                                                  
         MVC   P+20(2),PCLTAOFC                                                 
         MVC   P+29(2),PCLTACCA                                                 
*                                                                               
*******  XC    PCLTACCA,PCLTACCA                                                
         MVC   PCLTAOFC,0(R1)   SET PCLTAOFC FROM OFFTAB                        
*******  MVC   PCLTACCA,=C'XX'  NO ACC AGENCY REQUIRED                          
         MVC   P+37(1),PCLTOFF     "NEW" DATA                                   
         MVC   P+44(2),PCLTAOFC                                                 
         MVC   P+53(2),PCLTACCA                                                 
         B     PRT50                                                            
*                                                                               
PRT10    DS    0H              BOTH MEDIA AND ACC OFFICES CONVERTED             
         MVC   P+1(3),PCLTKAGY     "OLD" DATA                                   
         MVC   P+6(3),PCLTKCLT                                                  
         MVC   P+13(1),PCLTOFF                                                  
         MVC   P+20(2),PCLTAOFC                                                 
         MVC   P+29(2),PCLTACCA                                                 
*                                                                               
*****    XC    PCLTACCA,PCLTACCA                                                
         MVC   PCLTAOFC,3(R1)   SET PCLTAOFC FROM CLTTAB                        
         MVC   PCLTOFF,5(R1)    SET PCLTOFF  FROM CLTTAB                        
*****    MVC   PCLTACCA,=C'XX'  NO ACC AGENCY REQUIRED                          
         MVC   P+37(1),PCLTOFF     "NEW" DATA                                   
         MVC   P+44(2),PCLTAOFC                                                 
         MVC   P+53(2),PCLTACCA                                                 
*****    B     PRT50                                                            
*                                                                               
PRT50    CLI   RCWRITE,C'N'                                                     
         BE    PRT100                                                           
         GOTO1 PUTPRT                                                           
*                                                                               
PRT100   DS    0H                                                               
         BAS   RE,RPRT                                                          
         B     AGYC3                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*---------------------------------------------------------------------*         
*      OFFICE CONVERSION TABLE - YNR (YN) & ARMYP (AR)      (04/99)   *         
*---------------------------------------------------------------------*         
OFFTAB   DS    0H                                                               
*****    DC    CL2'  ',XL1'00'                                                  
         DC    CL2'D1',CL1'<'                                                   
         DC    CL2'BB',CL1'>'                                                   
         DC    CL2'BB',CL1'?'                                                   
         DC    CL2'BA',CL1'#'                                                   
         DC    CL2'AA',CL1'A'                                                   
         DC    CL2'D3',CL1'B'                                                   
         DC    CL2'CC',CL1'C'                                                   
         DC    CL2'BD',CL1'D'                                                   
         DC    CL2'CE',CL1'E'                                                   
         DC    CL2'CF',CL1'F'                                                   
         DC    CL2'CG',CL1'G'                                                   
         DC    CL2'CH',CL1'H'                                                   
         DC    CL2'CI',CL1'I'                                                   
         DC    CL2'CJ',CL1'J'                                                   
         DC    CL2'CK',CL1'K'                                                   
         DC    CL2'CL',CL1'L'                                                   
         DC    CL2'BM',CL1'M'                                                   
         DC    CL2'CN',CL1'N'                                                   
         DC    CL2'CO',CL1'O'                                                   
         DC    CL2'CP',CL1'P'                                                   
         DC    CL2'CQ',CL1'Q'                                                   
         DC    CL2'CR',CL1'R'                                                   
*****    DC    CL2'XX',CL1'\'                                                   
         DC    CL2'CS',CL1'S'                                                   
         DC    CL2'CT',CL1'T'                                                   
         DC    CL2'CU',CL1'U'                                                   
         DC    CL2'CV',CL1'V'                                                   
         DC    CL2'BW',CL1'W'                                                   
         DC    CL2'BX',CL1'X'                                                   
         DC    CL2'CY',CL1'Y'                                                   
         DC    CL2'CZ',CL1'Z'                                                   
         DC    CL2'C0',CL1'0'                                                   
         DC    CL2'C1',CL1'1'                                                   
         DC    CL2'C2',CL1'2'                                                   
         DC    CL2'C3',CL1'3'                                                   
         DC    CL2'C4',CL1'4'                                                   
         DC    CL2'C5',CL1'5'                                                   
         DC    CL2'C6',CL1'6'                                                   
         DC    CL2'C7',CL1'7'                                                   
         DC    CL2'C8',CL1'8'                                                   
         DC    CL2'C9',CL1'9'                                                   
OFFTABN  EQU   (*-OFFTAB)/3                                                     
         EJECT                                                                  
*---------------------------------------------------------------------*         
*   CLIENT OFFICE CONVERSION TABLE - YNR (YN) & ARMYP (AR)  (04/99)   *         
*---------------------------------------------------------------------*         
CLTTAB   DS    0H                                                               
         DC    CL3'DLV',CL2'CA',CL1'N'                                          
         DC    CL3'CLM',CL2'CA',CL1'N'                                          
         DC    CL3'DAZ',CL2'CA',CL1'N'                                          
         DC    CL3'DOK',CL2'CA',CL1'N'                                          
         DC    CL3'LR ',CL2'CA',CL1'N'                                          
         DC    CL3'DBM',CL2'CA',CL1'N'                                          
         DC    CL3'DRM',CL2'CA',CL1'N'                                          
         DC    CL3'DVL',CL2'CA',CL1'N'                                          
         DC    CL3'LM ',CL2'CA',CL1'N'                                          
         DC    CL3'LMS',CL2'CA',CL1'N'                                          
         DC    CL3'DMW',CL2'CA',CL1'N'                                          
         DC    CL3'DIN',CL2'CA',CL1'N'                                          
         DC    CL3'DLI',CL2'CA',CL1'N'                                          
         DC    CL3'DDE',CL2'CA',CL1'N'                                          
         DC    CL3'DOA',CL2'CA',CL1'N'                                          
         DC    CL3'DSE',CL2'CA',CL1'N'                                          
         DC    CL3'DSL',CL2'CA',CL1'N'                                          
         DC    CL3'DCI',CL2'CA',CL1'N'                                          
         DC    CL3'DPH',CL2'CA',CL1'N'                                          
         DC    CL3'DAT',CL2'CA',CL1'N'                                          
         DC    CL3'DJA',CL2'CA',CL1'N'                                          
         DC    CL3'DNE',CL2'CA',CL1'N'                                          
         DC    CL3'LDA',CL2'CA',CL1'N'                                          
         DC    CL3'DSY',CL2'CA',CL1'N'                                          
         DC    CL3'DCH',CL2'CA',CL1'N'                                          
         DC    CL3'DNW',CL2'CA',CL1'N'                                          
         DC    CL3'DCL',CL2'CA',CL1'N'                                          
         DC    CL3'DPI',CL2'CA',CL1'N'                                          
         DC    CL3'DTX',CL2'CA',CL1'N'                                          
         DC    CL3'DST',CL2'CA',CL1'N'                                          
         DC    CL3'DLM',CL2'CA',CL1'N'                                          
         DC    CL3'DME',CL2'CA',CL1'N'                                          
         DC    CL3'DOD',CL2'CA',CL1'N'                                          
         DC    CL3'DMD',CL2'CA',CL1'N'                                          
         DC    CL3'DLA',CL2'CA',CL1'N'                                          
         DC    CL3'DWA',CL2'CA',CL1'N'                                          
         DC    CL3'LMD',CL2'CA',CL1'N'                                          
         DC    CL3'DSD',CL2'CA',CL1'N'                                          
         DC    CL3'GLR',CL2'CA',CL1'N'                                          
         DC    CL3'DRN',CL2'CA',CL1'N'                                          
         DC    CL3'FMS',CL2'CA',CL1'N'                                          
         DC    CL3'DBP',CL2'CA',CL1'N'                                          
         DC    CL3'DCO',CL2'CA',CL1'N'                                          
         DC    CL3'DCT',CL2'CA',CL1'N'                                          
         DC    CL3'DDM',CL2'CA',CL1'N'                                          
         DC    CL3'DDO',CL2'CA',CL1'N'                                          
         DC    CL3'DES',CL2'CA',CL1'N'                                          
         DC    CL3'DEU',CL2'CA',CL1'N'                                          
         DC    CL3'DHM',CL2'CA',CL1'N'                                          
         DC    CL3'DHO',CL2'CA',CL1'N'                                          
         DC    CL3'DNJ',CL2'CA',CL1'N'                                          
         DC    CL3'DSW',CL2'CA',CL1'N'                                          
         DC    CL3'HH ',CL2'CA',CL1'N'                                          
         DC    CL3'HTZ',CL2'CA',CL1'N'                                          
         DC    CL3'LMA',CL2'CA',CL1'N'                                          
         DC    CL3'LMC',CL2'CA',CL1'N'                                          
         DC    CL3'MCD',CL2'CA',CL1'N'                                          
         DC    CL3'WME',CL2'D4',CL1'K'                                          
         DC    CL3'WLI',CL2'D4',CL1'K'                                          
         DC    CL3'LMW',CL2'D4',CL1'K'                                          
         DC    CL3'TBC',CL2'D4',CL1'K'                                          
         DC    CL3'TBE',CL2'D4',CL1'K'                                          
         DC    CL3'TBI',CL2'D4',CL1'K'                                          
         DC    CL3'TBK',CL2'D4',CL1'K'                                          
         DC    CL3'TBN',CL2'D4',CL1'K'                                          
         DC    CL3'TBP',CL2'D4',CL1'K'                                          
         DC    CL3'TBR',CL2'D4',CL1'K'                                          
         DC    CL3'TBS',CL2'D4',CL1'K'                                          
         DC    CL3'TBX',CL2'D4',CL1'K'                                          
         DC    CL3'TCS',CL2'D4',CL1'K'                                          
         DC    CL3'TIB',CL2'D4',CL1'K'                                          
         DC    CL3'TKB',CL2'D4',CL1'K'                                          
         DC    CL3'TPR',CL2'D4',CL1'K'                                          
         DC    CL3'TXB',CL2'D4',CL1'K'                                          
         DC    CL3'LMM',CL2'D4',CL1'K'                                          
         DC    CL3'LCJ',CL2'D4',CL1'K'                                          
         DC    CL3'WLD',CL2'D4',CL1'K'                                          
CLTTABN  EQU   (*-CLTTAB)/6                                                     
         EJECT                                                                  
PP02WRKD DSECT                                                                  
ELCODE   DS    X                                                                
OFCTEST  DS    C                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001PPREPFXOYR04/15/99'                                      
         END                                                                    
