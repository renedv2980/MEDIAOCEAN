*          DATA SET TZIHDMC    AT LEVEL 081 AS OF 10/27/00                      
*PHASE TZIHDMC                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
                                                                                
         TITLE 'ANOTHER DATAMGR EXAMPLE'                                        
TZIHDMC  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,TZIHDMC,=V(REGSAVE),R9                                         
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN                                                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TZIHDMC),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
* DON'T WORRY ABOUT THIS DMOPEN CALL                                            
********************************************************************            
MAIN     DS    0H                                                               
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',IOAREA,0                                           
         TM    DMCB+8,X'F2'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
*BUILD KEY TO SEARCH FOR RECORDS                                                
         LA    R1,KEY                                                           
         USING CTPHRECD,R1                                                      
                                                                                
         XC    KEY,KEY                                                          
         MVI   CTPHID,CTPHIDQ                                                   
         MVI   CTPHSUBI,CTPHSUBQ                                                
         MVI   CTPHHEXN,X'02'                                                   
                                                                                
         DROP  R1                                                               
         LA    R3,P                                                             
         USING PRD,R3                                                           
                                                                                
         MVC   PPNAME,=CL7'PHASE'                                               
         MVC   PLANG,=CL3'LANG'                                                 
         MVC   PDESC,=CL50'DESCRIPTION'                                         
         MVC   PRELDATE,=CL8'REL.DATE'                                          
         MVC   PTYPE,=CL7'TYPE'                                                 
         MVC   PRLIST,=CL20'LOAD LIST'                                          
         MVC   PCOMMENT,=CL30'COMMENTS'                                         
         GOTO1 =V(PRINTER)                                                      
                                                                                
         MVC   PPNAME,=CL7'-----'                                               
         MVC   PLANG,=CL3'----'                                                 
         MVC   PDESC,=CL50'-----------'                                         
         MVC   PRELDATE,=CL8'--------'                                          
         MVC   PTYPE,=CL7'----'                                                 
         MVC   PRLIST,=CL20'---------'                                          
         MVC   PCOMMENT,=CL30'--------'                                         
         GOTO1 =V(PRINTER)                                                      
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IOAREA            
MLOOP    DS    0H                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* PUT DSECTS ON IOAREA AND PRINT LINE                                           
* NOTE:  R2 WILL POINT TO IOAREA, AND WILL BE USED FOR NOTHING ELSE             
         LA    R2,IOAREA                                                        
         USING CTPHRECD,R2                                                      
                                                                                
*CHECK IF RIGHT RECORD TYPE WAS RETURNED                                        
         CLC   KEY(CTPHLANG-CTPHNAME),IOAREA                                    
         BNE   EMLOOP                                                           
                                                                                
* CHECK IF IT IS FOR RIGHT SYSTEM                                               
         CLI   CTPHHEXN,X'02'                                                   
         BNE   MGETNEXT                                                         
                                                                                
* CONVERT PHASE NAME FROM HEX CODE TO EBCDIC                                    
         GOTO1 =V(HEXOUT),DMCB,CTPHHEXN,PPNAME,L'CTPHHEXN                       
         MVI   PPNAME,C'T'          FIRST LETTER IS ALWAYS 'T'                  
         MVC   PPNAME+6(1),CTPHLVL                                              
                                                                                
* FIND LANGUAGE NAME CORRESPONDING TO LANGUAGE CODE                             
         SR    R1,R1                                                            
         ICM   R1,3,LANGTAB                                                     
         SR    R4,R4                                                            
         ZIC   R5,CTPHLANG                                                      
         MR    R4,R1                                                            
                                                                                
         LA    R5,LANGTAB0(R5)                                                  
         USING LANGTABD,R5                                                      
         MVC   PLANG,LANGSHR                                                    
         DROP  R5                                                               
                                                                                
* FIND DESCRIPTION ELEMENT                                                      
         LR    R4,R2               POINT R4 TO START OF RECORD                  
         MVI   ELCODE,CTPHDCEQ     LOAD R0 WITH DESCRIPTION EL. CODE            
         BAS   RE,GETEL            CALL TO ELEMENT FIND FUNCTION                
         BE    *+6                 DESCRIPTION MUST BE PRESENT                  
         DS    H'0'                OTHERWISE - DIE                              
                                                                                
         USING CTPHDSCD,R4         R4 POINTS TO ELEMENT NOW                     
         ZIC   R1,CTPHDLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8              EXEC. MOVE OF DESCRIPTION INTO P             
         B     *+10                                                             
         MVC   PDESC(0),CTPHDDSC                                                
         DROP  R4                                                               
                                                                                
* FIND SYSTEM ELEMENT                                                           
         LR    R4,R2               POINT R4 TO START OF RECORD                  
         MVI   ELCODE,CTPHSCEQ     LOAD R0 WITH DESCRIPTION EL. CODE            
         BAS   RE,GETEL            CALL TO ELEMENT FIND FUNCTION                
         BE    *+6                 SYSTEM ELEMENT MUSTST BE PRESENT             
         DS    H'0'                OTHERWISE - DIE                              
                                                                                
         USING CTPHSYSD,R4                                                      
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(DATCON),DMCB,(8,CTPHSRDT),(11,PRELDATE)                       
         TM    CTPHSFL1,CTPHSSCQ   TEST IF SCREEN                               
         BZ    *+14                                                             
         MVC   PTYPE,=CL7'SCREEN'                                               
         B     *+10                                                             
         MVC   PTYPE,=CL7'PROGRAM'                                              
         DROP  R4                                                               
                                                                                
* FIND RELOAD LIST ELEMENT                                                      
         LR    R4,R2                                                            
         MVI   ELCODE,CTPHLCEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   MERLIST                                                          
                                                                                
         USING CTPHLSTD,R4                                                      
         CLI   CTPHLFLG,CTPHLALQ   CHECK IF ALL                                 
         BNE   MRLIST10                                                         
         MVC   PRLIST,=CL20'ALL'                                                
         B     MERLIST                                                          
                                                                                
MRLIST10 DS    0H                                                               
         USING CTPHLSTD,R4                                                      
         CLI   CTPHLLEN,X'3'       IF LENGTH=3 - NO RELOAD LIST                 
         BE    MERLIST                                                          
                                                                                
         ZIC   R0,CTPHLLEN         AT THIS POINT WE KNOW THERE ARE SOME         
         AR    R0,R4               R0 POINTS TO NEXT ELEMENT                    
         LA    R4,CTPHLOVQ(R4)     R4 POINTS TO FIRST LIST MEMBER               
         LA    R5,PRLIST                                                        
MRLLP    DS    0H                                                               
         GOTO1 =V(HEXOUT),DMCB,0(R4),0(R5),1                                    
         AHI   R5,2                                                             
         AHI   R4,1                                                             
         CR    R4,R0                                                            
         BNL   EMRLLP                                                           
         MVI   0(R5),C','                                                       
         AHI   R5,1                                                             
         B     MRLLP                                                            
EMRLLP   DS    0H                                                               
MERLIST  DS    0H                                                               
         DROP  R4                                                               
                                                                                
* FIND COMMENT ELEMENT                                                          
         LR    R4,R2                                                            
         MVI   ELCODE,CTPHCCEQ                                                  
         BAS   RE,GETEL                                                         
         BE    MSKP                                                             
         GOTO1 =V(PRINTER)                                                      
         B     EMCOMMLP                                                         
MSKP     DS    0H                                                               
         USING CTPHCOMD,R4         R4 POINTS TO ELEMENT NOW                     
                                                                                
MCOMMLP  DS    0H                                                               
         ZIC   R1,CTPHCLEN                                                      
         CHI   R1,30               MAKE SURE COMMENT WIDTH NOT EXCEEDED         
         BNH   MCONT                                                            
         LHI   R1,30                                                            
MCONT    DS    0H                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8              EXEC. MOVE OF DESCRIPTION INTO P             
         B     *+10                                                             
         MVC   PCOMMENT(0),CTPHCTXT                                             
         GOTO1 =V(PRINTER)         ACTUAL CALL TO PRINT                         
         DROP  R4                                                               
         BAS   RE,NEXTEL                                                        
         B     EMCOMMLP                                                         
EMCOMMLP DS    0H                                                               
                                                                                
MGETNEXT DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,IOAREA            
         B     MLOOP                                                            
EMLOOP   DS    0H                                                               
                                                                                
EMAIN    XBASE                                                                  
         EJECT                                                                  
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
* DON'T WORRY ABOUT THE FIELDS ON THIS PAGE                                     
*                                                                               
UTL      DC    F'0',X'0A'          FOR OFFLINE DATAMGR                          
*                                                                               
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         EJECT                                                                  
                                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
DATADISP DC    H'28'               KEY+LENGTH+STATUS = 28                       
ELCODE   DS    X                                                                
WORK     DS    CL17                                                             
KEY      DS    CL25                CTFILE KEY                                   
IOAREA   DS    1000X               I/O AREA FOR CTFILE                          
                                                                                
       ++INCLUDE FALANGTAB                                                      
                                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
PRD      DSECT                                                                  
PPNAME   DS    CL7                                                              
PPNAMELQ EQU   L'PPNAME                                                         
         DS    (SPC)C                                                           
PLANG    DS    CL3                                                              
PLANGLQ  EQU   L'PLANG                                                          
         DS    (SPC)C                                                           
PDESC    DS    CL50                                                             
PDESCLQ  EQU   L'PDESC                                                          
         DS    (SPC)C                                                           
PRELDATE DS    CL8                                                              
PRELDLQ  EQU   L'PRELDATE                                                       
         DS    (SPC)C                                                           
PTYPE    DS    CL7                                                              
PTYPELQ  EQU   L'PTYPE                                                          
         DS    (SPC)C                                                           
PRLIST   DS    CL20                                                             
PRLISTLQ EQU   L'PRLIST                                                         
         DS    (SPC)C                                                           
PCOMMENT DS    CL30                                                             
PCOMMLQ  EQU   L'PCOMMENT                                                       
SPC      EQU   1                                                                
PRDLQ    EQU   *-PRD                                                            
                                                                                
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE FALANG                                                         
       ++INCLUDE CTGENPHASE                                                     
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081TZIHDMC   10/27/00'                                      
         END                                                                    
