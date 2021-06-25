*          DATA SET TZIHDM2A   AT LEVEL 055 AS OF 10/31/00                      
*PHASE TZIHDM2A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE FATABOFF                                                               
*INCLUDE DATCON                                                                 
                                                                                
         TITLE 'ANOTHER DATAMGR EXAMPLE'                                        
TZIHDM2A CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,TZIHDM2A,=V(REGSAVE),R9                                        
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
         DC    A(TZIHDM2A),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
                                                                                
******************************************************************              
MAIN     DS    0H                                                               
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',IOAREA,0                                           
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R9,P                                                             
         USING PRD,R9                                                           
                                                                                
         MVC   PNAMESH,=CL2'AG'                                                 
         MVC   PNAMELN,=CL33'AGENCY NAME'                                       
         MVC   PADDR,=CL33'AGENCY ADDRESS'                                      
         MVC   PLACDAT,=CL10'LAST ACTION'                                       
         MVC   PACCLST,=CL40'ACCESS LIST'                                       
         GOTO1 =V(PRINTER)                                                      
                                                                                
         MVC   PNAMESH,=2C'='                                                   
         MVC   PNAMELN,=33C'='                                                  
         MVC   PADDR,=33C'='                                                    
         MVC   PLACDAT,=10C'='                                                  
         MVC   PACCLST,=40C'='                                                  
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
                                                                                
* BUILD KEY TO SEARCH FOR ACCESS RECORDS                                        
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CT5REC,R1                                                        
         MVI   CT5KTYP,CT5KTYPQ    SYSTEM ACCESS REC                            
         DROP  R1                                                               
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IOAREA            
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,IOAREA           CALCULATE & STORE DISP TO FIRST EL           
         LHI   R0,(CT5DATA-CT5REC)                                              
         STH   R0,DATADISP                                                      
                                                                                
MLOOP    DS    0H                                                               
         CLC   IOAREA(CT5KTYP-CT5REC),KEY                                       
         BNE   EMLOOP                                                           
                                                                                
         MVC   KEYSAV,IOAREA                                                    
                                                                                
         MVC   PNAMESH,IOAREA+23                                                
                                                                                
         MVI   ELCODE,X'01'        SEARCH FOR ACTIVITY ELEMENT                  
         LA    R3,IOAREA                                                        
         BAS   RE,GETEL                                                         
         BNE   MNOX01ID            NO ACTIVITY ELEMENT FOUND                    
         USING CTACTD,R3                                                        
         GOTO1 =V(DATCON),DMCB,(3,CTACTDT),(11,PLACDAT)                         
         DROP  R3                                                               
MNOX01ID DS    0H                  LAST ACTIVITY DATE IS NOT PRINTED            
                                                                                
*FIND DESCRIPTION ELEMENT TO EXTRACT AGENCY CODE                                
         MVI   ELCODE,X'02'        SEARCHING FOR DESCRIPTION ELEMENT            
         LR    R3,R2               POINT R3 TO BEGINNING OF RECORD              
         BAS   RE,GETEL                                                         
         BNE   NOX02               NO DESCRIPTION ELEM. FOUND                   
                                                                                
*NOW BUILD KEY TO ACCESS ID RECORD                                              
         LA    R1,KEY                                                           
         USING CTIREC,R1                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKNUM,2(R3)       MOVE IN AGENCY ID                            
         DROP  R1                  KEY'S BUILT                                  
                                                                                
*CALL DATAMANAGER TO GET ID RECORD FOR CURRENT AGENCY                           
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,IOAREA            
         TM    DMCB+8,X'FF'                                                     
         BNZ   NOX02               NO SUCH ID RECORD - PRINT NOTHING            
                                                                                
         MVI   ELCODE,X'30'        SEARCH FOR DESTINATION ELEMENT               
         LA    R3,IOAREA                                                        
         BAS   RE,GETEL                                                         
         BNE   MNOX30ID                                                         
         USING CTDSTD,R3                                                        
         MVC   PNAMELN,CTDSTNAM    COPY NAME AND ADDRESS                        
         MVC   PADDR,CTDSTADD                                                   
         DROP  R3                                                               
                                                                                
MNOX30ID DS    0H                  AGENCY NAME & ADDRESS NOT PRINTED            
                                                                                
         MVI   ELCODE,X'21'        SEARCH FOR AUTHORIZATION ELEMENT             
         LA    R3,IOAREA                                                        
         LA    R6,PACCLST          PRINT FIELD FOR ACCESS LIST                  
                                                                                
         BAS   RE,GETEL                                                         
         BNE   MNOX21ID                                                         
MX21LP   DS    0H                                                               
         USING CTSYSD,R3           DSECT FOR AUTHORIZATION ELEMENT              
                                                                                
         L     R5,=V(SELIST)       R5 POINTS TO TABLE                           
         LH    R0,0(R5)            R0 HAS TAB ENTRY LENGTH                      
                                                                                
         LA    R5,6(R5)            R5 POINTS TO FIRST ENTRY                     
         USING SELISTD,R5                                                       
                                                                                
* LOOP THAT SEARCHES IN TABLE FOR SYSTEM NAME                                   
MTABLP   DS    0H                                                               
         CLC   SENAME,=7X'FF'                                                   
         BNL   MNEXT21                                                          
         CLC   SESYS,CTSYSNUM                                                   
         BE    EMTABLP                                                          
         AR    R5,R0                                                            
         B     MTABLP                                                           
EMTABLP  DS    0H                                                               
*AT THIS POINT R5 MUST POINT TO TAB ENTRY THAT WE NEED TO PRINT                 
                                                                                
**************START IN THE BACK, ELIMINATE SPACES...                            
         LA    R1,6(R5)                                                         
MSPCLP   DS    0H                                                               
         CR    R1,R5                                                            
         BNH   EMSPCLP                                                          
         CLI   0(R1),C' '                                                       
         BNE   EMSPCLP                                                          
         BCTR  R1,0                                                             
         B     MSPCLP                                                           
EMSPCLP  DS    0H                                                               
         SR    R1,R5               FIND NUMBER OF CHARACTERS                    
*CHECK IF TIME TO TRUNCATE                                                      
         LA    R0,PACCLST                                                       
         SR    R0,R6                                                            
         LPR   R0,R0               R0 HAS # OF CHARACTERS IN P                  
         AR    R0,R1                                                            
         CHI   R0,40                                                            
         BH    MNOX21ID                                                         
                                                                                
*EXECUTED MOVE                                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R5)                                                    
         LA    R6,1(R1,R6)         ADVANCE POINTER IN P LINE                    
                                                                                
         DROP  R5                                                               
         DROP  R3                                                               
                                                                                
         BAS   RE,NEXTEL                                                        
         BNE   MNOX21ID                                                         
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
                                                                                
MNEXT21  DS    0H                                                               
         B     MX21LP                                                           
MNOX21ID DS    0H                                                               
                                                                                
NOX02    DS    0H                  ACCESS REC HAS NO DESR EL, DO NOTHIN         
                                                                                
         GOTO1 =V(PRINTER)                                                      
                                                                                
         DROP  R9                                                               
                                                                                
         MVC   KEY,KEYSAV          RESTORE KEY                                  
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,IOAREA            
                                                                                
* GET NEXT ACCESS RECORD                                                        
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,IOAREA            
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         B     MLOOP                                                            
EMLOOP   DS    0H                                                               
                                                                                
EMAIN    XBASE                                                                  
         EJECT                                                                  
                                                                                
         GETEL R3,DATADISP,ELCODE                                               
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
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
WORK     DS    CL17                                                             
KEY      DS    CL25                CTFILE KEY                                   
KEYSAV   DS    CL25                CTFILE KEY                                   
IOAREA   DS    1000X               I/O AREA FOR CTFILE                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
                                                                                
PRD      DSECT                                                                  
PNAMESH  DS    CL2                                                              
         SPC   (SPC)C                                                           
PNAMELN  DS    CL33                                                             
         SPC   (SPC)C                                                           
PADDR    DS    CL33                                                             
         SPC   (SPC)C                                                           
PLACDAT  DS    CL10                                                             
         SPC   (SPC)C                                                           
PACCLST  DS    CL40                                                             
SPC      EQU   3                                                                
PRDLQ    EQU   *-PRD                                                            
                                                                                
                                                                                
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055TZIHDM2A  10/31/00'                                      
         END                                                                    
