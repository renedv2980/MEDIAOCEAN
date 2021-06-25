*          DATA SET TZIHDM3    AT LEVEL 034 AS OF 10/30/00                      
*PHASE TZIHDM3                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE FATABOFF                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
         TITLE 'ANOTHER DATAMGR EXAMPLE'                                        
TZIHDMGR CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,TZIHDMGR,=V(REGSAVE),R9                                        
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
         DC    A(TZIHDMGR),V(DUMMY)                                             
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
* DON'T WORRY ABOUT THIS DMOPEN CALL                                            
*                                                                               
MAIN     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',IO,0                                               
*                                                                               
         BAS   RE,PRINTIT                                                       
                                                                                
*                                                                               
GOODBYE  XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
** READ THE ACCESS RECORD                                                       
***********************************************************************         
PRINTIT  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CT5REC,R2                                                        
         MVI   CT5KTYP,CT5KTYPQ      READ FIRST ACCESS RECORD                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                
*                                                                               
PRNTLOOP CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,IO                                                            
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   PRNTDONE                                                         
*                                                                               
         BAS   RE,PRNTAGCY           PRINT THE ALPHA AGENCY ID                  
*                                                                               
         BAS   RE,PRNTPNAD           PRINT PID,NAME,ADDRESS                     
*                                                                               
         BAS   RE,PRNTAUTH           PRINT AUTHORIZATION NAMES                  
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,IO                
         B     PRNTLOOP                                                         
*                                                                               
         DROP  R2                                                               
PRNTDONE B     EXIT                                                             
***********************************************************************         
** PRINT ALPHA AGENCY ID                                                        
***********************************************************************         
PRNTAGCY NTR1                                                                   
*                                                                               
         LA    R2,IO                                                            
         USING CT5REC,R2                                                        
         LA    R3,P                                                             
         USING PRINTD,R3                                                        
*                                                                               
         MVC   PAGENCY,CT5KALPH                                                 
*                                                                               
         DROP  R2                                                               
         DROP  R3                                                               
         B     EXIT                                                             
***********************************************************************         
** PRINT CONTROL PID,NAME,AND ADDRESS                                           
***********************************************************************         
PRNTPNAD NTR1                                                                   
*                                                                               
         LA    R2,IO                                                            
         USING CT5REC,R2                                                        
         MVC   KEY,IO                SAVE OFF ACCESS RECORD KEY                 
         MVI   ELCODE,CTDSCELQ       USER-ID ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    GOTPNAD                                                          
         B     NOPNAD                                                           
         USING CTDSCD,R2                                                        
*                                                                               
GOTPNAD  XC    TEMPKEY,TEMPKEY                                                  
         LA    R4,TEMPKEY                                                       
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CTDSC                                                    
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',TEMPKEY,IO            
*                                                                               
         CLI   8(R1),0                                                          
         BNE   NOPNAD                                                           
*                                                                               
         BAS   RE,PRINTID                                                       
*                                                                               
         BAS   RE,PRNTNAAD                                                      
*                                                                               
         DROP  R4                                                               
NOPNAD   GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,IO                
         B     EXIT                                                             
***********************************************************************         
** PRINT THE ID                                                                 
***********************************************************************         
PRINTID  NTR1                                                                   
*                                                                               
         LA    R2,IO                                                            
         MVI   ELCODE,CTDSCELQ       THE DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    GOTID                                                            
         B     NOID                                                             
         USING CTDSCD,R2                                                        
*                                                                               
GOTID    ZIC   R5,CTDSCLEN                                                      
         AHI   R5,-3                                                            
         LA    R3,P                                                             
         USING PRINTD,R3                                                        
         EX    R5,*+4                                                           
         MVC   PPID(0),CTDSC                                                    
*                                                                               
         DROP  R2                                                               
         DROP  R3                                                               
*                                                                               
NOID     B     EXIT                                                             
***********************************************************************         
** PRINT NAME AND ADDRESS                                                       
***********************************************************************         
PRNTNAAD NTR1                                                                   
*                                                                               
         LA    R2,IO                                                            
         MVI   ELCODE,CTDSTELQ       COVER DESTINATION DETAIL ELEMENT           
         BAS   RE,GETEL                                                         
         BE    GOTNAAD                                                          
         B     NONAAD                                                           
         USING CTDSTD,R2                                                        
*                                                                               
GOTNAAD  LA    R3,P                                                             
         USING PRINTD,R3                                                        
         MVC   PNAME,CTDSTNAM                                                   
         MVC   PADDRESS,CTDSTADD                                                
*                                                                               
         DROP  R2                                                               
         DROP  R3                                                               
*                                                                               
NONAAD   B     EXIT                                                             
***********************************************************************         
** PRINT AUTHORIZED SYSTEM/AGENCY NAMES                                         
***********************************************************************         
PRNTAUTH NTR1                                                                   
*                                                                               
         LA    R3,P                                                             
         USING PRINTD,R3                                                        
         LA    R2,IO                                                            
         MVI   ELCODE,CTSYSELQ       SYSTEM AUTHORIZATION ELEMENT               
         BAS   RE,GETEL                                                         
         BE    GOTAUTH                                                          
         B     NOAUTH                                                           
         USING CTSYSD,R2                                                        
*                                                                               
GOTAUTH  L     R5,=V(SELIST)                                                    
         USING SELISTD,R5                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
*                                                                               
NEXTNAME CLC   CTSYSNUM,SESYS                                                   
         BE    GOTNAME                                                          
         BXLE  R5,R6,NEXTNAME                                                   
         DC    H'0'                                                             
*                                                                               
GOTNAME  LA    R4,L'SENAME(R5)       POINT TO                                   
         BCTR  R4,0                  LAST BYTE OF SENAME                        
*                                                                               
BACKSPAC CLI   0(R4),C' '            IS IT A SPACE?                             
         BNE   NOSPACES                                                         
         BCT   R4,BACKSPAC                                                      
*                                                                               
NOSPACES SR    R4,R5                 # OF BYTES                                 
         EX    R4,*+4                TO BE PRINTED                              
         MVC   PAUTH(0),SENAME                                                  
         AR    R3,R4                 BUMP TO  NEXT ...                          
         AHI   R3,1                  ... SPACE ON PRINTLINE                     
*                                                                               
         BAS   RE,NEXTEL             GET NEXT AUTHORIZATION ELEMENT             
         BNE   NOAUTH                                                           
         MVI   PAUTH,C','                                                       
         AR    R3,1                                                             
         B     GOTAUTH                                                          
*                                                                               
         DROP  R2                                                               
         DROP  R3                                                               
NOAUTH   XIT1                                                                   
***********************************************************************         
** THE GETEL MACRO                                                              
         GETEL R2,28,ELCODE                                                     
***********************************************************************         
** XIT1 FUNCTION CALL                                                           
EXIT     XIT1                                                                   
***********************************************************************         
*                                                                               
         EJECT                                                                  
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
ELCODE   DS    X                                                                
TEMPKEY  DS    CL25                                                             
WORK     DS    CL17                                                             
KEY      DS    CL25                CTFILE KEY                                   
IO       DS    1000X               I/O AREA FOR CTFILE                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
PRINTD   DSECT                                                                  
PAGENCY  DS    CL2                                                              
         DS    CL2                                                              
PPID     DS    CL8                                                              
         DS    CL2                                                              
PNAME    DS    CL33                                                             
         DS    CL2                                                              
PADDRESS DS    CL33                                                             
         DS    CL2                                                              
PAUTH    DS    0C                                                               
       ++INCLUDE FASELIST                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034TZIHDM3   10/30/00'                                      
         END                                                                    
