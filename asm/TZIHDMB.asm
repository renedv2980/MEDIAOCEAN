*          DATA SET TZIHDMB    AT LEVEL 058 AS OF 10/24/00                      
*PHASE TZIHDMB                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
         TITLE 'DATAMGR EXAMPLE'                                                
TZIHDMGR CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,TZIHDMGR,=V(REGSAVE)                                           
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
                                                                                
***********************************************************************         
MAIN     DS    0H                                                               
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'SPOT',               +        
               =C'NSTAFILEX',IOAREA,0                                           
                                                                                
         TM    DMCB+8,X'F2'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
* BUILD KEY TO SEARCH FOR STATION RECORDS                                       
                                                                                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,IOAREA           
         TM    DMCB+8,X'F2'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
                                                                                
MLOOP    DS    0H                                                               
         CLI   8(R1),0             CHECK FOR ERRORS IN RETURN CODE              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTKY                                    
                                                                                
NEXT     GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'STATION',KEY,IOAREA           
         TM    DMCB+8,X'F2'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         B     MLOOP                                                            
EMLOOP   DS    0H                                                               
                                                                                
                                                                                
         XBASE                                                                  
         EJECT                                                                  
                                                                                
                                                                                
                                                                                
* DON'T WORRY ABOUT THE FIELDS ON THIS PAGE                                     
*                                                                               
UTL      DC    F'0',X'02'          FOR OFFLINE DATAMGR                          
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
                                                                                
                                                                                
                                                                                
DMCB     DS    6F                                                               
KEY      DS    CL15                STAFILE KEY                                  
LASTMC   DS    CL4                                                              
LASTMN   DS    CL24                                                             
                                                                                
SRTKY    DS    CL(SRTKYDLQ)                                                     
IOAREA   DS    1000X               I/O AREA FOR STAFILE                         
                                                                                
*WORK     DS    (1000+SRTKYDLQ+4)X                                              
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=1200'                                  
SORTCARD DC    CL80'SORT FIELDS=(5,9,A),FORMAT=BI,WORK=1'                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
                                                                                
PRD      DSECT                                                                  
PMCODE   DS    CL4                                                              
         DS    CL(SPC)                                                          
PMNAME   DS    CL24                                                             
         DS    CL(SPC)                                                          
PCALL    DS    CL5                                                              
         DS    CL(SPC)                                                          
PKEY     DS    CL15                                                             
SPC      EQU   3                                                                
PRDLQ    EQU   *-PRD                                                            
                                                                                
SRTKYD   DSECT                                                                  
SRECLEN  DS    H                                                                
SRECZRS  DS    CL2                                                              
SMCODE   DS    CL4                                                              
SCALL    DS    CL5                                                              
SRTKYDLQ EQU   *-SRTKYD                                                         
                                                                                
                                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058TZIHDMB   10/24/00'                                      
         END                                                                    
