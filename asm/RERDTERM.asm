*          DATA SET RERDTERM   AT LEVEL 044 AS OF 09/04/03                      
*PHASE REPTERMA                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE STXITER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
*                                                                               
         TITLE 'CTRDTERM - SCAN AND PRINT OUT RECORDS - SPARTAN'                
***********************************************************************         
*                                                                     *         
*  PURPOSE:  LIST ID RECS WITH DARE PARTNER CODE = A/R                *         
*                                                                     *         
***********************************************************************         
*                                                                               
READTERM CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**RDTRM*,=V(REGSAVE),R9                                        
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         L     R1,=V(DATAMGR)                                                   
         ST    R1,VDATAMGR                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',FLIST,IO                    
         MVI   DATADISP+1,28                                                    
*                                                                               
* SEQUENTIALLY READ ALL ID RECS IN CTFILE                                       
         XC    KEY,KEY                                                          
         LA    R7,IO                                                            
         USING CTIREC,R7                                                        
         MVI   KEY,CTIKTYPQ                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,IO                       
*                                                                               
MX10     DS    0H                                                               
         CLI   CTIKTYP,CTIKTYPQ    IS THIS AN ID REC?                           
         BH    MXB                  NO - ALL DONE                               
         OC    CTIKID(8),CTIKID    PASSIVE KEY?                                 
         BZ    MX30                 YES - SKIP                                  
*                                                                               
         LR    R6,R7                                                            
         MVI   ELCODE,X'33'                                                     
         BAS   RE,GETEL                                                         
         BNE   MX30                                                             
         USING CTUSAD,R6                                                        
         CLI   CTUSADPI,C'A'                                                    
         BE    *+12                                                             
         CLI   CTUSADPI,C'R'                                                    
         BNE   MX30                                                             
*                                                                               
* PRINT OUT RECORD KEY...                                                       
         LA    R2,P                                                             
         MVC   0(L'CTIKID,R2),CTIKID                                            
         LA    R2,11(R2)                                                        
         MVC   0(1,R2),CTUSADPI                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
MX30     DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'CTFILE',KEY,IO                       
         TM    DMCB+8,X'80'                                                     
         BNZ   MXB                                                              
         B     MX10                                                             
         DROP  R7                                                               
MXB      XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SSB      DC    XL2'00',X'FF',253X'00'                                           
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8'NCTFILE '                                                    
         DC    CL8'X       '                                                    
*                                                                               
VDATAMGR DS    F                                                                
*                                                                               
         DC    C'**KEY***'                                                      
KEY      DS    XL25                BTAM KEY                                     
KEYSAVE  DS    XL25                USED TO INSURE EXCLUSIVE KEYS                
DATADISP DS    H                                                                
ELCODE   DS    CL1                                                              
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL255                                                            
*                                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
RECLEN   DS    H                   REC LEN FOR QSAM PUT                         
         DC    H'0'                                                             
IO       DS    XL2000              IO AREA                                      
LASTREC  DS    XL2000                                                           
THISREC  DS    XL2000                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(READTERM,65000)                                                
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG   *                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044RERDTERM  09/04/03'                                      
         END                                                                    
