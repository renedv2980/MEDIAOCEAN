*          DATA SET CTRDCTFIL  AT LEVEL 038 AS OF 07/12/01                      
*PHASE READCTFA                                                                 
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
*                                                                               
         TITLE 'CTRDCTFIL - SCAN AND PRINT OUT RECORDS - SPARTAN'               
***********************************************************************         
*                                                                     *         
*  PURPOSE:  READ ID RECS WITH TEST ELEMENT (X'23' VIRGIN CONTROL)    *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
SCANPROF CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,SCANPROF,=V(REGSAVE),R9                                        
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',FLIST,IO                 
         MVI   DATADISP+1,28                                                    
*                                                                               
* SEQUENTIALLY READ RECS IN CTFILE                                              
         XC    KEY,KEY                                                          
         LA    R7,KEY                                                           
         USING CTIREC,R7                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         LA    R7,IO                                                            
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',KEY,IO                    
MX10     DS    0H                                                               
*                                                                               
         CLC   KEY(1),IO                                                        
         BNE   MXB                                                              
         CLI   CTIKID,0            SKIP PASSIVES                                
         BNH   MX30                                                             
         LA    R6,IO                                                            
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   MX30                                                             
*                                                                               
*                                                                               
* PRINT OUT RECORD KEY...                                                       
         MVC   P(10),CTIKID                                                     
         LA    R2,P+12                                                          
*                                                                               
         USING SYSLSTD,R3                                                       
*                                                                               
MX12     LA    R3,SYSLST                                                        
         LA    R3,6(R3)                                                         
         MVC   BYTE2,2(R6)                                                      
         MVI   2(R6),C'0'          POS HEXIN                                    
         GOTO1 =V(HEXIN),DMCB,2(R6),BYTE,2                                      
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+6                                                              
         DCHO                                                                   
         MVC   2(1,R6),BYTE2                                                    
*                                                                               
MX14     CLC   SYSLNUM,BYTE                                                     
         BE    MX16                                                             
         AHI   R3,SYSLLEN                                                       
         CLI   0(R3),0                                                          
         BNE   MX14                                                             
         MVC   0(3,R2),=C'???'                                                  
         B     *+10                                                             
*                                                                               
MX16     MVC   0(3,R2),SYSLSHRT                                                 
         AHI   R2,4                                                             
         MVC   0(4,R2),2(R6)                                                    
         MVI   4(R2),C'='                                                       
         MVC   5(1,R2),6(R6)                                                    
         AHI   R2,7                                                             
         BAS   RE,NEXTEL                                                        
         BE    MX12                                                             
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
MX30     DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMRSEQ',=C'CTFILE',KEY,IO                    
         B     MX10                                                             
         DROP  R7                                                               
*                                                                               
MXB      XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SSB      DC    XL2'00',X'FF',253X'00'                                           
UTL      DC    F'0',X'0A'                                                       
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
FLIST    DS    0H                                                               
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
*                                                                               
         DC    C'**KEY***'                                                      
KEY      DS    XL25                BTAM KEY                                     
KEYSAVE  DS    XL25                USED TO INSURE EXCLUSIVE KEYS                
DATADISP DS    H                                                                
ELCODE   DS    CL1                                                              
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    CL255                                                            
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
*                                                                               
         DS    0D                                                               
       ++INCLUDE FASYSLST                                                       
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
         DC    A(SCANPROF,65000)                                                
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG   *                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038CTRDCTFIL 07/12/01'                                      
         END                                                                    
