*          DATA SET CTSCANPROF AT LEVEL 009 AS OF 03/25/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 045257.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PROCESS USING(WARN(15))                                                        
*PHASE SCANPROA                                                                 
*INCLUDE STXITER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
*                                                                               
         TITLE 'CTSCANPROF - SCAN AND PRINT OUT PROFILES - SPARTAN'             
***********************************************************************         
*                                                                     *         
*  PURPOSE:  READ USER PROFILE RECS FROM CTUSERS, AND PRINT OUT KEY   *         
*            AND PROFILE DATA (IN EBCDIC).  VERY SPARTAN, BUT IT      *         
*            WILL WORK.                                               *         
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
* SEQUENTIALLY READ ALL RECS IN CTUSER AND PRINT PROFILES                       
         XC    KEY,KEY                                                          
         LA    R7,IO                                                            
         USING CTUREC,R7                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',KEY,IO                    
MX10     DS    0H                                                               
*                                                                               
         CLI   CTUKTYP,CTUKTYPQ    IS THIS A USER PROFILE REC?                  
         BH    MXB                                                              
         BNE   MX30                NO - GET NEXT REC                            
*                                                                               
         CLI   CTUKSYS,C'P'                                                     
         BH    MXB                                                              
         BNE   MX30                                                             
         CLC   CTUKPROG(3),=C'12A'                                              
***         CLC   CTUKPROG+1(2),=C'WR'                                          
         BH    MXB                                                              
         BNE   MX30                                                             
         OC    CTUKAGY,CTUKAGY     IGNORE FIELD RECORDS                         
         BZ    MX30                                                             
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'72'                                                     
         BAS   RE,GETEL                                                         
*** WHOEVER IS IN HERE NEXT: FIX THIS BAD BRANCH!                               
         BNE   *+10                                                             
         USING CTPVD,R6                                                         
         CLI   CTPVALUE+6,C'Y'     CHECK MARK BUYS                              
         BNE   MX30                                                             
         DROP  R6                                                               
*                                                                               
* PRINT OUT RECORD KEY...                                                       
         LA    R2,P                                                             
         MVC   0(L'CTUKSYS,R2),CTUKSYS                                          
         LA    R2,L'CTUKSYS+2(R2)                                               
         MVC   0(L'CTUKPROG,R2),CTUKPROG                                        
         LA    R2,L'CTUKPROG+2(R2)                                              
         MVC   0(L'CTUKAGY,R2),CTUKAGY                                          
         LA    R2,L'CTUKAGY+2(R2)                                               
         MVC   0(L'CTUKMED,R2),CTUKMED                                          
         LA    R2,L'CTUKMED+2(R2)                                               
         MVC   0(L'CTUKCLT,R2),CTUKCLT                                          
         LA    R2,L'CTUKCLT+10(R2)                                              
*&&DO                                                                           
*                                                                               
* PRINT OUT RECORD DATA...                                                      
         LA    R6,IO                                                            
         MVI   ELCODE,X'72'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING CTPVD,R6                                                         
         MVC   0(L'CTPVALUE,R2),CTPVALUE                                        
         DROP  R6                                                               
*&&                                                                             
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
MX30     DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMRSEQ',=C'CTFILE',KEY,IO                    
         TM    DMCB+8,X'80'                                                     
         BNZ   *+8                                                              
         B     MX10                                                             
         DROP  R7                                                               
MXB      XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
UTL      DC    F'0',X'0A',XL251'00'                                             
SSB      DC    H'0',X'FF',1021X'00'                                             
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
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009CTSCANPROF03/25/15'                                      
         END                                                                    
