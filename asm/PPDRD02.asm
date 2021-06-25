*          DATA SET PPDRD02    AT LEVEL 009 AS OF 05/01/02                      
*PHASE T40902A,+0    ** NOTE "A" APPENDED TO PHASE                              
         TITLE 'T40902  PRINTPAK LOGICAL FILE MAINT.  REG REC'                  
*                                                                               
*****  CHANGE LOG                                                               
*                                                                               
*  SMYE  05/06/96   CHANGED REG REC ELEM AND RECORD LENGTHS                     
*                                                                               
T40902   CSECT                                                                  
         NMOD1 0,T40902                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T409FFD,RA                                                       
         USING PREGREC,R9                                                       
         EJECT                                                                  
         MVC   PREGKEY,KEY                                                      
****     MVC   PREGELEM(2),=X'048C'                                             
****     MVC   PREGLEN(2),=X'00AD'                                              
         MVC   PREGELEM(2),=X'0420'    NEW RECORD AND ELEMENT LENGTHS           
         MVC   PREGLEN(2),=X'0041'      (FROM 140 + 33  TO  32 + 33)            
         CLI   BACT,1                                                           
         BE    REGSCRN                                                          
         MVC   KEY+27(4),REGADDR                                                
         BAS   RE,GETREC                                                        
REGSCRN  CLI   BYTE2,0                                                          
         BNE   FORMAT                                                           
*            REGION SCREEN IN TWA SO EDIT IT UNLESS                             
*                    ACTION = DISPLAY                                           
         CLI   BACT,3                                                           
         BE    FORMAT                                                           
         LA    R2,REGNAMEH                                                      
         BAS   RE,ANY                                                           
         XC    PREGNAME,PREGNAME                                                
         MVC   PREGNAME,REGNAME                                                 
         CLI   BACT,1                                                           
         BNE   REGCHG                                                           
         BAS   RE,ADDREC                                                        
         B     DONE                                                             
*                                                                               
REGCHG   BAS   RE,PUTREC                                                        
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
FORMAT   LA    R6,HDRLAST                                                       
         GOTO1 VCALLOV,WORK,(R6),X'D90409F2'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRERR                                                           
         CLI   BACT,1                                                           
         BNE   PUTFLD                                                           
         FOUT  REGNAMEH,SPACES,20                                               
         LA    R2,REGNAMEH                                                      
         B     EXIT                                                             
*                                                                               
PUTFLD   FOUT  REGNAMEH,PREGNAME,20                                             
         CLI   BACT,2                                                           
         BNE   DONE                                                             
         LA    R2,REGNAMEH                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
SPACES   DC    CL40' '                                                          
VIRERR   DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGENEROL                                                       
*                                                                               
       ++INCLUDE GENOLD                                                         
       ++INCLUDE PREGREC                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPDRDFFD                                                       
         ORG   HDRLAST                                                          
       ++INCLUDE PPDRDF2D                                                       
         ORG   T409FFD                                                          
         DS    CL16                                                             
BREC     DS    CL1                                                              
BACT     DS    CL1                                                              
OLNUM    DS    CL1                                                              
DIVADDR  DS    F                                                                
REGADDR  DS    F                                                                
DSTADDR  DS    F                                                                
CPROFLE  DS    CL20                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009PPDRD02   05/01/02'                                      
         END                                                                    
