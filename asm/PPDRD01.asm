*          DATA SET PPDRD01    AT LEVEL 011 AS OF 05/01/02                      
*PHASE T40901A,+0    ** NOTE "A" APPENDED TO PHASE                              
         TITLE 'T40901  PRINTPAK LOGICAL FILE MAINT.  DIVREC'                   
*                                                                               
****  CHANGE LOG                                                                
*                                                                               
*  SMYE  05/01/96  CHANGED DIV REC ELEM AND RECORD LENGTHS                      
*                                                                               
T40901   CSECT                                                                  
         NMOD1 0,T40901                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T409FFD,RA                                                       
         USING PDIVREC,R9                                                       
         EJECT                                                                  
         MVC   PDIVKEY,KEY                                                      
****     MVC   PDIVLEN(2),=X'00AD'                                              
****     MVC   PDIVELEM(2),=X'038C'                                             
         MVC   PDIVLEN(2),=X'0041'    NEW RECORD AND ELEMENT LENGTHS            
         MVC   PDIVELEM(2),=X'0320'    (FROM 140 + 33  TO  32 + 33)             
         CLI   BACT,1                                                           
         BE    DIVSCRN                                                          
         MVC   KEY+27(4),DIVADDR                                                
         BAS   RE,GETREC                                                        
DIVSCRN  CLI   BYTE2,0                                                          
         BNE   FORMATD                                                          
*                                DIVISION SCREEN IN TWA SO EDIT IT              
*                                UNLESS ACTION = DISPLAY                        
         CLI   BACT,3                                                           
         BE    FORMATD                                                          
         LA    R2,DIVNAMEH                                                      
         BAS   RE,ANY                                                           
         XC    PDIVNAME,PDIVNAME                                                
         MVC   PDIVNAME,DIVNAME                                                 
         CLC   PDIVKDIV,=C'999'                                                 
         BE    CKACT            OMIT PROFILE CHECK                              
         LA    R3,DIVERR1                                                       
         CLI   CPROFLE,C'1'                                                     
         BE    CKACT                                                            
         CLI   CPROFLE,C'2'                                                     
         BNE   ERROR                                                            
CKACT    CLI   BACT,1                                                           
         BNE   DIVCHG                                                           
         BAS   RE,ADDREC                                                        
         B     DONE                                                             
*                                                                               
DIVCHG   BAS   RE,PUTREC                                                        
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
FORMATD  LA    R6,HDRLAST                                                       
         GOTO1 VCALLOV,WORK,(R6),X'D90409F1'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRERR                                                           
         CLI   BACT,1                                                           
         BNE   PUTFLD                                                           
         FOUT  DIVNAMEH,SPACES,20                                               
         LA    R2,DIVNAMEH                                                      
         B     EXIT                                                             
*                                                                               
PUTFLD   FOUT  DIVNAMEH,PDIVNAME,20                                             
         CLI   BACT,2                                                           
         BNE   DONE                                                             
         LA    R2,DIVNAMEH                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
SPACES   DC    CL40' '                                                          
DIVERR1  EQU   92                                                               
VIRERR   DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGENEROL                                                       
*                                                                               
       ++INCLUDE GENOLD                                                         
       ++INCLUDE PDIVREC                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPDRDFFD                                                       
         ORG   HDRLAST                                                          
       ++INCLUDE PPDRDF1D                                                       
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
**PAN#1  DC    CL21'011PPDRD01   05/01/02'                                      
         END                                                                    
