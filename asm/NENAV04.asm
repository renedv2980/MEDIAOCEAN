*          DATA SET NENAV04    AT LEVEL 168 AS OF 07/16/18                      
*PHASE T31804B                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31804   TITLE 'NENAV04 - FALINK REPLIES'                                       
T31804   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV04**,R8,RR=R2                                              
         LA    R7,2048(R8)                                                      
         LA    R7,2048(R7)                                                      
         USING T31804+8192,R7                                                   
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         ST    R2,MYRELO                                                        
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         BAS   RE,GTALPHID          GET USER ID ALPHA                           
         GOTO1 VALIMED                                                          
*                                                                               
* ROUTE THE ELEMENTS TO THE RIGHT ROUTINES                                      
*                                                                               
         CLC   =X'0100',SVRCVEL                                                 
         BNE   ROUT010                                                          
         BAS   RE,STAINFO          RETURN STATION INFO                          
         J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
ROUT010  DS    0H                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
EXITN    LHI   RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1                                                                   
*                                                                               
*                                                                               
*  ROUTINE SETS UP SECRET BLOCK FOR FUTURE SECURITY CALLS                       
*                                                                               
BLDSECRT NTR1                                                                   
*                                                                               
* INITIALIZE SECRET                                                             
*                                                                               
         L     RE,ATWA                                                          
         AH    RE,=Y(SVSECRET-TWAD)                                             
         ST    RE,ASECBLK                                                       
*                                                                               
         SPACE 1                                                                
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS       OR HAVE LIMIT ACCESS                    
         BZ    BLDSECEX                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDSECEX J     EXIT                                                             
*                                                                               
*  READ CONTROL FILE GET THE ID RECORD AND STORE                                
*  ALPHA ID IN LOCAL STORAGE                                                    
*                                                                               
GTALPHID NTR1                                                                   
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
*                                                                               
         LA    R3,KEY                                                           
         USING CTIREC,R3                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKNUM,TWAUSRID                                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 AIOCALL,DMCB,CTL+FIL+HIGH,AIO                                    
         L     R3,AIO                                                           
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',=CL8'CTFILE'),(X'02',(R3)),0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
         L     R6,12(R1)                                                        
*                                                                               
         MVC   ALPHID(10),2(R6)                                                 
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*  RETURN MASTER/STATION INFO                                                   
*                                                                               
STAINFO  NTR1                                                                   
         MVC   AIO,AIO3                                                         
*                                                                               
         LA    R3,KEY                                                           
         USING STARECD,R3                                                       
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),BNET                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,QAGY                                                     
         MVC   STAKCLT,=XL3'F0F0F0'                                             
*                                                                               
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO                                    
         CLC   KEY(9),KEYSAVE                                                   
         JNE   STAINFOX                                                         
         L     R3,AIO                                                           
*                                                                               
         LHI   R1,X'101'                                                        
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R4,STAKCALL         NETWORK                                      
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,STAKCALL         DEFAULT NTI TO NETWORK                       
         CLI   SNTISTA,0                                                        
         JE    SINFO02                                                          
         CLI   SNTISTA,C' '                                                     
         JE    SINFO02                                                          
         LA    R4,SNTISTA          NTI STATION                                  
SINFO02  LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,STYPE            MEDIA TYPE                                   
         LHI   R1,X'03'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,SPTYPE           POSTING TYPE                                 
         LHI   R1,X'04'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
STAINFOX J     EXIT                                                             
         LTORG                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
SPTFILE  DC    CL8'SPTFILE'                                                     
UNTFILE  DC    CL8'UNTFILE'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
XSPFILE  DC    CL8'XSPFILE'                                                     
GNDDIR   DC    CL8'GENDIR '                                                     
GNDFILE  DC    CL8'GENFILE'                                                     
*                                                                               
       ++INCLUDE NENAVWRK                                                       
*                                                                               
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
MYRELO   DS    F                                                                
ALPHID   DS    CL10                ALPHA ID                                     
         ORG                                                                    
*                                                                               
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENPTYP                                                      
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE SPTRNFEED                                                      
       ++INCLUDE SPGENWBFLT                                                     
       ++INCLUDE SPGENBARU                                                      
       ++INCLUDE SPGENPXC                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE NESYSFAC                                                       
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE FASSB                                                          
       ++INCLUDE FAUTL                                                          
       ++INCLUDE GEGENSDR                                                       
       ++INCLUDE FAXPEQUS                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE GEGENTOK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'168NENAV04   07/16/18'                                      
         END                                                                    
