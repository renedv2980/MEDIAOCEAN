*          DATA SET DDTRPACK   AT LEVEL 001 AS OF 07/28/04                      
*PHASE T00AFEA                                                                  
                                                                                
***********************************************************************         
*                                                                     *         
* R1 ==> C'P',AL3(ADID CODE)                                          *         
*        A(8 BYTE OUTPUT PACKED ADID CODE)                            *         
*                                                                     *         
*        C'U',AL3(8-BYTE PACKED ADID CODE)                            *         
*        A(12 BYTE OUTPUT ADID CODE)                                  *         
*                                                                     *         
* PROGRAM RETURNS CC NEQ IF CODE INVALID                              *         
***********************************************************************         
                                                                                
TRPACK   TITLE 'ALPHANUMERIC 12 BYTE ADID TO 8 BYTE BINARY'                     
TRPACK   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,*TRPACK*,CLEAR=YES                                         
         USING WORKD,RC                                                         
                                                                                
         SR    R2,R2                                                            
         ICM   R2,7,1(R1)          R2=A(INPUT)                                  
         SR    R3,R3                                                            
         ICM   R3,7,5(R1)          R3=A(OUTPUT)                                 
                                                                                
         CLI   0(R1),C'P'                                                       
         BE    PACK                                                             
         CLI   0(R1),C'U'                                                       
         BE    UNPK                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CONVERT 12 BYTE CHARACTER AD-ID TO 8 BYTE PACKED FORMAT             *         
***********************************************************************         
                                                                                
PACK     MVC   ADID,0(R2)                                                       
                                                                                
         LA    R1,ADID             FIRST 8 CAN'T CONTAIN SPACES                 
         LHI   R0,8                OR SPECIAL CHARACTERS                        
         CLI   0(R1),C'A'                                                       
         BL    NEQXIT                                                           
         AHI   R1,1                                                             
         BCT   R0,*-12                                                          
                                                                                
         TR    ADID,PACKTAB        TRANSLATE CHARACTERS                         
                                                                                
         LA    R1,ADID             TEST ALL CHARACTERS ARE VALID                
         LHI   R0,L'ADID                                                        
         CLI   0(R1),INVALID                                                    
         BE    NEQXIT                                                           
         AHI   R1,1                                                             
         BCT   R0,*-12                                                          
                                                                                
         LA    R5,ADID             CONVERT TO PACKED FORMAT                     
         LA    R6,ADIDP                                                         
         LHI   R0,4                                                             
PACK40   SR    RF,RF                                                            
         IC    RF,0(R5)            FIRST CHARACTER                              
         MHI   RF,L'UNPKTAB*L'UNPKTAB                                           
         LR    RE,RF                                                            
         SR    RF,RF                                                            
         IC    RF,1(R5)            SECOND CHARACTER                             
         MHI   RF,L'UNPKTAB                                                     
         AR    RE,RF                                                            
         SR    RF,RF                                                            
         IC    RF,2(R5)            THIRD CHARACTER                              
         AR    RE,RF                                                            
         STCM  RE,3,0(R6)                                                       
         AHI   R5,3                                                             
         AHI   R6,2                                                             
         BCT   R0,PACK40                                                        
                                                                                
         MVC   0(L'ADIDP,R3),ADIDP                                              
         B     EQXIT                                                            
         EJECT                                                                  
***********************************************************************         
* CONVERT 8 BYTE PACKED AD-ID TO 12 BYTE CHARACTER FORMAT             *         
***********************************************************************         
                                                                                
UNPK     MVC   ADIDP,0(R2)                                                      
                                                                                
         LA    R5,ADIDP                                                         
         LA    R6,ADID                                                          
         LHI   R4,4                NUMBER OF ITERATIONS                         
         LHI   R0,L'UNPKTAB                                                     
UNPK10   SR    RF,RF                                                            
         ICM   RF,3,0(R5)                                                       
         SR    RE,RE                                                            
         DR    RE,R0                                                            
         IC    RE,UNPKTAB(RE)      REMAINDER=3RD CHARACTER OF STRING            
         STC   RE,2(R6)                                                         
         SR    RE,RE                                                            
         DR    RE,R0                                                            
         IC    RE,UNPKTAB(RE)      REMAINDER=2ND CHARACTER OF STRING            
         STC   RE,1(R6)                                                         
         IC    RF,UNPKTAB(RF)      QUOTIENT=1ST CHARACTER OF STRING             
         STC   RF,0(R6)                                                         
UNPK20   AHI   R5,2                                                             
         AHI   R6,3                                                             
         BCT   R4,UNPK10                                                        
                                                                                
         MVC   0(L'ADID,R3),ADID                                                
                                                                                
EQXIT    CR    RB,RB               SET CC EQ                                    
         B     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
INVALID  EQU   X'FF'                                                            
                                                                                
PACKTAB  DC    256AL1(INVALID)                                                  
         ORG   PACKTAB+C' '                                                     
         DC    AL1(00)                                                          
         ORG   PACKTAB+C'A'                                                     
         DC    AL1(01,02,03,04,05,06,07,08,09)                                  
         ORG   PACKTAB+C'J'                                                     
         DC    AL1(10,11,12,13,14,15,16,17,18)                                  
         ORG   PACKTAB+C'S'                                                     
         DC    AL1(19,20,21,22,23,24,25,26)                                     
         ORG   PACKTAB+C'0'                                                     
         DC    AL1(27,28,29,30,31,32,33,34,35,36)                               
         ORG                                                                    
                                                                                
UNPKTAB  DC    C' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                         
                                                                                
         LTORG                                                                  
                                                                                
WORKD    DSECT                                                                  
ADID     DS    CL12                                                             
ADIDP    DS    CL8                                                              
DUB      DS    D                                                                
WORKL    EQU   *-WORKD                                                          
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDTRPACK  07/28/04'                                      
         END                                                                    
