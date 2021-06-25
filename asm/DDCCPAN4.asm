*          DATA SET DDCCPAN4   AT LEVEL 158 AS OF 05/29/97                      
*PHASE CCPAN4,*                                                                 
*INCLUDE DATCON                                                                 
         PRINT NOGEN                                                            
         SPACE 1                                                                
CCPAN2   CSECT                                                                  
         NBASE WORKX-WORKD,XXRMORXX,R9,WORK=A(WORKAREA),CLEAR=YES               
         USING WORKD,RC                                                         
         SPACE 1                                                                
*                                  INITIALISE STORAGE                           
INIT01   OPEN  (PANIN,INPUT)                                                    
         OPEN  (PANOUT,OUTPUT)                                                  
         BAS   RE,MAIN                                                          
         CLOSE PANIN                                                            
         CLOSE PANOUT                                                           
*                                                                               
XBASE    XBASE                                                                  
         EJECT                                                                  
*************************************************************                   
*        OPEN OUTPUT DATA SET                               *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
*                                                                               
MAIN010  GET   PANIN               GET A LINE                                   
         LR    R2,R1                                                            
*                                                                               
         MVC   BYTE,22(R2)         SAVE BOOK CHARACTER                          
*                                                                               
         CLI   22(R2),C'B'         SORT OUT ABCD                                
         BNE   *+10                                                             
         MVC   22(4,R2),=C' B  '                                                
         CLI   22(R2),C'C'                                                      
         BNE   *+10                                                             
         MVC   22(4,R2),=C'  C '                                                
         CLI   22(R2),C'D'                                                      
         BNE   *+10                                                             
         MVC   22(4,R2),=C'   D'                                                
*                                                                               
MAIN020  CLC   0(8,R2),SPACES                                                   
         BE    MAIN025                                                          
         OC    0(8,R2),0(R2)                                                    
         BE    MAIN025                                                          
         CLC   CSECTNOW,0(R2)      SAME CSECT                                   
         BE    MAIN030                                                          
MAIN025  LA    R3,CBLOCK           RESET CBLOCK                                 
         ST    R3,ACBLOCK                                                       
         B     MAIN400                                                          
*                                                                               
MAIN030  MVC   CSECTNOW,0(R2)      RESET CURRENT CSECT                          
*                                                                               
         CLI   BYTE,C'D'           MERGE D TYPE BOOKS                           
         BE    MAIN100                                                          
*                                                                               
         CLI   BYTE,C'C'           IF C TYPE BOOK HAS NO PHASE                  
         BNE   MAIN040                                                          
         CLC   60(8,R2),SPACES     IGNORE IT                                    
         BE    MAIN010                                                          
*                                                                               
MAIN040  L     R3,ACBLOCK                                                       
         MVC   0(80,R3),0(R2)      ADD TO TABLE                                 
         LA    R3,80(R3)                                                        
         ST    R3,ACBLOCK                                                       
         MVI   80(R3),0            END MARKER                                   
         B     MAIN010                                                          
         EJECT                                                                  
************************************************************                    
*        TEST FIELDS FOR SIMILARITYS                       *                    
************************************************************                    
         SPACE 1                                                                
MAIN100  LA    R3,CBLOCK           CHECK FOR MERGEABILITY                       
         USING PLINED,R2                                                        
         USING CLINED,R3                                                        
*                                                                               
MAIN110  MVI   FLAG,0                                                           
         CLI   0(R3),0             FAIL IF NO ENTRIES                           
         BE    MAIN300                                                          
*                                                                               
         CLC   CCSECT,SPACES       SAME CSECT OR SPACES                         
         BE    *+24                                                             
         CLC   PCSECT,SPACES                                                    
         BE    *+14                                                             
         CLC   PCSECT,CCSECT                                                    
         BNE   MAIN190                                                          
*                                                                               
         CLC   CCDATE,SPACES       SAME DATE OR SPACES                          
         BE    *+24                                                             
         CLC   PCDATE,SPACES                                                    
         BE    *+14                                                             
         CLC   PCDATE,CCDATE                                                    
         BNE   MAIN190                                                          
*                                                                               
         CLC   CLEVEL,SPACES       SAME LEVEL OR SPACES                         
         BE    *+24                                                             
         CLC   PLEVEL,SPACES                                                    
         BE    *+14                                                             
         CLC   PLEVEL,CLEVEL                                                    
         BNE   MAIN190                                                          
*                                                                               
         CLC   CT1,SPACES          SAME TYPE OR SPACES                          
         BE    *+24                                                             
         CLC   PT1,SPACES                                                       
         BE    *+14                                                             
         CLC   PT1,CT1                                                          
         BNE   MAIN190                                                          
*                                                                               
         CLC   CT2,SPACES          SAME TYPE OR SPACES                          
         BE    *+24                                                             
         CLC   PT2,SPACES                                                       
         BE    *+14                                                             
         CLC   PT2,CT2                                                          
         BNE   MAIN190                                                          
*                                                                               
         CLC   CT3,SPACES          SAME TYPE OR SPACES                          
         BE    *+24                                                             
         CLC   PT3,SPACES                                                       
         BE    *+14                                                             
         CLC   PT3,CT3                                                          
         BNE   MAIN190                                                          
*                                                                               
         CLC   CT4,SPACES          SAME TYPE OR SPACES                          
         BE    *+24                                                             
         CLC   PT4,SPACES                                                       
         BE    *+14                                                             
         CLC   PT4,CT4                                                          
         BNE   MAIN190                                                          
*                                                                               
         CLC   CCTRY,SPACES        SAME COUNTRY OR SPACES                       
         BE    *+24                                                             
         CLC   PCTRY,SPACES                                                     
         BE    *+14                                                             
         CLC   PCTRY,CCTRY                                                      
         BNE   MAIN190                                                          
*                                                                               
         CLC   CBOOK,SPACES        SAME BOOK OR SPACES                          
         BE    *+24                                                             
         CLC   PBOOK,SPACES                                                     
         BE    *+14                                                             
         CLC   PBOOK,CBOOK                                                      
         BNE   MAIN190                                                          
*                                                                               
         CLC   CRMBOOK,SPACES      SAME RMBOOK OR SPACES                        
         BE    *+24                                                             
         CLC   PRMBOOK,SPACES                                                   
         BE    *+14                                                             
         CLC   PRMBOOK,CRMBOOK                                                  
         BNE   MAIN190                                                          
*                                                                               
         CLC   CLMBOOK,SPACES      SAME LMBOOK OR SPACES                        
         BE    *+24                                                             
         CLC   PLMBOOK,SPACES                                                   
         BE    *+14                                                             
         CLC   PLMBOOK,CLMBOOK                                                  
         BNE   MAIN190                                                          
*                                                                               
         CLC   CPHASE,SPACES       SAME PHASE OR SPACES                         
         BE    *+24                                                             
         CLC   PPHASE,SPACES                                                    
         BE    *+14                                                             
         CLC   PPHASE,CPHASE                                                    
         BNE   MAIN190                                                          
*                                                                               
         OC    0(80,R2),0(R3)      MERGE IT                                     
         MVI   CT0,C'M'            FLAG IT AS MERGED                            
*                                                                               
         CLC   PT1(4),=C'ABCD'                                                  
         BE    MAIN300             EXIT WHEN WE HAVE THE FULL SET               
*                                                                               
MAIN190  LA    R3,80(R3)           NEXT ENTRY                                   
         CLI   0(R3),0                                                          
         BNE   MAIN110                                                          
         EJECT                                                                  
************************************************************                    
*        ALL MERGING DONE SO WRITE IT                      *                    
************************************************************                    
         SPACE 1                                                                
MAIN300  MVI   PT0,C' '            CLEAR THIS FLAG                              
         PUT   PANOUT,(R2)                                                      
         B     MAIN010             GO GET NEXT RECORD                           
*                                                                               
PANXX    MVI   FLAG,C'X'                                                        
MAIN400  LA    R3,CBLOCK           PRINT OUT WHATS LEFT                         
         CLI   0(R3),0                                                          
         BE    MAIN405                                                          
*                                                                               
MAIN401  CLI   CT0,C'M'            IGNORE MERGED BOOKS                          
         BE    MAIN402                                                          
         PUT   PANOUT,(R3)         WRITE IT OUT                                 
*                                                                               
MAIN402  LA    R3,80(R3)           NEXT                                         
         CLI   0(R3),0                                                          
         BNE   MAIN401                                                          
MAIN405  MVI   CBLOCK,0                                                         
         CLI   FLAG,C'X'                                                        
         BNE   MAIN030                                                          
         B     XIT1                                                             
*                                                                               
XIT1     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*   DATA CONTROL BLOCKS                                     *                   
*************************************************************                   
         SPACE 1                                                                
PANIN    DCB   DDNAME=PANIN,DSORG=PS,MACRF=GL,EODAD=PANXX                       
PANOUT   DCB   DDNAME=PANOUT,DSORG=PS,RECFM=FB,MACRF=(PM),             X        
               BLKSIZE=3120,LRECL=80                                            
         EJECT                                                                  
*************************************************************                   
*        LITERALS                                           *                   
*************************************************************                   
         SPACE 1                                                                
SPACES   DC    80C' '                                                           
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        WORK AREA                                          *                   
*************************************************************                   
         SPACE 1                                                                
CBLOCK   DS    4000CL80                                                         
         SPACE 1                                                                
WORKAREA DC  1000D'0'                                                           
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
*                                                                               
WORK     DS    CL166                                                            
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
SAVERE   DS    F                                                                
HALF     DS    H                                                                
FLAG     DS    X                                                                
BYTE     DS    X                                                                
DMCB     DS    6F                                                               
*                                                                               
ACBLOCK  DS    A                                                                
CSECTNOW DS    CL8                                                              
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
PLINED   DSECT                                                                  
PCSECT   DS    CL8                                                              
         DS    CL1                                                              
PCDATE   DS    CL6                                                              
         DS    CL3                                                              
PLEVEL   DS    CL3                                                              
PT0      DS    CL1                                                              
PT1      DS    CL1                                                              
PT2      DS    CL1                                                              
PT3      DS    CL1                                                              
PT4      DS    CL1                                                              
PCTRY    DS    CL2                                                              
         DS    CL1                                                              
PBOOK    DS    CL10                                                             
         DS    CL1                                                              
PRMBOOK  DS    CL8                                                              
         DS    CL1                                                              
PLMBOOK  DS    CL10                                                             
         DS    CL1                                                              
PPHASE   DS    CL8                                                              
*                                                                               
CLINED   DSECT                                                                  
CCSECT   DS    CL8                                                              
         DS    CL1                                                              
CCDATE   DS    CL6                                                              
         DS    CL3                                                              
CLEVEL   DS    CL3                                                              
CT0      DS    CL1                                                              
CT1      DS    CL1                                                              
CT2      DS    CL1                                                              
CT3      DS    CL1                                                              
CT4      DS    CL1                                                              
CCTRY    DS    CL2                                                              
         DS    CL1                                                              
CBOOK    DS    CL10                                                             
         DS    CL1                                                              
CRMBOOK  DS    CL8                                                              
         DS    CL1                                                              
CLMBOOK  DS    CL10                                                             
         DS    CL1                                                              
CPHASE   DS    CL8                                                              
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'158DDCCPAN4  05/29/97'                                      
         END                                                                    
