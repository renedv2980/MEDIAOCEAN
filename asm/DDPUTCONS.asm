*          DATA SET DDPUTCONS  AT LEVEL 003 AS OF 04/15/10                      
*PHASE PUTCONSA                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'PUT A SINGLE LINE TO THE OPERATOR CONSOLE'                      
PUTCONS  CSECT                                                                  
*                                                                               
* THIS PROGRAM WILL READ A SINGLE RECORD FROM AN INPUT DATASET, AND             
* PUT IT TO THE CONSOLE. (VERY HANDY FOR THE AUTONOTE FEATURE.)                 
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,PUTCONS,=V(REGSAVE)                                            
*                                                                               
         OPEN  MSGFILE                                                          
*                                                                               
         GET   MSGFILE,MESSAGEL                                                 
*                                                                               
         L     R2,MESSAGEL         RDW                                          
         SRL   R2,16               SHIFT RECLEN TO LOW-ORDER 4 BYTES            
         SHI   R2,4                SUBTRACT L'RDW                               
         STH   R2,WTOLEN           L'MESSAGE                                    
*                                                                               
* THE AUTONOTE FEATURE DOES NOT SUPPORT ALL CHARACTERS. IF AN INVALID           
* CHARACTER IS INCLUDED IN AN AUTONOTE MESSAGE, THE ENTIRE SUBJECT LINE         
* OF THE AUTONOTE IS TRUNCATED ON ITS WAY TO LOTUS NOTES BY SOME                
* OBSCURE IBM TRANSLATION MODULE THAT IS NO LONGER SUPPORTED BY ANYONE.         
* SO AS A PREEMPTIVE STRIKE, WE TRANSLATE THE INVALID CHARACTERS TO             
* QUESTION MARKS.                                                               
*                                                                               
         CLC   =C'AUTONOTE',MESSAGE  MAINFRAME-TO-NOTES AUTONOTE?               
         BNE   PUTWTO              NO: LEAVE IT ALONE                           
         BCTR  R2,0                                                             
         EX    R2,*+8              CONVERT INVALID CHARACTERS                   
         B     *+10                                                             
         TR    MESSAGE(0),AUTONOTE_TRTAB                                        
*                                                                               
PUTWTO   DS    0H                                                               
         LA    R9,WTOLEN                                                        
         WTO   TEXT=(R9)                                                        
*                                                                               
CLOSE    DS    0H                                                               
         CLOSE MSGFILE                                                          
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         DS    0F                                                               
MESSAGEL DS    H                   L'MESSAGE (IN INPUT RECORD)                  
*                                    (INCLUDES L'RDW)                           
WTOLEN   DS    H                   L'MESSAGE (FOR WTO)                          
*                                    (INCLUDES ONLY L'MESSAGE)                  
MESSAGE  DS    CL255               MESSAGE (SUBJECT ONLY)                       
         SPACE 3                                                                
MSGFILE  DCB   DDNAME=MSGFILE,DSORG=PS,MACRF=GM,RECFM=VB,LRECL=255,    +        
               BLKSIZE=0,EODAD=CLOSE                                            
         SPACE 3                                                                
* TRANSLATE INVALID AUTONOTE CHARACTERS TO VALID ONES. THE INVALID              
* CHARACTERS ARE:                                                               
*   THE "NOT" CHARACTER ("^"): TRANSLATE TO BLANK                               
*   THE SINGLE-QUOTE    ("'"): TRANSLATE TO BLANK                               
*   ANYTHING < X'40'    (" "): TRANSLATE TO A QUESTION MARK ("?")               
*                                                                               
AUTONOTE_TRTAB DS 0D                                                            
         DC    X'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'                              
         DC    X'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'                              
         DC    X'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'                              
         DC    X'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'                              
         DC    X'404142434445464748494A4B4C4D4E4F'                              
         DC    X'505152535455565758595A5B5C5D5E40'                              
         DC    X'606162636465666768696A6B6C6D6E6F'                              
         DC    X'707172737475767778797A7B7C407E7F'                              
         DC    X'808182838485868788898A8B8C8D8E8F'                              
         DC    X'909192939495969798999A9B9C9D9E9F'                              
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'                              
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'                              
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'                              
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'                              
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'                              
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'                              
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DDPUTCONS 04/15/10'                                      
         END                                                                    
