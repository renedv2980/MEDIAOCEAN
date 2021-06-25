*          DATA SET NEOLDMPA2B AT LEVEL 006 AS OF 05/01/02                      
*PHASE T31E2BA                                                                  
         TITLE 'T31E2B - MPA PROGRAM LISTING'                                   
T31E2B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MPPR**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R9,ANETWS2          ARGS FROM EDIT AND W/S                       
         USING MPACOM,R9                                                        
         EJECT                                                                  
*              CONTROL READING OF MPA FILE                                      
         SPACE 3                                                                
         USING ITEMD,R2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MRKEY,R4                                                         
         MVI   MRKMAJOR,C'M'                                                    
         MVI   MRMED,C'P'                                                       
         MVI   MRSRC,C'N'                                                       
         LA    R2,STACK                                                         
         SR    R3,R3                                                            
         SPACE 1                                                                
MP2      MVC   FILENAME,=C'DEMDIR  '                                            
         MVC   DUB,MRPNUM          GET NEXT PROGRAM                             
         LA    R4,KEY                                                           
         LH    R1,DUB                                                           
         LA    R1,1(R1)                                                         
         STH   R1,DUB                                                           
         MVC   MRPNUM,DUB                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BNE   MP20                                                             
         MVC   MRMBOOK,BOOK+1      FOR SELECTED BOOK                            
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    MP4                                                              
         MVC   KEY,KEYSAVE                                                      
         B     MP2                                                              
         SPACE 1                                                                
MP4      MVC   FILENAME,=C'DEMFIL  '                                            
         XC    MRKMKT,MRKMKT                                                    
         GOTO1 HIGH                                                             
         LA    R4,IO                                                            
         LA    R6,MRFRSTEL                                                      
         SPACE 1                                                                
MP6      CLI   0(R6),0             PICK UP ELEMENTS                             
         BE    MP14                                                             
         CLI   0(R6),X'21'                                                      
         BNE   MP12                                                             
         SPACE 1                                                                
         USING MPGMELEM,R6                                                      
MP10     ZIC   R7,1(R6)                                                         
         SH    R7,=H'2'                                                         
         GOTO1 CHOPPER,DMCB,((R7),MPGMNAME),(16,ITEMNAME),1                     
         SPACE 1                                                                
MP12     ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     MP6                                                              
         SPACE 1                                                                
MP14     MVC   ITEMCODE,MRPNUM                                                  
         LA    R2,20(R2)                                                        
         LA    R3,1(R3)                                                         
         CH    R3,=H'192'                                                       
         BE    MP20                                                             
         B     MP2                                                              
         EJECT                                                                  
*              HANDLE AND PRINT STACK                                           
         SPACE 3                                                                
MP20     CLI   SEQOPT,C'N'         SORT ON ALPHA OR DAY/TIME                    
         BE    MP24                                                             
         CLI   SEQOPT,C'D'                                                      
         BE    MP22                                                             
         GOTO1 XSORT,DMCB,STACK,(R3),20,16,0                                    
         B     MP24                                                             
         SPACE 1                                                                
MP22     GOTO1 XSORT,DMCB,STACK,(R3),20,4,16                                    
         SPACE 1                                                                
MP24     LA    R2,STACK                                                         
         LA    R0,48                                                            
         BAS   RE,MP26             PRINT JUST 2 PAGES                           
         LA    R2,1920(R2)                                                      
         BAS   RE,MP26                                                          
*                                                                               
         XC    FILENAME,FILENAME   RESTORE FILENAME                             
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
MP26     NTR1                                                                   
         SPACE 1                                                                
MP26B    LA    R3,P+1              PRINT 48 LINES 2 UP                          
         BAS   RE,MP28                                                          
         LA    R2,960(R2)                                                       
         LA    R3,P+44                                                          
         BAS   RE,MP28                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SH    R2,=H'940'                                                       
         BCT   R0,MP26B                                                         
         B     XIT                                                              
         SPACE 1                                                                
MP28     NTR1                                                                   
         OC    0(20,R2),0(R2)      DISPLAY AN ITEM                              
         BZ    XIT                                                              
         USING ITEMD,R2                                                         
         MVC   0(16,R3),ITEMNAME                                                
         EDIT  (2,ITEMCODE),(4,17(R3)),FILL=0                                   
         MVC   22(3,R3),=C'NBC'                                                 
         CLC   ITEMCODE,=H'2000'                                                
         BL    XIT                                                              
         MVC   22(3,R3),=C'ABC'                                                 
         CLC   ITEMCODE,=H'3000'                                                
         BL    XIT                                                              
         MVC   22(3,R3),=C'CBS'                                                 
         B     XIT                                                              
         SPACE 1                                                                
DAYL     DC    C'M-FMONTUEWEDTHUFRISATSUNM-SVAR'                                
         EJECT                                                                  
*              ROUTINE FOR HEAD HOOK                                            
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H4+8(6),SPLBOOK                                                  
         CLC   P+40(40),SPACES                                                  
         BE    *+16                                                             
         MVC   H7+43(40),H7                                                     
         MVC   H8+43(40),H8                                                     
         MVC   H4+44(17),=C'DAY/TIME SEQUENCE'                                  
         CLI   SEQOPT,C'D'                                                      
         BE    XIT                                                              
         MVC   H4+44(17),=C'NUMERIC SEQUENCE '                                  
         CLI   SEQOPT,C'N'                                                      
         BE    XIT                                                              
         MVC   H4+44(19),=C'ALPHABETIC SEQUENCE'                                
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 3                                                                
*              DSECT FOR MODULE                                                 
         SPACE 1                                                                
MPACOM   DSECT                                                                  
** PASSED  FROM  EDIT                                                           
SEQOPT   DS    CL1                                                              
BOOK     DS    CL3                                                              
*                                                                               
*                                                                               
STACK    DS    3840C                                                            
         SPACE 3                                                                
*              DSECT FOR STACK ITEM                                             
         SPACE 1                                                                
ITEMD    DSECT                                                                  
ITEMNAME DS    CL16                                                             
ITEMDAY  DS    CL1                                                              
ITEMSQH  DS    CL1                                                              
ITEMCODE DS    CL2                                                              
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFBD                                                       
       ++INCLUDE DEMPAFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEOLDMPA2B05/01/02'                                      
         END                                                                    
