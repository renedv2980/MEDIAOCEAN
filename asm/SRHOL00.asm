*          DATA SET SRHOL00    AT LEVEL 002 AS OF 09/09/14                      
*PHASE T15000A                                                                  
         TITLE '$HOLE - DISPLAY CONTENTS OF C/R TEST BUFFER'                    
HOLE     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,**$HOLE*,CLEAR=YES                                         
         USING WORKD,RC            RC=A(W/S)                                    
         MVC   IPARMS,0(R1)                                                     
         L     RA,ASYSFACS                                                      
         USING SYSFACD,RA          RA=A(SYSFACS)                                
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9         R9=A(COMFACS)                                
         L     R8,ATWA                                                          
         USING SRHOLFFD,R8         R8=A(TWA)                                    
*                                                                               
         L     R7,VSSB                                                          
         USING SSBD,R7                                                          
         OC    SSBCRADR,SSBCRADR   TEST C/R BUFFER ACTIVE                       
         BZ    EXIT                                                             
         OC    SSBCRLEN,SSBCRLEN                                                
         BZ    EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SEE IF WE WANT TO SQUASH UP THE HOLE                                *         
***********************************************************************         
         CLC   SRVP1(3),=C'FIX'                                                 
         BNE   MAIN02                                                           
         GOTO1 CCALLOV,DMCB,0,(C'H',0),0                                        
         CLI   4(R1),0                                                          
         BE    MAIN02                                                           
         MVC   SRVMSG(50),=CL50'Facpak is busy - try again'                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY BUFFER ADDRESS, SIZE ETC.                                   *         
***********************************************************************         
MAIN02   LA    R2,SRVD1                                                         
         MVC   0(L'ADDR,R2),ADDR   BUFFER ADDRESS                               
         AHI   R2,L'ADDR                                                        
         LA    R0,SSBCRADR+1                                                    
         GOTO1 CHEXOUT,DMCB,(R0),(R2),3,0                                       
         AHI   R2,6                                                             
*                                                                               
         MVC   0(L'LEN,R2),LEN     BUFFER LENGTH                                
         AHI   R2,L'LEN                                                         
         ICM   R0,15,SSBCRLEN                                                   
         EDIT  (R0),(9,(R2)),0,ALIGN=LEFT,ZERO=NOBLANK                          
         AR    R2,R0                                                            
*                                                                               
         MVC   0(L'AVAIL,R2),AVAIL BUFFER LENGTH                                
         AHI   R2,L'AVAIL                                                       
*                                                                               
         L     RF,SSBCRADR                                                      
         USING HOLED,RF                                                         
HDR02    OC    HOKEY,HOKEY                                                      
         BZ    HDR04                                                            
         ICM   RE,15,HPSLENH                                                    
         AHI   RE,HOLELQ                                                        
         AR    RF,RE               NEXT HOLE ENTRY                              
         B     HDR02                                                            
         DROP  RF                                                               
*                                                                               
HDR04    L     RE,SSBCRLEN                                                      
         S     RF,SSBCRADR                                                      
         SR    RE,RF                                                            
         EDIT  (RE),(9,(R2)),0,ALIGN=LEFT,ZERO=NOBLANK                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY LINES                                                       *         
***********************************************************************         
         LA    R3,SRVL1H                                                        
         USING FHD,R3                                                           
         LHI   R0,OLINEL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         DR    RE,R0                                                            
*                                                                               
         STH   RF,HALF             HALF=NUMBER OF COLUMNS ON LINE               
         LHI   RF,1                                                             
         STH   RF,HALF1            HALF1=CURRENT COLUMN                         
*                                                                               
         L     R2,SSBCRADR                                                      
         USING HOLED,R2                                                         
*                                                                               
DLINE02  LA    R3,SRVL1H           BACK TO START OF DISPLAY LINES               
*                                                                               
DLINE04  LH    R4,HALF1                                                         
         BCTR  R4,0                                                             
         MHI   R4,OLINEL                                                        
         LA    R4,FHDA(R4)         R4=A(OUTPUT BLOCK)                           
         OC    HOKEY,HOKEY                                                      
         BZ    EXIT                                                             
*                                                                               
         BRAS  RE,FORMAT           FORMAT OUTPUT BLOCK                          
*                                                                               
         ICM   RF,15,HPSLENH                                                    
         AHI   RF,HOLELQ                                                        
         AR    R2,RF               NEXT HOLE ENTRY                              
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FHLN                                                        
         BZ    *+8                                                              
         BXH   R3,RF,DLINE04                                                    
*                                                                               
         LH    RF,HALF1            NEXT COLUMN                                  
         AHI   RF,1                                                             
         CH    RF,HALF                                                          
         BH    EXIT                                                             
         STH   RF,HALF1                                                         
         B     DLINE02                                                          
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT A PHASE LINE - A(PHSHDR)/PHASE NAME/LENGTH                   *         
* ENTRY  R2=A(HOLED ENTRY)                                            *         
*        R4=A(OUTPUT AREA)                                            *         
***********************************************************************         
         USING HOLED,R2                                                         
         USING PROGSPCD,HOKEY                                                   
         USING OLINED,R4                                                        
FORMAT   NTR1                                                                   
         MVC   OPHASE,DELETED                                                   
         CLC   HOKEY(PSKEYL),EFFS  PHASE HAS BEEN DELETED?                      
         BE    FMT02               YES                                          
         MVC   OPHASE,SPACES                                                    
*                                                                               
         GOTO1 CHEXOUT,DMCB,PSNAME,OPHASE,L'PSNAME,0                            
         MVC   OPHASE(1),PSLANG    SET LANGUAGE                                 
*                                                                               
         CLI   PSLVL,C' '          SET TEST LEVEL (IF ANY)                      
         BNH   *+10                                                             
         MVC   OPHASE+6(1),PSLVL                                                
*                                                                               
FMT02    LA    RF,HOPHASE          SET ADDRESS OF PHASE                         
         ST    RF,FULL                                                          
         GOTO1 CHEXOUT,DMCB,FULL+1,OADDR,3,0                                    
*                                                                               
         EDIT  (B4,HPSLEN),(6,OLEN),0,ZERO=NOBLANK                              
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
EFFS     DC    16X'FF'                                                          
DELETED  DC    C'Deleted'                                                       
SPACES   DC    80C' '                                                           
*                                                                               
ADDR     DC    C'Buffer address='                                               
LEN      DC    C',Buffer length='                                               
AVAIL    DC    C',Available='                                                   
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
WORKD    DSECT                                                                  
IPARMS   DS    0XL32                                                            
ASYSFACS DS    A                   A(SYSTEM FACILITIES TABLE)                   
ATIA     DS    A                   A(TIA)                                       
AUTL     DS    A                   A(UTL ENTRY)                                 
ACOMFACS DS    A                   A(COMMON FACILITIES TABLE)                   
ASELIST  DS    A                   A(SELIST ENTRY)                              
ATWA     DS    A                   A(TWA)                                       
AMAP     DS    A                   A(PHASE MAP)                                 
ATIOB    DS    A                   A(TRANSLATOR INPUT OUTPUT BLOCK)             
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
*                                                                               
HALF     DS    H                                                                
HALF1    DS    H                                                                
*                                                                               
ANEXT    DS    A                                                                
WORK     DS    CL20                                                             
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* OUTPUT LINE FORMAT DSECT                                            *         
***********************************************************************         
OLINED   DSECT                                                                  
OADDR    DS    XL6                                                              
         DS    XL1                                                              
OPHASE   DS    XL7                                                              
         DS    XL1                                                              
OLEN     DS    XL6                                                              
         DS    XL2                                                              
OLINEL   EQU   *-OLINED                                                         
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
SRHOLFFD DSECT                                                                  
         DS    CL64                                                             
* SRHOLFFD                                                                      
       ++INCLUDE SRHOLFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* DDFH                                                                          
       ++INCLUDE DDFH                                                           
* FASYSFAC                                                                      
       ++INCLUDE FASYSFAC                                                       
* FAPROGSPCD                                                                    
       ++INCLUDE FAPROGSPCD                                                     
* FALANG                                                                        
       ++INCLUDE FALANG                                                         
* FASSB                                                                         
       ++INCLUDE FASSB                                                          
* FAHOLED                                                                       
       ++INCLUDE FAHOLED                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRHOL00   09/09/14'                                      
         END                                                                    
