*          DATA SET SPLDEXTSC  AT LEVEL 064 AS OF 10/01/99                      
*PHASE SPEXTSC                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXTSC - FIX BAD BUY RECORDS'                                
*                                                                               
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         L     R8,12(R1)           DMLDDEFN                                     
         USING LDDEFND,R8                                                       
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  DS    0H                                                               
         L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         XC    COUNT,COUNT                                                      
         L     RF,=V(CLUNPK)                                                    
         ST    RF,VCLUNPK                                                       
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R5,AREC             THIS IS FOR RECUP CALLS                      
         SR    R0,R0               MAKE SURE 2 BYTES BEYOND REC'S 00            
         ICM   R0,3,13(R5)                                                      
         AR    R5,R0                                                            
         XC    0(2,R5),0(R5)                                                    
*                                                                               
         L     R6,AREC                                                          
         USING BUYRECD,R6                                                       
         CLI   BUYKAM,X'11'        SPOT BUY?                                    
         BL    DMXKEEP             NO                                           
*                                                                               
         XC    THREBYTE,THREBYTE                                                
         MVI   FLAG99,0                                                         
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'99'                                                     
*                                                                               
DMXREC1  BAS   RE,NEXTEL           NO DUPLICATE 66, 99'S, KEEP REC              
         BNE   DMXKEEP                                                          
*                                                                               
         CLI   0(R6),X'66'                                                      
         BNE   DMXREC2                                                          
         CLC   THREBYTE+2(1),2(R6) SAME COMMENT NUMBER                          
         BE    XR1                 WE HAVE DUPLICATE, PROCESS                   
         MVC   THREBYTE,0(R6)                                                   
         B     DMXREC1                                                          
*                                                                               
DMXREC2  CLI   0(R6),X'99'                                                      
         BNE   DMXREC1                                                          
         CLI   FLAG99,1            HAVE WE SEEN ONE BEOFRE?                     
         BE    XR1                 WE HAVE DUPLICATE, PROCESS                   
         MVI   FLAG99,1                                                         
         B     DMXREC1                                                          
**********************************************************************          
*        THE GOAL HERE IS TO MAKE ONE COPY OF EACH DISTINGUISHED     *          
*        ELEMENT, THEN DELETE ALL X'66' AND X'99' ELEMENTS ON THE    *          
*        BAD RECORD.  THEN USE RECUP TO INSERT ONE COPY FOR EACH.    *          
**********************************************************************          
*                                                                               
XR1      L     R6,AREC                                                          
         USING BUYRECD,R6                                                       
         MVC   P+2(3),=C'KEY'                                                   
         GOTO1 LHEXOUT,DMCB,BUYKEY,P+25,13,=C'TOG'                              
         GOTO1 LPRINTER                                                         
         MVC   P+2(10),=C'OLD LENGTH'                                           
         EDIT  BUYRLEN,(4,P+25)                                                 
         GOTO1 LPRINTER                                                         
*                                                                               
         MVI   FLAG99,0                                                         
         XC    SVCOM1,SVCOM1       THE STORAGE PLACES FOR COMMENT ELEM          
         XC    SVCOM2,SVCOM2                                                    
         XC    SVCOM3,SVCOM3                                                    
         XC    SVCOM4,SVCOM4                                                    
         XC    SVCOM5,SVCOM5                                                    
         XC    SVACTEL,SVACTEL     ACTIVITY ELEMENT                             
         XC    SVDATE1,SVDATE1     ACTIVITY DATE                                
         LA    R6,BDELEM                                                        
*                                                                               
XR2NXT   BAS   RE,NEXTEL                                                        
         BE    XR2                                                              
         B     XR10                                                             
*                                                                               
XR2NXT2  BAS   RE,NEXTEL2                                                       
         BNE   XR10                                                             
*                                                                               
XR2      CLI   0(R6),X'66'         COMMENT ELEMENT                              
         BE    XR5                                                              
         CLI   0(R6),X'99'                                                      
         BNE   XR2NXT                                                           
*                                                                               
         USING ACTVELEM,R6                                                      
         CLI   FLAG99,1            HAS THERE BEEN A 99 BEFORE?                  
         BE    XR3                                                              
         MVI   FLAG99,1                                                         
         B     XR3A                                                             
XR3      CLC   ACTVCHG+2(3),SVDATE1                                             
         BNH   XR4                 I GUESS WE KEEP THE MORE RECENT ONE          
XR3A     MVC   SVDATE1,ACTVCHG+2                                                
         MVC   SVACTEL,ACTVELEM                                                 
         MVC   P+2(7),=C'SVACTEL'                                               
         MVC   P+20(12),SVACTEL                                                 
         GOTO1 LPRINTER                                                         
*                                                                               
XR4      GOTO1 =V(RECUP),DMCB,(C'S',AREC),(R6),0                                
         MVC   P+2(6),=C'DELACT'                                                
         GOTO1 LPRINTER                                                         
         B     XR2NXT2             LOOK FOR MORE                                
         DROP  R6                                                               
*                                                                               
         USING COMELEM,R6                                                       
XR5      CLI   CMNUM,1                                                          
         BNE   XR5B                                                             
         OC    SVCOM1,SVCOM1                                                    
         BNZ   XR5DEL                                                           
         MVC   SVCOM1,0(R6)                                                     
XR5DEL   GOTO1 =V(RECUP),DMCB,(C'S',AREC),(R6)         DELETE ELEMENT           
         MVC   P+2(5),=C'DEL66'                                                 
         GOTO1 LPRINTER                                                         
         B     XR2NXT2                                                          
*                                                                               
XR5B     CLI   CMNUM,2                                                          
         BNE   XR5C                                                             
         OC    SVCOM2,SVCOM2                                                    
         BNZ   XR5DEL                                                           
         MVC   SVCOM2,COMELEM                                                   
         B     XR5DEL                                                           
*                                                                               
XR5C     CLI   CMNUM,3                                                          
         BNE   XR5D                                                             
         OC    SVCOM3,SVCOM3                                                    
         BNZ   XR5DEL                                                           
         MVC   SVCOM3,COMELEM                                                   
         B     XR5DEL                                                           
*                                                                               
XR5D     CLI   CMNUM,4                                                          
         BNE   XR5E                                                             
         OC    SVCOM4,SVCOM4                                                    
         BNZ   XR5DEL                                                           
         MVC   SVCOM4,COMELEM                                                   
         B     XR5DEL                                                           
*                                                                               
XR5E     CLI   CMNUM,5                                                          
         BNE   XR5DEL              DELETE ELEMENT IF IT'S NOT 1-5               
         OC    SVCOM5,SVCOM5                                                    
         BNZ   XR5DEL                                                           
         MVC   SVCOM5,COMELEM                                                   
         B     XR5DEL                                                           
*                                                                               
XR10     ST    R6,SVEOR                                                         
         XC    ELEM,ELEM           R6 POINTS TO END OF REC                      
         CLI   FLAG99,1                                                         
         BNE   XR10A                                                            
         MVC   P+2(6),=C'ADDACT'                                                
         GOTO1 LPRINTER                                                         
         GOTO1 =V(RECUP),DMCB,(C'S',AREC),SVACTEL,(R6)                          
*                                                                               
XR10A    LA    R4,SVCOM5                                                        
         BAS   R9,ADDCOM                                                        
*                                                                               
         LA    R4,SVCOM4                                                        
         BAS   R9,ADDCOM                                                        
*                                                                               
         LA    R4,SVCOM3                                                        
         BAS   R9,ADDCOM                                                        
*                                                                               
         LA    R4,SVCOM2                                                        
         BAS   R9,ADDCOM                                                        
*                                                                               
         LA    R4,SVCOM1                                                        
         BAS   R9,ADDCOM                                                        
         B     XR15                                                             
*                                                                               
ADDCOM   OC    0(3,R4),0(R4)       TEST FOR COMMENT                             
         BZR   R9                                                               
         GOTO1 =V(RECUP),DMCB,(C'S',AREC),(R4),(R6)                             
         BR    R9                                                               
*                                                                               
XR15     L     R1,COUNT                                                         
         AHI   R1,1                                                             
         ST    R1,COUNT                                                         
*                                                                               
         L     R6,AREC                                                          
         USING BUYRECD,R6                                                       
         MVC   P+2(10),=C'NEW LENGTH'                                           
         EDIT  BUYRLEN,(4,P+25)                                                 
         GOTO1 LPRINTER                                                         
***                                                                             
* THIS BE A BADDIE                                                              
         L     R6,AREC                                                          
         MVC   P(2),BUYALPHA       AGENCY POWER CODE                            
         MVI   P+2,C'/'                                                         
*                                                                               
         MVC   BYTE,BUYKAM         MEDIA                                        
         NI    BYTE,X'0F'                                                       
         MVI   P+3,C'T'                                                         
         CLI   BYTE,X'01'                                                       
         BE    DMXR10                                                           
         MVI   P+3,C'R'                                                         
         CLI   BYTE,X'02'                                                       
         BE    DMXR10                                                           
         MVI   P+3,C'N'                                                         
         CLI   BYTE,X'03'                                                       
         BE    DMXR10                                                           
         MVI   P+3,C'X'                                                         
         CLI   BYTE,X'04'                                                       
         BE    DMXR10                                                           
         MVI   P+3,C'?'                                                         
*                                                                               
DMXR10   DS    0H                                                               
         GOTO1 VCLUNPK,DMCB,BUYKCLT,P+5                                         
         GOTO1 LHEXOUT,DMCB,BUYKPRD,P+9,1,=C'TOG'                               
         EDIT  (B1,BUYKEST),(3,P+12),FILL=0                                     
         EDIT  (B1,BUYKBUY),(3,P+30),FILL=0                                     
*                                                                               
         GOTO1 LHEXOUT,DMCB,BUYMSTA,P+60,5,=C'TOG'                              
         GOTO1 LPRINTER                                                         
         CLC   COUNT,=F'10'                                                     
         BH    DMXKEEP                                                          
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 =V(PRNTBL),DMCB,C'BADREC',AREC,C'DUMP',(R0),=C'1D00',   X        
               (C'P',LPRINT)                                                    
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
VCLUNPK  DS    A                                                                
SVEOR    DS    F                                                                
COUNT    DS    H                   NUMBER OF CLIENT RECORDS CHANGED             
BYTE     DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
THREBYTE DS    XL3                                                              
SVDATE1  DS    CL3                                                              
SVACTEL  DS    XL12                                                             
FLAG99   DS    X                                                                
         DS    0D                                                               
         DC    CL8'SVCOM1'                                                      
SVCOM1   DS    XL80                                                             
         DC    CL8'SVCOM2'                                                      
SVCOM2   DS    XL80                                                             
         DC    CL8'SVCOM3'                                                      
SVCOM3   DS    XL80                                                             
         DC    CL8'SVCOM4'                                                      
SVCOM4   DS    XL80                                                             
         DC    CL8'SVCOM5'                                                      
SVCOM5   DS    XL80                                                             
ELEM     DS    XL100                                                            
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VMSUNPK  DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064SPLDEXTSC 10/01/99'                                      
         END                                                                    
