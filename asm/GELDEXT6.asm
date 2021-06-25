*          DATA SET GELDEXT6   AT LEVEL 018 AS OF 01/27/00                      
*PHASE GELDEXT6,*                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'GELDEXT -GENDIR/FIL LOAD/DUMP CON/NFI RECORDS'                  
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
         NMOD1 (WORKX-WORKD),MPLDEXT                                            
         USING WORKD,RC                                                         
         EJECT                                                                  
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
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
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
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*        OPEN  (GENTAPE,OUTPUT)                                                 
*                                                                               
         B     DMXIT                                                            
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   L     R2,AREC                                                          
         USING FDRRECD,R2             FIELD RECORD DSECT                        
         CLC   =C'XA',FDRKMIN         ACTION RECORD?                            
         BE    ACCTEST                                                          
         CLC   =C'XD',FDRKMIN         DOWNLOAD RECORD?                          
         BE    ACCTEST                                                          
         CLC   =C'XF',FDRKMIN         FIELD RECORD?                             
         BE    ACCTEST                                                          
         CLC   =C'XP',FDRKMIN         PFKEY RECORD?                             
         BE    ACCTEST                                                          
         CLC   =C'XR',FDRKMIN         RECORD RECORD?                            
         BE    ACCTEST                                                          
         CLC   =C'XS',FDRKMIN         SCREEN RECORD?                            
         BE    ACCTEST                                                          
         CLC   =C'XX',FDRKMIN         DEFCLM RECORD?                            
         BE    ACCTEST                                                          
         B     DMXKEEP                                                          
         DROP  R2                                                               
*                                                                               
         USING FDRRECD,R2                                                       
ACCTEST  CLI   FDRKSYS,6                                                        
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
*NOP     CLOSE GENTAPE                                                          
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(12),=C'DELETED RECS'                                        
         EDIT  (P4,RECDEL),(8,P)                                                
         AP    RECDEL,RECDEL                                                    
         BNZ   *+10                                                             
         MVC   P(8),=8C'.'                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
GENTAPE  DCB   DDNAME=GENTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=4004,BUFNO=2                                      
RECDEL   DC    PL4'0'                                                           
SPLRECS  DC    PL4'0'                                                           
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
TEMPKEY  DS    CL32                                                             
IOL      DS    CL4                                                              
WORK     DS    CL128                                                            
WORKX    DS    0C                                                               
       ++INCLUDE GEGENSCR                                                       
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018GELDEXT6  01/27/00'                                      
         END                                                                    
         USING FR1RECD,R2                                                       
         USING FRARECD,TEMPKEY                                                  
ACTIONR  MVC   TEMPKEY,FR1KEY                                                   
         XC    FR1KEY,FR1KEY                                                    
         MVI   FR1KMIN,FR1KMINQ                                                 
         MVI   FR1KTYP,FR1KTYPQ                                                 
         MVC   FR1KSYS,FRAKSYS                                                  
         MVC   FR1KPRG,FRAKPRG                                                  
         MVC   FR1KREC,FRAKREC                                                  
         MVC   FR1KACT,FRAKACT                                                  
         MVC   FR1KCTRY,FRAKCTRY                                                
         MVC   FR1KSUB,FRAKSUB                                                  
         B     DMXKEEP                                                          
*                                                                               
                                                                                
         USING FDLRECD,TEMPKEY                                                  
DLOADR   B     DMXKEEP                                                          
*                                                                               
         USING FD2RECD,R2                                                       
         USING FDRRECD,TEMPKEY                                                  
FIELDR   MVC   TEMPKEY,FD2KEY                                                   
         XC    FD2KEY,FD2KEY                                                    
         MVI   FD2KMIN,FD2KMINQ                                                 
         MVI   FD2KTYP,FD2KTYPQ                                                 
         MVC   FD2KSYS,FDRKSYS                                                  
         MVC   FD2KPRG,FDRKPRG                                                  
         MVC   FD2KREC,FDRKREC                                                  
         MVC   FD2KNUM,FDRKNUM                                                  
         MVI   FD2KCTRY,X'FF'                                                   
         MVI   FD2KSUB,X'FF'                                                    
         B     DMXKEEP                                                          
*                                                                               
         USING FR2RECD,R2                                                       
         USING FRPRECD,TEMPKEY                                                  
PFKEYR   MVC   TEMPKEY,FR2KEY                                                   
         XC    FR2KEY,FR2KEY                                                    
         MVI   FR2KMIN,FR2KMINQ                                                 
         MVI   FR2KTYP,FR2KTYPQ                                                 
         MVC   FR2KSYS,FRPKSYS                                                  
         MVC   FR2KPRG,FRPKPRG                                                  
         MVC   FR2KREC,FRPKREC                                                  
         MVC   FR2KACT,FRPKACT                                                  
         MVC   FR2KPFK,FRPKPFK                                                  
         MVC   FR2KCTRY,FRPKCTRY                                                
         MVC   FR2KSUB,FRPKSUB                                                  
         B     DMXKEEP                                                          
*                                                                               
         USING FR3RECD,R2                                                       
         USING FRRRECD,TEMPKEY                                                  
RECORDR  MVC   TEMPKEY,FR3KEY                                                   
         XC    FR3KEY,FR3KEY                                                    
         MVI   FR3KMIN,FR3KMINQ                                                 
         MVI   FR3KTYP,FR3KTYPQ                                                 
         MVC   FR3KSYS,FRRKSYS                                                  
         MVC   FR3KPRG,FRRKPRG                                                  
         MVC   FR3KREC,FRRKREC                                                  
         MVC   FR3KCTRY,FRRKCTRY                                                
         MVC   FR3KSUB,FRRKSUB                                                  
         B     DMXKEEP                                                          
*                                                                               
         USING FS1RECD,R2                                                       
         USING FSRRECD,TEMPKEY                                                  
SCREENR  MVC   TEMPKEY,FS1KEY                                                   
         XC    FS1KEY,FS1KEY                                                    
         MVI   FS1KMIN,FS1KMINQ                                                 
         MVI   FS1KTYP,FS1KTYPQ                                                 
         MVC   FS1KSYS,FSRKSYS                                                  
         MVC   FS1KPRG,FSRKPRG                                                  
         MVC   FS1KREC,FSRKREC                                                  
         MVC   FS1KCODE,FSRKCODE                                                
         MVC   FS1KPAGE,FSRKPAGE                                                
         MVC   FS1KCTRY,FSRKCTRY                                                
         MVI   FS1KSUB,X'FF'                                                    
         B     DMXKEEP                                                          
*                                                                               
         USING FCRRECD,R2                                                       
DEFCLMR  B     DMXKEEP                                                          
