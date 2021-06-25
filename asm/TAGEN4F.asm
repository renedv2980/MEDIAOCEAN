*          DATA SET TAGEN4F    AT LEVEL 073 AS OF 10/10/14                      
*PHASE T7024FA,*                                                                
         TITLE 'T7024F - GUARANTEE CODE CHANGE'                                 
T7024F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T7024F                                                    
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         ST    RE,ASVPTRS          SAVE A(SAVED POINTER BLOCK)                  
         AHI   RE,L'SVPTRBLK                                                    
         ST    RE,AUPPTRS          SAVE A(UPDATED POINTER BLOCK)                
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
         MVC   SGUSHED(9),=C'Pid Num  '                                         
         OI    SGUSHEDH+6,X'80'                                                 
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   GU30                                                             
*                                                    IF FLD CHGD RE-VAL         
         GOTO1 FLDVAL,DMCB,(X'40',SGUSSNH),(X'80',SGUNCODH)                     
         BNE   *+12                                                             
         CLI   STATUS,STOK         IF WENT THROUGH VALKEY                       
         BE    GU20                SIMPLY LOOK FOR PFKEY                        
         MVI   STATUS,0                                                         
         LA    R2,SGUSSNH          VALIDATE SS NUMBER                           
         GOTO1 ANY                                                              
*                                                                               
         CLI   SGUSSNH+5,0                                                      
         BE    MISSERR                                                          
         CLI   SGUSSNH+5,9         SSN ENTERED?                                 
         BE    GU02                                                             
         CLI   SGUSSNH+5,6         PID ENTERED?                                 
         BNE   INVERR                                                           
         MVC   TGPID,SGUSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   GU02                                                             
         MVC   SGUSSN,TGSSN                                                     
         MVI   SGUSSNH+5,9                                                      
         B     GU03                                                             
*                                                                               
GU02     MVC   WORK(9),=9X'F0'     INSURE VALID NUMERIC                         
         MVZ   WORK(9),8(R2)                                                    
         CLC   WORK(9),=9X'F0'                                                  
         BNE   INVERR                                                           
*                                                                               
GU03     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SGUSSNMH                        
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SGUSSN,SPACES                                                    
         MVC   SGUSSN(L'TGPID),TGPID                                            
         MVI   SGUSSNH+5,L'TGPID                                                
         OI    SGUSSNH+6,X'80'                                                  
*                                                                               
         LA    R2,SGUCCODH         CURRENT GUARANTEE CODE                       
         GOTO1 ANY                                                              
         MVC   OLDCODE,SGUCCOD                                                  
         MVC   OLDCODC,SGUCCOD                                                  
         XC    OLDCODC,=X'FFFFFFFF'                                             
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'A4',OLDCODC)                              
         BNE   INVERR                                                           
         OI    4(R2),X'20'                                                      
*                                                                               
         L     R4,AIO              GET GUARANTEE DETAILS ELEMENT                
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGUD,R4                                                         
         MVC   TGAGY,TAGUAGY       SAVE AGENCY                                  
*                                                                               
         LA    R2,SGUNCODH         NEW GUARANTEE CODE                           
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLI   8(R2),C'X'                                                       
         BNE   GU05                                                             
         XC    NEWCODE,NEWCODE                                                  
         XC    NEWCODC,NEWCODC                                                  
         B     GU10                                                             
*                                                                               
GU05     MVC   NEWCODE,SGUNCOD                                                  
         MVC   NEWCODC,SGUNCOD                                                  
         XC    NEWCODC,=X'FFFFFFFF'                                             
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'A4',NEWCODC)                              
         BNE   INVERR              NO GUARANTEE RECORD                          
*                                                                               
         L     R4,AIO              GET GUARANTEE DETAILS ELEMENT                
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAGUD,R4                                                         
*                                                                               
         TM    TAGUSTAT,TAGUSLCK   IF GUARANTEE IS LOCKED                       
         BO    GUARERR             CANNOT ATTACH TO CAST                        
*                                                                               
         OC    TGAGY,TGAGY         IF AGENCIES ARE DEFINED                      
         BZ    GU10                                                             
         OC    TAGUAGY,TAGUAGY                                                  
         BZ    GU10                                                             
         CLC   TGAGY,TAGUAGY       THEY SHOULD MATCH                            
         BNE   AGYERR                                                           
*                                                                               
GU10     CLC   NEWCODE,OLDCODE                                                  
         BE    INVERR              CANNOT HAVE SAME CODES                       
         OI    4(R2),X'20'                                                      
         OI    TRNSTAT,OKINTPFK                                                 
         OI    STATUS,STOK                                                      
         B     INFMSG                                                           
*                                                                               
GU20     CLI   PFAID,13            THEN                                         
         BNE   INFMSG              CONTINUE GIVING MESSAGE UNTIL                
         B     GUX                 HIT PF13                                     
*                                                                               
GU30     CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   GUX                                                              
         BAS   RE,GETCAST                                                       
*                                                                               
GUX      B     XIT                                                              
         SPACE 2                                                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE NEW GUARANTEE CODE                                            
*                                                                               
*        GET ALL CAST RECORDS AND CHANGE GUARANTEE CODE                         
*                                                                               
GETCAST  NTR1                                                                   
         USING TLCAPD,R3                                                        
         LA    R3,KEY                                                           
GCAST10  XC    KEY,KEY                                                          
         MVI   TLCAPCD,TLCAGCDQ   READ ALL CAST RECORDS ATTACHED                
         MVC   TLCAGSSN,TGSSN     TO THE GUARANTEE THAT IS BEING                
         MVC   TLCAGGUA,SGUCCOD   CHANGED                                       
         GOTO1 HIGH                                                             
         CLC   KEY(TLCAGCOM-TLCAPD),KEYSAVE                                     
         BNE   XIT                                                              
                                                                                
         OI    GENSTAT1,CATCHIOR                                                
         GOTO1 CATCHIOS           IF TOO MANY IO'S                              
         CLI   ERROR,NOTONLIN        GIVE USER MSG                              
         BNE   GCAST20                                                          
         MVI   MYMSGNO1,230                                                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SGUCCODH                                                      
         B     ERRXIT                                                           
                                                                                
GCAST20  MVC   SVCOM,TLCAGCOM                                                   
         DROP  R3                                                               
                                                                                
         MVI   RDUPDATE,C'Y'      GET CAST RECORD                               
         GOTO1 GETREC             AND SAVE INITIAL PASSIVES                     
         GOTO1 SAVPTRS,DMCB,ASVPTRS                                             
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TACAGUA,NEWCODE     UPDATE RECORD AND KEYS WITH                  
         GOTO1 ACTVIN,DMCB,0       NEW GUARANTEE CODE                           
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,ASVPTRS                                             
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCAPCD,TLCAHCDQ    READ CAST HOLDING FEE PASSIVES               
         MVC   TLCAHCOM,SVCOM      FOR THE COMM'L                               
         GOTO1 HIGH                                                             
GCAST40  GOTO1 SEQ                                                              
         CLC   KEY(TLCAHSRT-TLCAPD),KEYSAVE                                     
         BNE   GCAST10                                                          
         OC    TLCAHNXT,TLCAHNXT   IF ANY CAST HAS RECEIVED A                   
         BZ    GCAST40             HOLDING FEE NOTICE                           
         CLC   TLCAHDTE,TLCAHNXT   THAT HAS NOT BEEN PAID ...                   
         BH    GCAST40                                                          
         DROP  R3                                                               
                                                                                
         USING TLCOPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVCOM                                                   
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         BE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      ... TURN ON COMMERCIAL'S                     
         BAS   RE,GETEL            REISSUE REQUIRED STATUS                      
         BE    *+6                                                              
         DC    H'00'                                                            
         TM    TACOSTA2,TACOCHHF                                                
         BO    GCAST10                                                          
         OI    TACOSTA2,TACOCHHF                                                
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         GOTOR SNDMQHFR,DMCB,(PRGSTAT,SVCOM),(TGSYSTA2,HEXOUT),MQIO             
         B     GCAST10                                                          
         EJECT                                                                  
*                                                                               
MISSERR  MVI   ERROR,MISSING       MISSING INPUT                                
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT                                
         B     ERRXIT                                                           
*                                                                               
AGYERR   MVI   ERROR,ERGUAAGY      AGENCIES ON GUAR RECS DO NOT MATCH           
         B     ERRXIT                                                           
*                                                                               
GUARERR  MVI   ERROR,ERGRTLCK      GUARANTEE RECORD IS LOCKED                   
         B     ERRXIT                                                           
*                                                                               
INFMSG   MVI   MYMSGNO1,35         PRESS PF13 TO MAKE CHANGES                   
         LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
*              CONSTANTS, ETC.                                                  
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAMQHFR                                                        
       ++INCLUDE TAGENFFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR4FD                                                       
         SPACE 4                                                                
*                                                                               
         ORG   SGUWORK                                                          
*                                                                               
OLDCODC  DS    CL4                 OLD GUARANTEE CODE - COMPLEMENTED            
NEWCODC  DS    CL4                 NEW GUARANTEE CODE - COMPLEMENTED            
OLDCODE  DS    CL4                 OLD GUARANTEE CODE                           
NEWCODE  DS    CL4                 NEW GUARANTEE CODE                           
STATUS   DS    XL1                 STATUS                                       
STOK     EQU   X'80'                                                            
SVCOM    DS    XL(L'TGCOM)         SAVED INTERNAL COMMERCIAL NUMBER             
*                                                                               
ASVPTRS  DS    A                   A(SAVED POINTER BLOCK)                       
AUPPTRS  DS    A                   A(UPDATED POINTER BLOCK)                     
         EJECT                                                                  
         SPACE 2                                                                
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
         PRINT ON                                                               
TMPD     DSECT                                                                  
SVPTRBLK DS    CL((15*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS                       
UPPTRBLK DS    CL((15*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS FOR ADDPTRS           
TMPLNQ   EQU   *-TMPD                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073TAGEN4F   10/10/14'                                      
         END                                                                    
