*          DATA SET TAGEN3F    AT LEVEL 068 AS OF 03/17/15                      
*PHASE T7023FD,*                                                                
         TITLE 'T7023F - SS NUMBER CHANGE'                                      
T7023F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7023F                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
***********************************************************************         
*              MODE CONTROLLED ROUTINES                               *         
***********************************************************************         
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
         TM    TGSYSTAT,TASYSPID                                                
         BZ    SS05                                                             
         MVC   SSSLCSS(11),=C'Current Pid'                                      
         OI    SSSLCSSH+6,X'80'                                                 
         MVC   SSSLNSS(7),=C'New Pid'                                           
         OI    SSSLNSSH+6,X'80'                                                 
*                                                                               
SS05     LA    R2,SSSSSNH          READ OLD SSN AND GET NAME                    
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    SS10                                                             
         CLI   SSSSSNH+5,6                                                      
         BH    SS10                                                             
         MVC   TGPID,SSSSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         MVC   SSSSSN,TGSSN                                                     
         MVI   SSSSSNH+5,9                                                      
SS10     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'38',0(R2)),SSSNAMEH                       
*                                                                               
         OC    SSSSSN,SPACES       FILL BOTH NUMBERS WITH BLANKS                
         OC    SSSNSSN,SPACES                                                   
*                                                                               
         CLC   SSSSSN,SSSNSSN      OLD EQUAL NEW?                               
         BNE   SS20                NO                                           
         LA    R2,SSSNSSNH         YES, ERROR                                   
         B     INVERR                                                           
*                                                                               
SS20     CLI   MODE,VALREC                                                      
         BNE   SSX                                                              
         BAS   RE,GETCAST          UPDATE RECORDS WITH NEW NUMBER               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    SSX                                                              
         CLI   SSSSSNH+5,6                                                      
         BNH   SS40                                                             
         MVC   TGSSN,SSSSSN                                                     
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SSSSSN,SPACES                                                    
         MVC   SSSSSN(L'TGPID),TGPID                                            
         MVI   SSSSSNH+5,6                                                      
         OI    SSSSSNH+6,X'80'                                                  
*                                                                               
SS40     CLI   SSSNSSNH+5,6                                                     
         BNH   SSX                                                              
         MVC   TGSSN,SSSNSSN                                                    
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SSSNSSN,SPACES                                                   
         MVC   SSSNSSN(L'TGPID),TGPID                                           
         MVI   SSSNSSNH+5,6                                                     
         OI    SSSNSSNH+6,X'80'                                                 
SSX      B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE NEW SSN                                   *         
***********************************************************************         
*                                                                               
GETCAST  NTR1                                                                   
         LA    R2,SSSNSSNH         MAKE SURE THEY ENTERED A NEW SSN             
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    GETC01                                                           
         CLI   SSSNSSNH+5,6                                                     
         BH    GETC01                                                           
         MVC   TGPID,SSSNSSN                                                    
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   GETC01                                                           
         MVC   SSSNSSN,TGSSN                                                    
         MVI   SSSNSSNH+5,9                                                     
GETC01   GOTO1 RECVAL,DMCB,TLW4CDQ,0(R2)                                        
*                                                                               
         OC    NXTKEY,NXTKEY       IN MIDDLE OF READ?                           
         BZ    GETC02              NO                                           
         CLI   NXTKEY,TLECCCDQ     YES, READING ECAST RECORD?                   
         BE    GETC04              YES                                          
         CLI   NXTKEY,TLW4CCDQ     NO, READING W4 RECORD?                       
         BE    GETC06              YES                                          
*                                                                               
GETC02   MVI   RECCD,TLCACCDQ      START/CONTINUE WITH CAST BY SSN              
         BAS   RE,CHGCAST                                                       
*                                                                               
GETC04   MVI   RECCD,TLECCCDQ      START/CONTINUE WITH ECAST BY SSN             
         BAS   RE,CHGCAST                                                       
*                                                                               
GETC06   MVI   RECCD,TLW4CCDQ      START/CONTINUE WITH W4 BY SSN                
         BAS   RE,CHGCAST                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              CHANGE CAST, ECAST AND W4 RECORDS                      *         
***********************************************************************         
*                                                                               
CHGCAST  NTR1                                                                   
         LA    R2,KEY              BUILD KEY FOR READ                           
         USING TLDRD,R2                                                         
         MVC   KEY,NXTKEY          MOVE IN LAST KEY READ                        
         OC    KEY,KEY             ARE WE IN MIDDLE OF READING?                 
         BNZ   CHGC06              YES, CONTINUING THEN                         
*                                                                               
         MVC   TLDRCD,RECCD        NO, BUILD KEY FOR READ                       
         CLI   RECCD,TLCACCDQ      SET SSN BASED ON RECORD TYPE                 
         BNE   CHGC02                                                           
         MVC   KEY+TLCACSSN-TLCAPCD(L'SSSSSN),SSSSSN                            
         B     CHGC06                                                           
*                                                                               
CHGC02   CLI   RECCD,TLECCCDQ                                                   
         BNE   CHGC04                                                           
         MVC   KEY+TLECCSSN-TLECPD(L'SSSSSN),SSSSSN                             
         B     CHGC06                                                           
*                                                                               
CHGC04   MVC   KEY+TLW4CCRP-TLW4PD(L'SSSSSN),SSSSSN                             
*                                                                               
CHGC06   GOTO1 HIGH                                                             
         B     CHGC10                                                           
*                                                                               
CHGC08   GOTO1 SEQ                                                              
*                                                                               
CHGC10   LA    RE,TLCACCOM-TLCAPD-1   SET LENGTH BASED ON RECORD TYPE           
         CLI   RECCD,TLCACCDQ                                                   
         BE    CHGC12                                                           
         LA    RE,TLECCEPI-TLECPD-1                                             
         CLI   RECCD,TLECCCDQ                                                   
         BE    CHGC12                                                           
         LA    RE,TLW4CSSN-TLW4PD-1                                             
*                                                                               
CHGC12   EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE      SAME KEY?                                    
         BNE   CHGCX               NO                                           
         MVC   NXTKEY,KEY          YES, SAVE FOR NEXT TIME IN                   
*                                                                               
         GOTO1 GETFACT,DMCB,0      SPLIT REQ INTO CHUNKS OF 2000 I/O            
         L     R1,DMCB             THIS FIXES I/O AND LOCKER PROBLEMS           
         USING FACTSD,R1                                                        
         LH    R3,=H'2000'                                                      
         CLM   R3,3,FATIOCNT                                                    
         BH    CHGC14                                                           
*                                                                               
         MVI   MYMSGNO1,77         PUT MSG - HIT ENTER TO CONTINUE              
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SSSSSNH                                                       
         B     ERRXIT                                                           
         DROP  R1                                                               
*                                                                               
CHGC14   MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
*                                                                               
         CLI   RECCD,TLCACCDQ      ARE WE READING CAST RECORD?                  
         BNE   CHGC18              NO                                           
*                                                                               
         L     R1,AIO              YES, UPDATE KEY WITH NEW SSN                 
         USING TLCAD,R1                                                         
         MVC   TLCASSN,SSSNSSN                                                  
         OC    TLCASEQ,TLCASEQ                                                  
         JNZ   CHGC16                                                           
         MVC   TLCASEQ,=X'0001'                                                 
*                                                                               
CHGC16   GOTO1 ACTVIN,DMCB,0       UPDATE ACTIVITY & ADD POINTER                
         GOTO1 ADDPTRS,DMCB,(X'80',PTRBLK)                                      
*                                                                               
         USING TACAD,R4                                                         
         L     R4,AIO              IF RECORD HAS GUARANTEE CODE ...             
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   CHGC17                                                           
         OC    TACAGUA,TACAGUA                                                  
         BZ    CHGC17                                                           
         DROP  R4                                                               
                                                                                
         L     R4,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLCAKEY),0(R4)                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      CLEAR OUT GUARANTEE CODE                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         XC    TACAGUA,TACAGUA                                                  
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'80',PTRBLK)                                      
         DROP  R4                                                               
                                                                                
CHGC17   MVC   KEY,NXTKEY          RESTORE READ SEQUENCE                        
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         B     CHGC08              GET NEXT RECORD                              
         DROP  R1                                                               
*                                                                               
CHGC18   CLI   RECCD,TLECCCDQ      ARE WE READING ECAST RECORDS?                
         BNE   CHGC20              NO                                           
*                                                                               
         L     R1,AIO              YES, UPDATE KEY WITH NEW SSN                 
         USING TLECD,R1                                                         
         MVC   TLECSSN,SSSNSSN                                                  
         B     CHGC16              UPDATE ACTIVITY AND ADD POINTER              
*                                                                               
CHGC20   L     R4,AIO              MUST BE W4 RECORD THEN                       
         USING TATID,R4                                                         
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   CHGC26                                                           
*                                                                               
CHGC22   CLI   TATITYPE,TATITYCO          IS THIS A CORP NUMBER?                
         BNE   CHGC24                     NO                                    
         CLC   TATIID(L'SSSSSN),SSSSSN    YES, IS THIS THE CORRECT ID?          
         BNE   CHGC24                     NO                                    
         MVC   TATIID(L'SSSNSSN),SSSNSSN  YES, SAVE NEW SSN                     
         OC    TATIID,SPACES              FILL WITH BLANKS                      
         B     CHGC26                                                           
*                                                                               
CHGC24   BAS   RE,NEXTEL                                                        
         BE    CHGC22                                                           
*                                                                               
CHGC26   GOTO1 ACTVIN,DMCB,0         UPDATE ACTIVITY ELEMENT                    
         GOTO1 PUTREC                REWRITE THE RECORD                         
         GOTO1 ADDPTRS,DMCB,PTRBLK   ADD POINTERS                               
         B     CHGC08              GET NEXT RECORD                              
         DROP  R4                                                               
*                                                                               
CHGCX    XC    NXTKEY,NXTKEY        CLEAR FOR NEXT TIME IN                      
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
MISSERR  MVI   ERROR,MISSING       MISSING INPUT                                
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT                                
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR3FD                                                       
         ORG   SSSWORK                                                          
*                                                                               
RECCD    DS    CL1                 CURRENT RECORD CODE                          
NXTKEY   DS    CL48                NEXT TIME IN READ THIS KEY                   
*                                                                               
PTRBLK   DS    CL((2*L'TLDRREC)+1)                                              
         EJECT                                                                  
* TAGENWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
* TASYSEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TASYSEQUS                                                      
         PRINT ON                                                               
* TASYSDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* TAGENEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENEQUS                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068TAGEN3F   03/17/15'                                      
         END                                                                    
