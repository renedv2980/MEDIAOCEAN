*          DATA SET TAGEN2E    AT LEVEL 065 AS OF 02/20/09                      
*PHASE T7022EA,*                                                                
         TITLE 'T7022E - CORP CHANGE'                                           
T7022E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7022E                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,PFTABLE                                             
         TM    TGSYSTAT,TASYSPID                                                
         BZ    CC05                                                             
         MVC   SCCSHED(13),=C'Performer Pid'                                    
         OI    SCCSHEDH+6,X'80'                                                 
*                                                                               
CC05     LA    R2,SCCSSNH                                                       
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    CC07                                                             
         CLI   SCCSSNH+5,0                                                      
         BE    MISSERR                                                          
         CLI   SCCSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    CC07                RECVAL CALL DOES NOT CHECK FOR               
         CLI   SCCSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   INVERR                                                           
         MVC   TGPID,SCCSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   CC07                                                             
         MVC   SCCSSN,TGSSN                                                     
         MVI   SCCSSNH+5,9                                                      
*                                                                               
CC07     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'28',0(R2)),SCCNAMEH                       
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    CC08                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SCCSSN,SPACES                                                    
         MVC   SCCSSN(L'TGPID),TGPID                                            
         MVI   SCCSSNH+5,6                                                      
         OI    SCCSSNH+6,X'80'                                                  
*                                                                               
CC08     L     R4,AIO              ENSURE TYPE = 'I' OR 'E'                     
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TAW4TYPE,TAW4TYIN                                                
         BE    CC10                                                             
         CLI   TAW4TYPE,TAW4TYCA                                                
         BE    CC10                                                             
         CLI   TAW4TYPE,TAW4TYES                                                
         BNE   INVERR                                                           
*                                                                               
CC10     CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   CCX                                                              
         BAS   RE,GETCAST                                                       
*                                                                               
CCX      B     XIT                                                              
         SPACE 2                                                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE NEW CORP CODE                                                 
*                                                                               
*        GET ALL CAST RECORDS AND CHANGE CORP CODE                              
*                                                                               
GETCAST  NTR1                                                                   
*                                                                               
         LA    R2,SCCCCODH         INPUT FOR OLD CORP                           
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVI   OLDCODE,0                                                        
         CLC   =C'NO',8(R2)                                                     
         BE    GET10               FROM NO CORP TO NEW CORP                     
         MVI   OLDCODE,C'A'                                                     
         CLC   =C'ALL',8(R2)                                                    
         BE    GET10               LEAVE OLD CORP NUMBER = 0                    
         CLI   5(R2),1             ONLY ALLOW 1 CHARACTER INPUT                 
         BNE   INVERR                                                           
         GOTO1 VALINUM                                                          
         MVC   OLDCODE,8(R2)                                                    
*                                                                               
GET10    LA    R2,SCCNCODH         ENSURE INPUT FOR NEW CORP CODE               
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLI   8(R2),C'X'          IF ERASE CURRENT CORP NUMBER                 
         BNE   GET20                                                            
         MVI   NEWCODE,0           INSERT A 0 INSTEAD OF CORP NUMBER            
         B     GET30                                                            
*                                                                               
GET20    GOTO1 VALINUM             ELSE ENSURE IT'S NUMERIC                     
         L     R4,AIO              AND A VALID CORP CODE                        
         USING TATID,R4                                                         
         MVI   ELCODE,TATIELQ                                                   
         MVI   WORK,TATITYCO                                                    
         MVC   WORK+1(1),8(R2)                                                  
         GOTO1 GETL,DMCB,(2,WORK)  GET CORRECT TAX ID ELEMENT                   
         BNE   INVERR                                                           
         MVC   NEWCODE(1),8(R2)                                                 
*                                                                               
GET30    CLC   NEWCODE,OLDCODE     ENSURE ACTUAL CHANGE                         
         BE    INVERR                                                           
*                                                                               
         OC    NXTKEY,NXTKEY       IF IN MIDDLE OF READING                      
         BZ    GETC40                                                           
         CLI   NXTKEY,TLECCCDQ     ECAST RECORDS                                
         BE    GETC50              CONTINUE WITH ECAST AGAIN                    
*                                                                               
GETC40   MVI   RECCD,TLCACCDQ      READ/(CONTINUE) CAST RECS BY SSN             
         BAS   RE,CHGCAST                                                       
GETC50   MVI   RECCD,TLECCCDQ      READ/(CONTINUE) ECAST RECS BY SSN            
         BAS   RE,CHGCAST                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              READ AND CHANGE CORP CODE ON CAST/ECAST RECORDS                  
*                                                                               
CHGCAST  NTR1                                                                   
         LA    R2,KEY                                                           
         USING TLDRD,R2                                                         
         MVC   KEY,NXTKEY          IN THE MIDDLE OF READING                     
         OC    KEY,KEY                                                          
         BNZ   CHGC35                                                           
         MVC   TLDRCD,RECCD        SET RECORD CODE                              
         CLI   RECCD,TLCACCDQ      SET PERFORMERS SSN                           
         BNE   *+14                                                             
         MVC   KEY+TLCACSSN-TLCAPCD(L'TGSSN),TGSSN                              
         B     *+10                                                             
         MVC   KEY+TLECCSSN-TLECPD(L'TGSSN),TGSSN                               
*                                                                               
CHGC35   GOTO1 HIGH                                                             
         B     CHGC50                                                           
*                                                                               
CHGC40   GOTO1 SEQ                                                              
*                                                                               
CHGC50   LA    RE,TLCACCOM-TLCAPD-1 LENGTH OF CAST KEY TO COMPARE               
         CLI   RECCD,TLCACCDQ                                                   
         BE    *+8                                                              
         LA    RE,TLECCEPI-TLECPD-1 LENGTH OF ECAST KEY TO COMPARE              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   CHGCX                                                            
         MVC   NXTKEY,KEY          SAVE FOR NEXT TIME IN                        
*                                                                               
**NO-OP**OI    GENSTAT1,CATCHIOR  RETURN TO USER                                
**NO-OP**GOTO1 CATCHIOS           IF TOO MANY IO'S                              
**NO-OP**CLI   ERROR,NOTONLIN     GIVE USER MSG                                 
**NO-OP**BNE   CHGC55                                                           
*                                                                               
         GOTO1 GETFACT,DMCB,0      SPLIT REQ INTO CHUNKS OF 2000 I/O            
         L     R1,DMCB             THIS FIXES I/O AND LOCKER PROBLEMS           
         USING FACTSD,R1                                                        
         LH    R3,=H'2000'                                                      
         CLM   R3,3,FATIOCNT                                                    
         BH    CHGC55                                                           
         MVI   MYMSGNO1,77         PUT MSG - HIT ENTER TO CONTINUE              
**NO-OP**MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SCCSSNH                                                       
         B     ERRXIT                                                           
         DROP  R1                                                               
*                                                                               
CHGC55   CLI   OLDCODE,C'A'        IF CHANGING ALL CORPS                        
         BNE   *+8                                                              
         MVI   RDUPDATE,C'Y'       OKAY TO READ FOR UPDATE NOW                  
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO              GET CAST DETAILS ELEMENT                     
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      FROM CAST RECORD                             
         BAS   RE,GETEL                                                         
         BE    *+6                 WHICH MUST EXIST                             
         DC    H'0'                                                             
*                                                                               
         CLI   OLDCODE,C'A'        CHANGE ALL CORPS                             
         BE    CHGC57                                                           
         CLC   TACACORP,OLDCODE    IF THIS IS THE CORP NUMBER WE WANT           
         BNE   CHGC40                 TO CHANGE                                 
         MVI   RDUPDATE,C'Y'       NOW RE-READ FOR UPDATE                       
         GOTO1 GETREC                                                           
*                                                                               
CHGC57   MVC   TACACORP,NEWCODE                                                 
         GOTO1 ACTVIN,DMCB,0                                                    
         GOTO1 PUTREC              CHANGE IT & REWRITE THE RECORD               
         B     CHGC40                                                           
*                                                                               
CHGCX    XC    NXTKEY,NXTKEY       CLEAR CONTINUE KEY                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
MISSERR  MVI   ERROR,MISSING       MISSING INPUT                                
         B     ERRXIT                                                           
*                                                                               
INVNUM   MVI   ERROR,NOTNUM        INVALID NUMBER                               
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT                                
         B     ERRXIT                                                           
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
         SPACE 3                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
*                                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'PAYEE   ',CL8'DISPLAY '                               
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CCOM    ',CL8'LIST    '                               
PF14     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF14X    EQU   *                                                                
*                                                                               
         DC    AL1(PF15X-*,15,0,0,0)                                            
         DC    CL3' ',CL8'YTD     ',CL8'DISPLAY '                               
PF15X    EQU   *                                                                
*                                                                               
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3' ',CL8'LIEN    ',CL8'LIST    '                               
PF16     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF16X    EQU   *                                                                
*                                                                               
         DC    AL1(PF17X-*,17,0,(PF17X-PF17)/KEYLNQ,0)                          
         DC    CL3' ',CL8'DUECOMP ',CL8'LIST    '                               
PF17     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF17X    EQU   *                                                                
*                                                                               
         DC    AL1(PF18X-*,18,0,0,0)                                            
         DC    CL3' ',CL8'GRT     ',CL8'LIST    '                               
PF18X    EQU   *                                                                
*                                                                               
         DC    AL1(PF19X-*,19,0,(PF19X-PF19)/KEYLNQ,0)                          
         DC    CL3' ',CL8'CHECK   ',CL8'LIST    '                               
PF19     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF19X    EQU   *                                                                
*                                                                               
         DC    AL1(PF23X-*,23,PFTINT,0,PFTRETRN)                                
         DC    CL3' ',CL8'        ',CL8'        '                               
PF23X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR2ED                                                       
         SPACE 4                                                                
*                                                                               
         ORG   SCCWORK                                                          
*                                                                               
RECCD    DS    CL1                 CURRECTN RECORD CODE                         
OLDCODE  DS    CL1                 OLD CORPORATION CODE                         
NEWCODE  DS    CL1                 NEW CORPORATION CODE                         
NXTKEY   DS    CL48                NEXT TIME IN READ THIS KEY                   
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
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065TAGEN2E   02/20/09'                                      
         END                                                                    
