*          DATA SET ACPRO2E    AT LEVEL 036 AS OF 09/12/02                      
*PHASE T60B2EA,*                                                                
         TITLE 'T60B2E - WORKCODE REPORT'                                       
T60B2E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B2E**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE FOR KEY FIELD VALIDATIONS                                         
*                                                                               
VKEY     NTR1                                                                   
         MVI   FILTWG,0                                                         
         LA    R2,PROWGRH                                                       
         XC    PROWGRN,PROWGRN                                                  
         OI    PROWGRNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         MVI   OPTION,C'Y'                                                      
         GOTO1 VALWG                                                            
         MVC   FILTWG,8(R2)                                                     
         GOTO1 ANYNAME                                                          
         SPACE 1                                                                
VKEY2    LA    R2,PROSORTH                                                      
         MVI   NAMEOPT,C'N'                                                     
         CLI   5(R2),0                                                          
         BE    VKEY3                                                            
         MVC   NAMEOPT,8(R2)                                                    
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'Y'                                                       
         BE    VKEY3                                                            
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
         SPACE 1                                                                
VKEY3    MVI   COM,0                                                            
         LA    R2,PROCOMH                                                       
         CLI   5(R2),0                                                          
         BE    VKEY4                                                            
         MVI   ERROR,INVALID                                                    
         MVC   COM,8(R2)                                                        
         CLI   8(R2),C'Y'                                                       
         BE    VKEY4                                                            
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
         SPACE 1                                                                
VKEY4    MVI   TYP,0                                                            
         LA    R2,PROTYPH                                                       
         CLI   5(R2),0                                                          
         BE    VKEY5                                                            
         MVI   ERROR,INVALID                                                    
         MVC   TYP,8(R2)                                                        
         CLI   8(R2),C'T'                                                       
         BE    VKEY5                                                            
         CLI   8(R2),C'P'                                                       
         BE    VKEY5                                                            
         CLI   8(R2),C'O'                                                       
         BE    VKEY5                                                            
         CLI   8(R2),C'M'                                                       
         BNE   ERREND                                                           
         SPACE 1                                                                
VKEY5    MVI   MED,0                                                            
         LA    R2,PROMEDH                                                       
         CLI   5(R2),0                                                          
         BE    VKEY6                                                            
         MVI   ERROR,INVALID                                                    
         MVC   MED,8(R2)                                                        
         CLI   8(R2),C'Y'                                                       
         BE    VKEY6                                                            
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
         SPACE 1                                                                
VKEY6    MVI   STA,0                                                            
         LA    R2,PROSTAH                                                       
         CLI   5(R2),0                                                          
         BE    VKEY7                                                            
         MVI   ERROR,INVALID                                                    
         MVC   STA,8(R2)                                                        
         CLI   8(R2),C'A'                                                       
         BE    VKEY7                                                            
         CLI   8(R2),C'I'                                                       
         BNE   ERREND                                                           
         SPACE 1                                                                
VKEY7    MVI   ADJ,0                                                            
         LA    R2,PROADJH                                                       
         CLI   5(R2),0                                                          
         BE    VKEYX                                                            
         MVI   ERROR,INVALID                                                    
         MVC   ADJ,8(R2)                                                        
         CLI   8(R2),C'Y'                                                       
         BE    VKEYX                                                            
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              HANDLE I/O FOR MEDIA RECORDS                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
*                                                                               
         GOTO1 TWAVSORT,DMCB,SORTFLD,RECTYPE,0                                  
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),CUL                                                     
         GOTO1 HIGH                                                             
         B     PREP4                                                            
         SPACE 1                                                                
PREP2    GOTO1 SEQ                                                              
         SPACE 1                                                                
PREP4    CLC   KEY(4),KEYSAVE      CHECK C/B                                    
         BNE   PREP16                                                           
*                                                                               
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACANALD,R6                                                       
         CLI   FILTWG,0                                                         
         BE    PREP6                                                            
         CLC   FILTWG,ACANGRUP     APPLY WORKCODE GROUP FILTER                  
         BNE   PREP2                                                            
*                                                                               
PREP6    CLI   COM,0               CHECK COMMISSION ?                           
         BE    PREP8               NO                                           
         TM    ACANSTAT,X'02'      YES, IS RECORD COMMISSIONABLE ?              
         BO    PREP7               NO                                           
         CLI   COM,C'Y'            YES, SKIP THOSE WE DON'T WANT                
         B     *+8                                                              
*                                                                               
PREP7    CLI   COM,C'N'                                                         
         BNE   PREP2                                                            
*                                                                               
PREP8    CLI   TYP,0               CHECK TYPE ?                                 
         BE    PREP9               NO                                           
         CLC   ACANTYPE,TYP        YES, IS DO THEY MATCH ?                      
         BE    PREP9               YES                                          
         CLI   TYP,C'O'            NO, DIO WE WANT OOP ?                        
         BNE   PREP2               NO, SKIP IT                                  
         CLI   ACANTYPE,X'40'      YES, IS TYPE ZERO OR BLANK ?                 
         BH    PREP2               NO                                           
*                                                                               
PREP9    CLI   MED,0               CHECK MEDIA ?                                
         BE    PREP11              NO                                           
         TM    ACANSTAT,X'01'      YES, IS RECORD MEDIA TRANSFER ?              
         BZ    PREP10              NO                                           
         CLI   MED,C'Y'            YES, SKIP THOSE WE DON'T WANT                
         B     *+8                                                              
*                                                                               
PREP10   CLI   MED,C'N'                                                         
         BNE   PREP2                                                            
*                                                                               
PREP11   CLI   STA,0               CHECK STATUS ?                               
         BE    PREP13              NO                                           
         TM    ACANSTA2,X'80'      YES, IS RECORD INACTIVE ?                    
         BZ    PREP12              NO                                           
         CLI   STA,C'I'            YES, SKIP THOSE WE DON'T WANT                
         B     *+8                                                              
*                                                                               
PREP12   CLI   STA,C'A'                                                         
         BNE   PREP2                                                            
*                                                                               
PREP13   CLI   ADJ,0               CHECK ADJUSTMENT RATE ?                      
         BE    PREP15              NO                                           
         TM    ACANSTA2,X'40'      YES, IS RECORD TO BE ADJUSTED ?              
         BZ    PREP14              NO                                           
         CLI   ADJ,C'Y'            YES, SKIP THOSE WE DON'T WANT                
         B     *+8                                                              
*                                                                               
PREP14   CLI   ADJ,C'N'                                                         
         BNE   PREP2                                                            
*                                                                               
PREP15   XC    SORTREC,SORTREC                                                  
         LA    R3,SORTREC                                                       
         USING SORTD,R3                                                         
         LA    RE,SORTRECL                                                      
         STH   RE,SORTRLEN                                                      
         MVC   SORTWORK,ACANCODE   WORK CODE                                    
         MVC   SORTCODE,=C'99'     FORCE TO SAME VALUE FOR NAME SORT            
         CLI   NAMEOPT,C'Y'        TEST FOR NAME OPTION                         
         BE    *+10                                                             
         MVC   SORTCODE,ACANCODE   NO-SORT ON REAL CODE                         
         MVC   SORTDESC,ACANDESC                                                
         MVC   SORTWGR,ACANGRUP                                                 
         MVI   SORTCOMM,C'Y'                                                    
         TM    ACANSTAT,X'02'      TEST IF NON-COMMISSIONABLE                   
         BZ    *+8                                                              
         MVI   SORTCOMM,C'N'                                                    
         MVC   SORTTYP,ACANTYPE                                                 
         CLI   SORTTYP,C' '        DO WE HAVE A TYPE ?                          
         BH    *+8                 YES                                          
         MVI   SORTTYP,C'O'        NO, FORCE OOP                                
         MVI   SORTMED,C'N'                                                     
         TM    ACANSTAT,X'01'                                                   
         BZ    *+8                                                              
         MVI   SORTMED,C'Y'                                                     
         MVI   SORTSTA,C'A'                                                     
         TM    ACANSTA2,X'80'                                                   
         BZ    *+8                                                              
         MVI   SORTSTA,C'I'                                                     
         MVI   SORTADJ,C'N'                                                     
         TM    ACANSTA2,X'40'                                                   
         BZ    *+8                                                              
         MVI   SORTADJ,C'Y'                                                     
*                                                                               
         MVC   SORTNAME,SPACES                                                  
         MVI   ELCODE,ACNMELQ      LOOK FOR LONG NAME ON RECORD                 
         BAS   RE,GETELIO                                                       
         BNE   PREP15P             NO LONG NAME                                 
*                                                                               
         USING ACNAMED,R6                                                       
         ZIC   R1,ACNMLEN          PUT LONG NAME TO SORT                        
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SORTNAME(0),ACNMNAME                                             
*                                                                               
PREP15P  GOTO1 TWAVSORT,DMCB,=C'PUT',SORTD                                      
         B     PREP2                                                            
         SPACE 1                                                                
PREP16   MVI   COL,0               SET COLUMN INDEX TO ZERO                     
*                                                                               
PREP17   GOTO1 TWAVSORT,DMCB,=C'GET'                                            
         ICM   R3,15,DMCB+4                                                     
         BZ    PREP19              EOF                                          
         USING SORTD,R3                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         LA    R2,WORK                                                          
         USING PRTD,R2                                                          
         MVC   PRTWORK1,SORTWORK                                                
         MVC   PRTDESC1,SORTDESC                                                
         MVC   PRTWGR1,SORTWGR                                                  
         MVC   PRTCOMM1,SORTCOMM                                                
         MVC   PRTTYP1,SORTTYP                                                  
         MVC   PRTMED1,SORTMED                                                  
         MVC   PRTSTA1,SORTSTA                                                  
         MVC   PRTADJ1,SORTADJ                                                  
*                                                                               
         ZIC   R4,COL                                                           
         LR    RE,R4                                                            
         LA    RF,PRTWORK2-PRTWORK1 DISPLACEMENT TO NEXT COLUMN                 
         MR    RE,RE                                                            
         LA    R2,P                                                             
         LA    R5,PRTWORK1-1(RF)   R5=A(PRINT POSITION)                         
         MVC   0(PRTCOLL,R5),WORK                                               
*                                                                               
         LA    R5,L'P(R5)          PRINT LONGNAME UNDERNEATH                    
         MVC   5(L'SORTNAME,R5),SORTNAME                                        
*                                                                               
         LA    R4,1(R4)                                                         
         LA    R5,2                TEST IF THIRD COLUMN                         
         CR    R4,R5                                                            
         BNH   PREP18                                                           
         MVI   ALLOWLIN,2          YES-ALLOW FOR SKIPPED LINE                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SR    R4,R4               RESET COLUMN INDEX FOR NEW LINE              
*                                                                               
PREP18   STC   R4,COL                                                           
         B     PREP17                                                           
*                                                                               
PREP19   CLI   COL,0                                                            
         BE    PREC20                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PREC20   GOTO1 TWAVSORT,DMCB,=C'END'                                            
*                                                                               
PREPX    B     XIT                                                              
         EJECT                                                                  
*              HEAD HOOK                                                        
         SPACE 3                                                                
HOOK     NTR1                                                                   
         CLI   FILTWG,0            TEST FOR WORK GROUP FILTER                   
         BE    HOOK2                                                            
         MVC   H4+1(10),=C'WORK GROUP'                                          
         MVC   H4+14(1),FILTWG                                                  
         MVC   H4+16(L'PROWGRN),PROWGRN                                         
         SPACE 1                                                                
HOOK2    L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         CLI   BOXOPT,C'N'                                                      
         BE    XIT                                                              
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         MVI   PRTLBOX-PRTD(R2),C'L'                                            
         MVI   PRTBOX1-PRTD(R2),C'C'                                            
         MVI   PRTBOX4-PRTD(R2),C'C'                                            
         MVI   PRTRBOX-PRTD(R2),C'R'                                            
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              SPECS FOR HEADINGS ETC                                           
         SPACE 3                                                                
MYSPECS  DS    0F                                                               
         SSPEC H1,2,CREATED        SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,46,C'WORK CODE LISTING'                                       
         SSPEC H2,46,C'-----------------'                                       
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
*                                                                               
         SSPEC H8,2,C'CD'                                                       
         SSPEC H8,5,C'DESCRIPTION'                                              
         SSPEC H8,21,C'WKG'                                                     
         SSPEC H8,25,C'COM'                                                     
         SSPEC H8,29,C'TYP'                                                     
         SSPEC H8,33,C'MED'                                                     
         SSPEC H8,37,C'STA'                                                     
         SSPEC H8,41,C'ADJ'                                                     
*                                                                               
         SSPEC H8,45,C'CD'                                                      
         SSPEC H8,48,C'DESCRIPTION'                                             
         SSPEC H8,64,C'WRG'                                                     
         SSPEC H8,68,C'COM'                                                     
         SSPEC H8,72,C'TYP'                                                     
         SSPEC H8,76,C'MED'                                                     
         SSPEC H8,80,C'STA'                                                     
         SSPEC H8,84,C'ADJ'                                                     
*                                                                               
         SSPEC H8,88,C'CD'                                                      
         SSPEC H8,91,C'DESCRIPTION'                                             
         SSPEC H8,107,C'WRG'                                                    
         SSPEC H8,111,C'COM'                                                    
         SSPEC H8,115,C'TYP'                                                    
         SSPEC H8,119,C'MED'                                                    
         SSPEC H8,123,C'STA'                                                    
         SSPEC H8,127,C'ADJ'                                                    
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 2                                                                
SORTFLD  DC    CL80'SORT FIELDS=(5,17,A),FORMAT=BI,WORK=1'                      
RECTYPE  DC    CL80'RECORD TYPE=V,LENGTH=80'                                    
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPRODED                                                       
         SPACE 2                                                                
FILTWG   DS    C                                                                
NAMEOPT  DS    C                                                                
STA      DS    C                                                                
TYP      DS    C                                                                
COM      DS    C                                                                
MED      DS    C                                                                
ADJ      DS    C                                                                
COL      DS    X                   COLUMN NUMBER                                
         DS    0D                                                               
SORTREC  DS    XL(SORTRECL)                                                     
         SPACE 2                                                                
* DSECT TO COVER SORT RECORD                                                    
*                                                                               
SORTD    DSECT                                                                  
SORTRLEN DS    H                                                                
         DS    H                                                                
SORTKEY  DS    0CL17                                                            
SORTCODE DS    CL2                 = WORKCODE OR = C'99' IF NAMEOPT=Y           
SORTDESC DS    CL15                WORKCODE NAME                                
SORTDATA DS    0CL4                                                             
SORTWORK DS    CL2                 WORKCODE                                     
SORTWGR  DS    C                   WORKCODE GROUP                               
SORTCOMM DS    C                   COMMISSIONABLE                               
SORTTYP  DS    C                   WORKCODE TYPE                                
SORTMED  DS    C                   MEDIA TRANSFER                               
SORTSTA  DS    C                   ACTIVE/INACTIVE                              
SORTADJ  DS    C                   RATE ADJUSTMENT INDICATOR                    
SORTNAME DS    CL36                LONG NAME                                    
SORTRECL EQU   *-SORTD                                                          
         SPACE 2                                                                
* DSECT TO COVER PRINT LINES                                                    
*                                                                               
PRTD     DSECT                                                                  
PRTLBOX  DS    C                                                                
PRTWORK1 DS    CL2                 WORK CODE 1                                  
         DS    CL1                                                              
PRTDESC1 DS    CL15                DESCRIPTION 1                                
         DS    CL2                                                              
PRTWGR1  DS    C                                                                
         DS    CL3                                                              
PRTCOMM1 DS    C                   COMMISSIONABLE 1                             
         DS    CL3                                                              
PRTTYP1  DS    C                                                                
         DS    CL3                                                              
PRTMED1  DS    C                                                                
         DS    CL3                                                              
PRTSTA1  DS    C                                                                
         DS    CL3                                                              
PRTADJ1  DS    C                                                                
PRTCOLL  EQU   *-PRTLBOX           LENGTH OF PRINT COLUMN                       
         DS    C                                                                
PRTBOX1  DS    C                                                                
PRTWORK2 DS    CL2                 WORK CODE 2                                  
         DS    CL1                                                              
PRTDESC2 DS    CL15                DESCRIPTION 2                                
         DS    CL2                                                              
PRTWGR2  DS    C                                                                
         DS    CL3                                                              
PRTCOMM2 DS    C                   COMMISSIONABLE 2                             
         DS    CL3                                                              
PRTTYP2  DS    C                                                                
         DS    CL3                                                              
PRTMED2  DS    C                                                                
         DS    CL3                                                              
PRTSTA2  DS    C                                                                
         DS    CL3                                                              
PRTADJ2  DS    C                                                                
         DS    C                                                                
PRTBOX4  DS    C                                                                
PRTWORK3 DS    CL2                 WORK CODE 3                                  
         DS    CL1                                                              
PRTDESC3 DS    CL15                DESCRIPTION 3                                
         DS    CL2                                                              
PRTWGR3  DS    C                   WORK GROUP 3                                 
         DS    CL3                                                              
PRTCOMM3 DS    C                   COMMISSIONABLE 3                             
         DS    CL3                                                              
PRTTYP3  DS    C                                                                
         DS    CL3                                                              
PRTMED3  DS    C                                                                
         DS    CL3                                                              
PRTSTA3  DS    C                                                                
         DS    CL1                                                              
PRTADJ3  DS    C                                                                
         DS    CL3                                                              
PRTRBOX  DS    C                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036ACPRO2E   09/12/02'                                      
         END                                                                    
