*          DATA SET CTREP5202  AT LEVEL 028 AS OF 05/01/02                      
*PHASE CT5202A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE SORTER                                                                 
         TITLE 'PROFILE REPORT'                                                 
         PRINT NOGEN                                                            
CT5202   CSECT                                                                  
         NMOD1 0,**PROF**,RR=R2                                                 
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         L     R8,=A(BUFFALOC)                                                  
         USING BUFFALOD,R8                                                      
         A     R8,RELO                                                          
         L     R9,=A(MYBUFFIO)                                                  
         A     R9,RELO                                                          
         USING MYRECD,R9                                                        
         CLI   MODE,RUNFRST                                                     
         BNE   PFA                                                              
         GOTO1 BUFFALO,DMCB,=C'SET',(R8)                                        
         B     XIT                                                              
         SPACE 2                                                                
PFA      CLI   MODE,REQFRST                                                     
         BNE   PFB                                                              
         GOTO1 BUFFALO,DMCB,=C'RESET',(R8)                                      
         MVC   PAGE,=H'1'                                                       
         L     R2,=A(SORTC)                                                     
         A     R2,RELO                                                          
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,(40,(R2)),RR=RB                 
         XC    LASTKEY,LASTKEY                                                  
         B     XIT                                                              
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=20'                                    
         EJECT                                                                  
*              PROCESS A RECORD                                                 
         SPACE 3                                                                
PFB      CLI   MODE,PROCPROF                                                    
         BNE   PF50                                                             
         L     R2,ADRECORD                                                      
         USING CTPREC,R2                                                        
         CLC   CTPKEY(22),LASTKEY                                               
         BE    PF1                                                              
         OC    LASTKEY,LASTKEY                                                  
         BZ    PF1                                                              
         BAS   RE,PRINTEM                                                       
         SPACE 2                                                                
PF1      BAS   RE,FILL                                                          
         MVC   LASTKEY,CTPKEY                                                   
         MVI   FORCEHED,C'Y'                                                    
         LR    R4,R2                                                            
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     PF4                                                              
         SPACE 2                                                                
PF2      MVI   ELCODE,0                                                         
         BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
PF4      BNE   XIT                                                              
         MVC   OVERRIDE,SAVEOVER                                                
         MVC   FIELD,SPACES                                                     
         MVC   VALUE,SPACES                                                     
         MVC   VALUE+100(50),VALUE                                              
         CLI   0(R4),X'56'                                                      
         BE    PF40                                                             
         CLI   0(R4),X'4F'                                                      
         BH    PF2                                                              
         CLI   0(R4),X'40'                                                      
         BL    PF2                                                              
         BAS   RE,PROTYPE                                                       
         CLI   0(R4),X'40'                                                      
         BE    PF10                                                             
         CLI   0(R4),X'41'                                                      
         BE    PF11                                                             
         CLI   0(R4),X'42'                                                      
         BE    PF12                                                             
         CLI   0(R4),X'44'                                                      
         BE    PF14                                                             
         CLI   0(R4),X'45'                                                      
         BE    PF15                                                             
         CLI   0(R4),X'46'                                                      
         BE    PF16                                                             
         CLI   0(R4),X'48'                                                      
         BE    PF20                                                             
         CLI   0(R4),X'4A'                                                      
         BE    PF22                                                             
         CLI   0(R4),X'4C'                                                      
         BE    PF24                                                             
         CLI   0(R4),X'FD'                                                      
         BE    PF30                                                             
         CLI   0(R4),X'4E'                                                      
         BE    PF34                                                             
         B     PF2                                                              
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
ELCODE   DC    X'00'                                                            
         DC    X'00'                                                            
         EJECT                                                                  
*              ROUTINES FOR SPECIFIC ELEMENTS                                   
         SPACE 3                                                                
         USING CTDCOD,R4                                                        
PF10     MVC   VALUE(10),CTDCODE                                                
         MVC   FIELD(11),=C'DESTINATION'                                        
         MVI   FNO,4                                                            
         B     PFEND                                                            
         SPACE 2                                                                
         USING CTACOD,R4                                                        
PF11     MVC   VALUE(3),CTACODE                                                 
         MVC   FIELD(14),=C'ATTENTION TYPE'                                     
         MVI   FNO,44                                                           
         B     PFEND                                                            
         USING CTOCOD,R4                                                        
PF12     MVC   VALUE(10),CTOCODE                                                
         MVC   FIELD(11),=C'OUTPUT TYPE'                                        
         MVI   FNO,8                                                            
         B     PFEND                                                            
         SPACE 2                                                                
         USING CTPRID,R4                                                        
PF14     MVC   VALUE(2),CTPRITY                                                 
         MVC   FIELD(8),=C'PRIORITY'                                            
         MVI   FNO,12                                                           
         B     PFEND                                                            
         SPACE 2                                                                
         USING CTRCLD,R4                                                        
PF15     MVC   VALUE(1),CTRCLASS                                                
         MVC   FIELD(12),=C'READER CLASS'                                       
         MVI   FNO,16                                                           
         B     PFEND                                                            
         SPACE 2                                                                
         USING CTSRTD,R4                                                        
PF16     SR    R3,R3                                                            
         IC    R3,CTSRTLEN                                                      
         SH    R3,=H'6'                                                         
         SRL   R3,1                                                             
         LA    R5,CTSRTFRM                                                      
         LA    R7,VALUE                                                         
         MVC   FIELD(12),=C'SORT FORMULA'                                       
         MVI   FNO,20                                                           
         SPACE 2                                                                
PF18     SR    R6,R6                                                            
         IC    R6,0(R5)                                                         
         SLL   R6,25                                                            
         SRL   R6,25                                                            
         EDIT  (R6),(2,0(R7)),ALIGN=LEFT                                        
         AR    R7,R0                                                            
         MVI   0(R7),C'/'                                                       
         EDIT  (1,1(R5)),(2,1(R7)),ALIGN=LEFT                                   
         LA    R7,1(R7)                                                         
         AR    R7,R0                                                            
         MVI   0(R7),C'/'                                                       
         MVI   1(R7),C'A'                                                       
         TM    0(R5),X'80'                                                      
         BNO   *+8                                                              
         MVI   1(R7),C'D'                                                       
         LA    R7,3(R7)                                                         
         LA    R5,2(R5)                                                         
         BCT   R3,PF18                                                          
         B     PFEND                                                            
         SPACE 2                                                                
PF20     MVC   FIELD(23),=C'PROCESSING INSTRUCTIONS'                            
         MVI   FNO,24                                                           
         B     PF26                                                             
         SPACE 2                                                                
PF22     MVC   FIELD(23),=C'BREAK-DOWN INSTRUCTIONS'                            
         MVI   FNO,28                                                           
         B     PF26                                                             
         SPACE 2                                                                
PF24     MVC   FIELD(21),=C'SHIPPING INSTRUCTIONS'                              
         MVI   FNO,32                                                           
         SPACE 2                                                                
         USING CTPRCD,R4                                                        
PF26     SR    R3,R3                                                            
         IC    R3,CTPRCLEN                                                      
         SH    R3,=H'7'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   VALUE(0),CTPRCINS                                                
         B     PFEND                                                            
         SPACE 2                                                                
         USING CTJCLD,R4                                                        
PF30     MVC   VALUE(10),CTJCLEX                                                
         MVC   FIELD(8),=C'JCL BOOK'                                            
         MVI   FNO,36                                                           
         B     PFEND                                                            
         SPACE 2                                                                
         USING CTPHSD,R4                                                        
PF34     MVC   FIELD(11),=C'TEST PHASES'                                        
         MVI   FNO,40                                                           
         LA    R5,CTPHS01                                                       
         LA    R6,VALUE                                                         
         LA    R7,1                                                             
         SPACE 2                                                                
PF36     CLI   0(R5),C' '                                                       
         BE    PF38                                                             
         CLI   0(R5),0                                                          
         BE    PF38                                                             
         MVC   0(4,R6),=C'ON=X'                                                 
         STC   R7,0(R6)                                                         
         OI    0(R6),X'F0'                                                      
         MVC   3(1,R6),0(R5)                                                    
         LA    R6,5(R6)                                                         
         SPACE 2                                                                
PF38     LA    R5,1(R5)                                                         
         CH    R7,=H'4'                                                         
         BE    PFEND                                                            
         LA    R7,1(R7)                                                         
         B     PF36                                                             
         SPACE 2                                                                
         USING CTTIMED,R4                                                       
PF40     EDIT  (4,CTTIME),(10,VALUE),2,ALIGN=LEFT                               
         MVC   FIELD(20),=C'AVERAGE TIME/REQUEST'                               
         MVI   FNO,0                                                            
         SPACE 2                                                                
PFEND    GOTO1 BUFFALO,DMCB,=C'PUT',(R8),(R9)                                   
         MVC   SORTTYPE,FNO                                                     
         MVC   SORTVAL,VALUE                                                    
         MVC   SORTPRG(3),CTPKSYS                                               
         MVC   SORTOVER,OVERRIDE                                                
         CLI   FNO,44                                                           
         BE    PFEND2                                                           
         CLI   FNO,8                                                            
         BE    PFEND2                                                           
         CLI   FNO,12                                                           
         BNE   PFEND4                                                           
         SPACE 2                                                                
PFEND2   GOTO1 =V(SORTER),DMCB,=C'PUT',SORTTYPE,RR=RB                           
         SPACE 2                                                                
PFEND4   MVC   OVERRIDE,SPACES                                                  
         MVC   TYPE,SPACES                                                      
         MVC   VALUE,SPACES                                                     
         MVC   VALUE+100(50),SPACES                                             
         MVI   OVERRIDE,0                                                       
         GOTO1 BUFFALO,DMCB,=C'PUT',(R8),(R9)                                   
         MVI   OVERRIDE,X'FF'                                                   
         BASR  RE,RF                                                            
         B     PF2                                                              
         SPACE 2                                                                
PF50     CLI   MODE,REQLAST                                                     
         BNE   PF52                                                             
         BAS   RE,PRINTEM                                                       
         BAS   RE,XREF                                                          
         B     XIT                                                              
         SPACE 1                                                                
PF52     CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
*&&OS*&& GOTO1 =V(SORTER),DMCB,=C'END',RR=RB                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT PROFILE TYPE                                   
         SPACE 2                                                                
PROTYPE  NTR1                                                                   
         USING CTDCOD,R4                                                        
         MVC   TYPE(14),SPACES                                                  
         MVC   TYPE(6),=C'SACRED'                                               
         CLI   CTDCOTYP,C'S'                                                    
         BE    XIT                                                              
         MVC   TYPE(9),=C'PERMANENT'                                            
         CLI   CTDCOTYP,C'P'                                                    
         BE    XIT                                                              
         MVC   TYPE(9),=CL9'EVERY XXX'                                          
         MVC   TYPE+6(3),CTDCODTA                                               
         CLI   CTDCOTYP,C'D'                                                    
         BE    XIT                                                              
         MVC   TYPE(9),=CL9'UNKNOWN'                                            
         CLI   CTDCOTYP,C'T'                                                    
         BNE   XIT                                                              
         MVC   TYPE(9),=CL9'UNTIL'                                              
         GOTO1 DATCON,DMCB,(1,CTDCODTA),(8,TYPE+6)                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FILL HEADINGS AND OVERRIDE                            
         SPACE 3                                                                
FILL     NTR1                                                                   
         LA    R4,SYSTAB                                                        
         USING CTPREC,R2                                                        
         OC    CTPKORIG,CTPKORIG   PICK UP NAME FROM DEFAULT                    
         BNZ   PR6                                                              
         SPACE 2                                                                
PR2      CLC   0(1,R4),CTPKSYS                                                  
         BE    PR4                                                              
         CLI   0(R4),C' '                                                       
         BE    PR4                                                              
         LA    R4,10(R4)                                                        
         B     PR2                                                              
         SPACE 2                                                                
PR4      MVC   HEAD4+10(9),1(R4)                                                
         MVC   HEAD5+10(2),1(R4)                                                
         MVC   HEAD5+12(2),CTPKPROG                                             
         MVI   HEAD5+14,C'-'                                                    
         MVC   HEAD5+15(36),SPACES                                              
         LR    R4,R2                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR6                                                              
         USING CTDSCD,R4                                                        
         SR    R3,R3                                                            
         IC    R3,CTDSCLEN                                                      
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   HEAD5+15(0),CTDSC                                                
         SPACE 2                                                                
PR6      MVC   OVERRIDE,SPACES                                                  
         OC    CTPKORIG,CTPKORIG                                                
         BZ    PR10                                                             
         EDIT  (2,CTPKORIG),(10,OVERRIDE),ALIGN=LEFT                            
         MVC   MYKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),CTPKORIG                                               
         BAS   RE,READ                                                          
         MVI   ELCODE,X'02'                                                     
         LR    R4,R2                                                            
         BAS   RE,GETEL                                                         
         USING CTDSCD,R4                                                        
         BNE   *+10                                                             
         MVC   OVERRIDE(10),CTDSC                                               
         MVC   KEY,MYKEY                                                        
         BAS   RE,READ                                                          
         SPACE 2                                                                
PR10     MVC   SAVESYS,HEAD4+10                                                 
         MVC   SAVEPROG,HEAD5+10                                                
         MVC   SAVEOVER,OVERRIDE                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO PRINT XREF LISTINGS                                  
         SPACE 3                                                                
XREF     NTR1                                                                   
         MVC   P,SPACES                                                         
         XC    LASTSORT,LASTSORT                                                
         SPACE 2                                                                
XR2      GOTO1 =V(SORTER),DMCB,=C'GET',RR=RB                                    
         L     R2,DMCB+4                                                        
         LTR   R2,R2               EOF                                          
         BNZ   XR4                                                              
         BAS   RE,XRPRINT                                                       
         MVI   RCSUBPRG,1                                                       
         B     XIT                                                              
         SPACE 2                                                                
XR4      MVC   SORTTYPE(20),0(R2)                                               
         CLC   SORTTYPE,LASTSORT                                                
         BE    XR6                                                              
         BAS   RE,XRPRINT                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         CLI   SORTTYPE,44                                                      
         BE    XR6                                                              
         MVI   RCSUBPRG,3                                                       
         CLI   SORTTYPE,8                                                       
         BE    XR6                                                              
         MVI   RCSUBPRG,4                                                       
         SPACE 2                                                                
XR6      CLC   SORTTYPE(7),LASTSORT                                             
         BE    XR8                                                              
         BAS   RE,XRPRINT                                                       
         GOTO1 REPORT                                                           
         MVC   P+1(6),SORTVAL                                                   
         SPACE 2                                                                
XR8      BAS   RE,XRPOST                                                        
         MVC   LASTSORT,SORTTYPE                                                
         B     XR2                                                              
         EJECT                                                                  
*              ROUTINE TO POST INTO PRINT LINE AND PRINT                        
         SPACE 2                                                                
XRPOST   NTR1                                                                   
         CLC   P+96(14),SPACES                                                  
         BE    XR10                                                             
         BAS   RE,XRPRINT                                                       
         SPACE 2                                                                
XR10     LA    R2,P+10                                                          
         SPACE 2                                                                
XR12     CLC   0(20,R2),SPACES                                                  
         BE    XR14                                                             
         LA    R2,1(R2)                                                         
         B     XR12                                                             
         SPACE 2                                                                
XR14     LA    R2,1(R2)                                                         
         MVC   0(3,R2),SORTPRG                                                  
         CLC   SORTOVER,SPACES                                                  
         BE    XIT                                                              
         MVI   3(R2),C'-'                                                       
         MVC   4(10,R2),SORTOVER                                                
         B     XIT                                                              
         SPACE 2                                                                
XRPRINT  NTR1                                                                   
         CLC   P,SPACES                                                         
         BE    XIT                                                              
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL PRINTING                                      
         SPACE 3                                                                
PRINTEM  NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         XC    0(200,R9),0(R9)                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R8),(R9),0                                
         B     PR22                                                             
         SPACE 2                                                                
PR20     GOTO1 BUFFALO,DMCB,=C'SEQ',(R8),(R9),0                                 
         SPACE 2                                                                
PR22     CLI   DMCB+8,0                                                         
         BE    PR24                                                             
         GOTO1 BUFFALO,DMCB,=C'RESET',(R8)                                      
         B     XIT                                                              
         SPACE 2                                                                
PR24     CLI   OVERRIDE,0                                                       
         BNE   PR26                                                             
         MVC   P,SPACES                                                         
         MVC   P+1(23),FIELD                                                    
         B     PR20                                                             
         SPACE 2                                                                
PR26     CLI   OVERRIDE,X'FF'                                                   
         BE    PR28                                                             
         MVC   P+27(10),OVERRIDE                                                
         CLC   OVERRIDE,SPACES                                                  
         BNE   *+10                                                             
         MVC   P+27(7),=C'DEFAULT'                                              
         MVC   P+39(14),TYPE                                                    
         GOTO1 CHOPPER,DMCB,(150,VALUE),(50,P+55),(C'P',3)                      
         SPACE 2                                                                
PR28     MVC   HEAD4+10(9),SAVESYS                                              
         MVC   HEAD5+10(50),SAVEPROG                                            
         GOTO1 REPORT                                                           
         B     PR20                                                             
         SPACE 2                                                                
         SPACE 2                                                                
AREA     DC    300C' '                                                          
SYSTAB   DC    CL10'SSPOTPAK'                                                   
         DC    CL10'PPRINTPAK'                                                  
         DC    CL10'RREPPAK'                                                    
         DC    CL10'AACCPAK'                                                    
         DC    CL10'MMEDLINE'                                                   
         DC    CL10'CCONTROL'                                                   
         DC    CL10'DCPP'                                                       
         DC    CL10' UNKNOWN'                                                   
MYKEY    DS    CL25                                                             
LASTKEY  DC    XL25'00'                                                         
SAVESYS  DC    CL9' '                                                           
SAVEPROG DC    CL50' '                                                          
SAVEOVER DC    CL10' '                                                          
         SPACE 2                                                                
LASTSORT DS    CL20                                                             
SORTTYPE DS    CL1                 20 BYTE SORT RECORDS                         
SORTVAL  DS    CL6                                                              
SORTPRG  DS    CL3                                                              
SORTOVER DS    CL10                                                             
         DS    CL21                                                             
         EJECT                                                                  
*              DATA MANAGER AIDS                                                
READ     NTR1                                                                   
         SPACE 3                                                                
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,KEY,(R2),(0,DMWORK)                   
         XIT1                                                                   
RELO     DS    A                                                                
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTREPWORKD                                                     
         PRINT ON                                                               
         BUFF  LINES=100,ROWS=0,COLUMNS=0,FLAVOR=DATA,KEYLIST=(200,A)           
         SPACE 3                                                                
*              DSECT TO COVER BUFFALO RECORDS                                   
         SPACE 2                                                                
MYRECD   DSECT                                                                  
FNO      DS    CL1                                                              
FIELD    DS    CL23                                                             
OVERRIDE DS    CL10                                                             
TYPE     DS    CL14                                                             
VALUE    DS    CL150                                                            
         DS    CL2                                                              
         SPACE 2                                                                
MYBUFFIO CSECT                                                                  
         DS    CL200                                                            
         SPACE 2                                                                
SORTC    CSECT                                                                  
         DS    41000C                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028CTREP5202 05/01/02'                                      
         END                                                                    
